#include "bind_power.h"
#include "lexer.h"
#include "types.h"
#include "util/dynamic_array.h"
#include <stdlib.h>

/** declarations */
char          _parse_hex (const char buf[2]);
char          _parse_char (const parser_t *this, lex_token_t tok);
ast_node_t    _get_lvalue (parser_t *this, lex_token_pos *posp, bool name_only);
size_t        _get_name (parser_t *this);

ast_node_pos  _parse_args (parser_t *this);
ast_node_pos  _parse_block (parser_t *this);
ast_node_pos  _parse_cond (parser_t *this);
ast_node_pos  _parse_decl (parser_t *this, enum lex_type to_match);
ast_node_pos  _parse_expr (parser_t *this, uint8_t thrs);
ast_node_pos  _parse_func (parser_t *this);
ast_node_pos  _parse_stmt (parser_t *this);
ast_node_pos  _parse_var (parser_t *this, enum lex_type to_match);

#define PEEK_TOKEN(prs, tok, pos) ({                     \
		(pos) = _parser_peek_token((prs));       \
		(tok) = parser_get_token((prs), (pos)); })
#define NEXT_TOKEN(prs, tok, pos) ({                     \
		(pos) = _parser_next_token((prs));       \
		(tok) = parser_get_token((prs), (pos)); })
#define POP_TOKEN(prs, tok, pos) ({                      \
		(pos) = _parser_pop_token((prs));        \
		(tok) = parser_get_token((prs), (pos)); })
#define AST_APPEND(prs, node) ({                            \
		arr_push((prs)->ast, (node));               \
		(ast_node_pos){ arr_ulen((prs)->ast) - 1}; })

/** definitions */

/** aux functions */
/* {{{ */
inline char
_parse_hex (const char buf[2])
{
	return (('0' <= buf[0] && buf[0] <= '9' ? buf[0] - '0' : 9 + buf[0] - 'a') << 4)
		+ ('0' <= buf[1] && buf[1] <= '9' ? buf[1] - '0' : 9 + buf[1] - 'a');
}

char
_parse_char (const parser_t *this, lex_token_t tok)
{
	slice_char_t sl = parser_get_value_by_tok(this, tok);
	char buf[2];

	switch (sl.length) {
	case 2: return sl.ptr[1];
	case 3: switch(sl.ptr[2]) {
		case 'n': return '\n';
		case 't': return '\t';
		case 'r': return '\r';
		case '0': return '\0';
		case '\\': return '\\';
		case '\'': return '\'';
		case '"': return '"';
		default: _assert(false, "Invalid character passed scan");
		}
	case 5:
		buf[0] = sl.ptr[3]; buf[1] = sl.ptr[4];
		return _parse_hex(buf);
	default: _assert(false, "Invalid character passed scan");
	}
}

ast_node_t
_get_lvalue (parser_t *this, lex_token_pos *posp, bool name_only)
{/* {{{ */
	ast_node_t node;
	lex_token_t tok;
	lex_token_pos pos;

	if (!posp) NEXT_TOKEN(this, tok, pos);
	else pos = *posp, tok = parser_get_token(this, pos);
	node = SWITCH(tok.type, ast_node_t, {
	case DANA_NAME:
		slice_char_t sl = parser_get_value_by_tok(this, tok);
		size_t hash = hm_hash_bytes(sl.ptr, sl.length);
		hm_put(this->names, hash, pos);
		return (ast_node_t){ .type = AST_NAME, .pl_data.name = hash };
	case DANA_STRING:
		_assert(!name_only, "String literal not allowed for specific lvalue");
		return (ast_node_t){
			.type = AST_STRING,
			.pl_data.str = parser_append_text(this, tok),
		};
	default:
		_assert(false, "Wrong token at start of lvalue");
		return (ast_node_t){};
	});

	// dbg(ast_get_type_str(node), "%s");/* {{{ */
	// {
		// slice_char_t sl = parser_get_value_by_pos(this, pos);
		// LEX_TEMP_SLICE(sl);
		// dbg(sl.ptr, "%s");
	// }
	// {
		// slice_char_t sl = parser_get_value_by_tok(this, tok);
		// LEX_TEMP_SLICE(sl);
		// dbg(sl.ptr, "%s");
		// dbg(node.pl_data.name, "%lu");
		// dbg(hm_get(this->names, node.pl_data.name).pos, "%u");
	// }/* }}} */

	if (!name_only)
		while (PEEK_TOKEN(this, tok, pos).type == DANA_OPEN_BRACKET) {
			POP_TOKEN(this, tok, pos);
			node = (ast_node_t){
				.type = AST_ARR_AT,
				.op_data.lhs = AST_APPEND(this, node),
				.op_data.rhs = _parse_expr(this, 0),
			};
			_assert(NEXT_TOKEN(this, tok, pos).type == DANA_CLOSE_BRACKET,
					"Mismatching array-at brackets");
		}

	return node;
}/* }}} */

inline size_t
_get_name (parser_t *this)
{ return _get_lvalue(this, NULL, true).pl_data.name; }
/* }}} */

ast_node_pos
_parse_args (parser_t *this)
{/* {{{ */
	lex_token_t __attribute__((unused)) tok;
	lex_token_pos pos;
	ast_node_pos *tmp = {0};

	if (PEEK_TOKEN(this, tok, pos).type == DANA_CLOSE_PAREN)
		return (ast_node_pos){};

	arr_push(tmp, _parse_expr(this, 0));
	while (PEEK_TOKEN(this, tok, pos).type == DANA_COMMA) {
		POP_TOKEN(this, tok, pos);
		arr_push(tmp, _parse_expr(this, 0));
	}

	return AST_APPEND(this, ((ast_node_t){
			.type = AST_ARGS,
			.extra_data = parser_append_extras(this, tmp),
			}));
}/* }}} */

ast_node_pos
_parse_block (parser_t *this)
{/* {{{ */
	ast_node_t node;
	lex_token_t __attribute__((unused)) tok;
	lex_token_pos pos;
	ast_node_pos *tmp = {0};

	printf("parsing block...\n");
	switch (PEEK_TOKEN(this, tok, pos).type) {
	// TODO: handle indentation
	case DANA_KW_BEGIN:
		POP_TOKEN(this, tok, pos);
		node = (ast_node_t){ .type = AST_BLOCK };
		break;
	default:
		node = (ast_node_t){
			.type = AST_BLOCK_SIMPLE,
			.name_data.body = _parse_stmt(this),
		};
		printf("--- END, simple block\n");
		return AST_APPEND(this, node);
	}
	
	while (true) switch (PEEK_TOKEN(this, tok, pos).type) {
	case DANA_EOF:
		_assert(false, "Reached EOF before end of block");
	// TODO: similar
	case DANA_KW_END:
		POP_TOKEN(this, tok, pos);
		node.extra_data = parser_append_extras(this, tmp);
		printf("--- END, block\n");
		return AST_APPEND(this, node);
	default:
		arr_push(tmp, _parse_stmt(this));
	}

	_assert(false, "Reached end of parse_block. shouldn't happen");
}/* }}} */

ast_node_pos
_parse_cond (parser_t *this)
{/* {{{ */
	ast_node_t node;
	lex_token_t tok;
	lex_token_pos pos;
	ast_node_pos *tmp = {0};
	bool is_simple = true;

	printf("\tparsing cond...\n");
	_assert(NEXT_TOKEN(this, tok, pos).type == DANA_KW_IF,
			"Expected if, found [%s] |%.*s|",
			lex_get_type_str(tok),
			UNSLICE(parser_get_value_by_tok(this, tok)));
	printf("\t---\tIF:\n");
	arr_push(tmp, _parse_expr(this, 0));

	_assert(NEXT_TOKEN(this, tok, pos).type == DANA_COLON,
			"Expected colon, found [%s] |%.*s|",
			lex_get_type_str(tok),
			UNSLICE(parser_get_value_by_tok(this, tok)));
	printf("\t---\tIF-BLOCK:\n");
	arr_push(tmp, _parse_block(this));

	while (PEEK_TOKEN(this, tok, pos).type == DANA_KW_ELIF) {
		POP_TOKEN(this, tok, pos);
		printf("\t---\tELIF:\n");
		is_simple = false;
		arr_push(tmp, _parse_expr(this, 0));
		_assert(NEXT_TOKEN(this, tok, pos).type == DANA_COLON,
				"Expected colon, found [%s] |%.*s|",
				lex_get_type_str(tok),
				UNSLICE(parser_get_value_by_tok(this, tok)));
		printf("\t---\tELIF-BLOCK:\n");
		arr_push(tmp, _parse_block(this));
	}

	if (PEEK_TOKEN(this, tok, pos).type == DANA_KW_ELSE) {
		POP_TOKEN(this, tok, pos);
		is_simple = false;
		_assert(NEXT_TOKEN(this, tok, pos).type == DANA_COLON,
				"Expected colon, found [%s] |%.*s|",
				lex_get_type_str(tok),
				UNSLICE(parser_get_value_by_tok(this, tok)));
		printf("\t---\tELSE-BLOCK:\n");
		arr_push(tmp, _parse_block(this));
	}

	if (is_simple) {
		node = (ast_node_t){
			.type = AST_COND_SIMPLE, .op_data = { tmp[0], tmp[1] },
		};
		arr_free(tmp);
	} else {
		node = (ast_node_t){
			.type = AST_COND,
			.extra_data = parser_append_extras(this, tmp),
		};
	}
	printf("\t---END cond\n");
	return AST_APPEND(this, node);
}/* }}} */

ast_node_pos
_parse_decl (parser_t *this, enum lex_type to_match)
/* if to_match == 0, run decl, don't consume keyword */
{/* {{{ */
	ast_node_t node;
	ast_node_pos decl;
	lex_token_t __attribute__((unused)) tok;
	lex_token_pos pos;

	if (to_match)   _assert(NEXT_TOKEN(this, tok, pos).type == to_match,
			"Func-decl did not begin with expected keyword");
	if (to_match == DANA_KW_DEF) {
		decl = _parse_decl(this, 0);
		node = (ast_node_t){
			.type = AST_DEF_PROC +
				(parser_get_node(this, decl).type - AST_DECL_PROC),
			.op_data = { decl, _parse_func(this) },
		};
		return AST_APPEND(this, node);
	}
	/* match decl, keyword consumed */
	node = (ast_node_t){
		.type = AST_DECL_PROC, .name_data.name = _get_name(this),
	};
	if (PEEK_TOKEN(this, tok, pos).type == DANA_KW_IS) {
		POP_TOKEN(this, tok, pos);
		node.type = SWITCH(NEXT_TOKEN(this, tok, pos).type, enum ast_type, {
		case DANA_KW_INT : return AST_DECL_INT;
		case DANA_KW_BYTE: return AST_DECL_BYTE;
		default          : _assert(false, "Invalid return type");
		});
	}
	if (PEEK_TOKEN(this, tok, pos).type == DANA_COLON) {
		POP_TOKEN(this, tok, pos);
		while (true) {
			node.name_data.body = _parse_var(this, 0);
			if (PEEK_TOKEN(this, tok, pos).type == DANA_COMMA) {
				POP_TOKEN(this, tok, pos);
				continue;
			}
			break;
		}
	}
	return AST_APPEND(this, node);
}/* }}} */

ast_node_pos
_parse_expr (parser_t *this, uint8_t thrs)
{/* {{{ */
	ast_node_t node;
	ast_node_pos lhs;
	lex_token_t tok;
	lex_token_pos pos;
	parser_bp_t bp;

	node = SWITCH(NEXT_TOKEN(this, tok, pos).type, ast_node_t, {
	case DANA_KW_FALSE:
	case DANA_KW_TRUE:
		return (ast_node_t){
			.type = AST_BOOL,
			.pl_data.num = tok.type == DANA_KW_TRUE,
		};
	case DANA_NUMBER: {
		slice_char_t sl = parser_get_value_by_tok(this, tok);
		LEX_TEMP_SLICE(sl);
		return (ast_node_t){
			.type = AST_NUMBER, .pl_data.num = atoi(sl.ptr),
		};
	}
	case DANA_CHAR:
		return (ast_node_t){
			.type = AST_CHAR,
			.pl_data.ch = _parse_char(this, tok),
		};
	case DANA_STRING:
	case DANA_NAME:
		return _get_lvalue(this, &pos, false);
	default:
		_assert(!bp_is_prefix(tok, &bp),
				"No valid token found in start of expr");
		lhs = _parse_expr(this, bp.rhs);
		_assert(tok.type != DANA_OPEN_PAREN ||
			NEXT_TOKEN(this, tok, pos).type == DANA_CLOSE_PAREN,
					"Mismatching grouping parentheses");
		return (ast_node_t){ .type = tok.type, .op_data.lhs = lhs };
	});

	while(true) {
		PEEK_TOKEN(this, tok, pos);

		if (bp_is_postfix(tok, &bp)) {
			if (bp.lhs < thrs) break;
			POP_TOKEN(this, tok, pos);
			if (tok.type == DANA_OPEN_PAREN) {
				_assert(node.type == AST_NAME,
						"func-call must be after identifier");
				node = (ast_node_t){
					.type = AST_FUNC,
					.name_data.name = node.pl_data.name,
					.name_data.body = _parse_args(this),
				};
				_assert(NEXT_TOKEN(this, tok, pos).type == DANA_CLOSE_PAREN,
						"Mismatching func-call parentheses");
			} else {
				node = (ast_node_t){
					.type = tok.type,
					.op_data.lhs = AST_APPEND(this, node),
				};
			}
			continue;
		}

		if (bp_is_infix(tok, &bp)) {
			if (bp.lhs < thrs) break;
			POP_TOKEN(this, tok, pos);
			node = (ast_node_t){
				.type = tok.type,
				.op_data.lhs = AST_APPEND(this, node),
				.op_data.rhs = _parse_expr(this, bp.rhs),
			};
			continue;
		}

		break;
	}

	return AST_APPEND(this, node);
}/* }}} */

ast_node_pos
_parse_func (parser_t *this)
{/* {{{ */
	lex_token_t tok;
	lex_token_pos pos;
	ast_node_pos *tmp = {0};

	while (true) {
		switch (PEEK_TOKEN(this, tok, pos).type) {
		case DANA_KW_DEF:
		case DANA_KW_DECL:
			arr_push(tmp, _parse_decl(this, tok.type));
			continue;
		case DANA_KW_VAR:
			arr_push(tmp, _parse_var(this, DANA_KW_VAR));
			continue;
		default:
			arr_push(tmp, _parse_block(this));
		};
		break;
	}

	return AST_APPEND(this, ((ast_node_t){
		.type = AST_LOCAL_DEF,
		.extra_data = parser_append_extras(this, tmp),
	}));
}/* }}} */

ast_node_pos
_parse_stmt (parser_t *this)
{/* {{{ */
	ast_node_t node;
	lex_token_t tok;
	lex_token_pos pos;

	// PEEK_TOKEN(this, tok, pos);
	// printf("---\tParse stmt: peek: %s [%.*s]\n",
			// lex_get_type_str(tok),
			// UNSLICE(parser_get_value_by_tok(this, tok)));

	if (PEEK_TOKEN(this, tok, pos).type == DANA_KW_IF)
		return _parse_cond(this);
	node = SWITCH(NEXT_TOKEN(this, tok, pos).type, ast_node_t, {
	case DANA_KW_SKIP:
	case DANA_KW_EXIT:
		return (ast_node_t){ .type = tok.type };
	case DANA_KW_RETURN:
		_assert(NEXT_TOKEN(this, tok, pos).type == DANA_COLON,
				"Missing colon after return keyword");
		return (ast_node_t){
			.type = AST_RETURN,
			.op_data.lhs = _parse_expr(this, 0),
		};
	case DANA_KW_BREAK:
	case DANA_KW_CONT:
		uint8_t type = tok.type;
		if (PEEK_TOKEN(this, tok, pos).type != DANA_COLON)
			return (ast_node_t){ type };
		POP_TOKEN(this, tok, pos);
		return (ast_node_t){
			.type = type, .name_data.name = _get_name(this),
		};
	case DANA_KW_LOOP:
		size_t name = 0;
		if (PEEK_TOKEN(this, tok, pos).type != DANA_COLON)
			name = _get_name(this);
		_assert(NEXT_TOKEN(this, tok, pos).type == DANA_COLON,
				"Could not find colon in loop definition");
		return (ast_node_t){
			.type = AST_LOOP,
			.name_data = { name, _parse_block(this) },
		};
	case DANA_NAME:
		node = _get_lvalue(this, &pos, false);
		ast_node_pos args = {0};
		if (PEEK_TOKEN(this, tok, pos).type == DANA_ASSIGN) {
			POP_TOKEN(this, tok, pos);
			return (ast_node_t){
				AST_ASSIGN,
				.op_data.lhs = AST_APPEND(this, node),
				.op_data.rhs = _parse_expr(this, 0),
			};
		}
		_assert(node.type == AST_NAME,
				"Invalid token for begin of proc-call");
		if (PEEK_TOKEN(this, tok, pos).type == DANA_COLON) {
			POP_TOKEN(this, tok, pos);
			args = _parse_args(this);
		}
		return (ast_node_t){
			.type = AST_PROC,
			.name_data = { node.pl_data.name, args },
		};
	default:
		_assert(false, "Could not find start of stmt");
		return (ast_node_t){};
	});

	printf("\tparsed stmt %s\n", ast_get_type_str(node));

	return AST_APPEND(this, node);
}/* }}} */

ast_node_pos
_parse_var (parser_t *this, enum lex_type to_match)
{/* {{{ */
	ast_node_pos ret, prv = {0};
	lex_token_t tok;
	lex_token_pos pos;

	const bool expect_var = to_match == DANA_KW_VAR;
	size_t *names = {0};
	enum ast_type dtype;
	uint16_t dim[4] = {0};
	uint8_t nd = 0;

	printf(expect_var ? "Parse var-def\n": "Parse fpar-def\n");

	if (expect_var) _assert(NEXT_TOKEN(this, tok, pos).type == DANA_KW_VAR,
			"Did not find var keyword on var-def");
	do arr_push(names, _get_name(this));
	while (PEEK_TOKEN(this, tok, pos).type == DANA_NAME);

	printf("---\tNames:");
	for (uint32_t i=0; i<arr_ulen(names); ++i)
		printf(" %.*s", UNSLICE(parser_get_name(this, names[i])));
	printf("\n");

	if (expect_var) _assert(NEXT_TOKEN(this, tok, pos).type == DANA_KW_IS,
			"Did not find is keyword on var-def");
	else            _assert(NEXT_TOKEN(this, tok, pos).type == DANA_KW_AS,
			"Did not find as keyword on fpar-def");

	dtype = SWITCH(NEXT_TOKEN(this, tok, pos).type, enum ast_type, {
	case DANA_KW_INT:
	case DANA_KW_BYTE: return tok.type;
	case DANA_KW_REF:
		_assert(!expect_var, "Cannot use ref on var-def");
		return SWITCH(NEXT_TOKEN(this, tok, pos).type, enum ast_type, {
		case DANA_KW_INT : return AST_REF_INT;
		case DANA_KW_BYTE: return AST_REF_BYTE;
		default: _assert(false, "Invalid type for ref");
		});
	default: _assert(false, "Invalid type for var-def/fpar-def");
	});

	if (PEEK_TOKEN(this, tok, pos).type == DANA_OPEN_BRACKET) {
		dtype = SWITCH(dtype, enum ast_type, {
		case AST_INT : return AST_ARR_INT;
		case AST_BYTE: return AST_ARR_BYTE;
		default      : _assert(false, "Cannot have reference to array");
		});
	}
	while (PEEK_TOKEN(this, tok, pos).type == DANA_OPEN_BRACKET) {
		POP_TOKEN(this, tok, pos);
		_assert(nd < 4, "Cannot have more than 4 array dimensions");
		switch (NEXT_TOKEN(this, tok, pos).type) {
		case DANA_CLOSE_BRACKET:
			_assert(!expect_var, "Cannot have var-array on fpar-def");
			_assert(nd == 0, "Only first dimension can be variable");
			++nd;
			continue;
		case DANA_NUMBER: {
			slice_char_t sl = parser_get_value_by_tok(this, tok);
			LEX_TEMP_SLICE(sl);
			dim[nd++] = atoi(sl.ptr);
			break;
		}
		default:
			_assert(false, "Invalid contains of array def dimensions");
		}
		_assert(NEXT_TOKEN(this, tok, pos).type == DANA_CLOSE_BRACKET,
				"Mismatching brackets in var/farg-def");
	}

	printf("---\tdtype: %s (%u)\n", ast_symbol_arr[dtype], dtype);
	printf("---\tdim: [%u %u %u %u]\n", dim[0], dim[1], dim[2], dim[3]);

	for (uint32_t i=0; i<arr_ulen(names); ++i) {
		printf("---\tAppending %.*s\n",
				UNSLICE(parser_get_name(this, names[i])));
		ast_node_pos node = AST_APPEND(this, ((ast_node_t){
			.type = dtype,
			.var_data = {
				.name = names[i],
				.dim[0] = dim[0], .dim[1] = dim[1],
				.dim[2] = dim[2], .dim[3] = dim[3],
			},
		}));
		if (prv.pos) this->ast[prv.pos].var_data.next = node;
		else ret = node;
		prv = node;
	}

	// for (ast_node_pos i=ret; i.pos < prv.pos; ++i.pos)
		// _ast_node_print(this, i, "\n");

	arr_free(names);
	printf("END var\n");
	return ret;
}/* }}} */

