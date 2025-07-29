#pragma once

#include <stdio.h>
#include "lexer.h"
#include "util/dynamic_array.h"

#ifndef _assert
	#define _assert(cond, fmt, ...) \
		assert(cond && "ASSERT\t%s:%d\n", __FILE__, __LINE__)
#endif

POS_DECL(ast_node_pos, 32);
POS_DECL(token_pos, 24);

#define OP(e, s) AST_    ## e,
#define TK(e, s) AST_    ## e,
#define LIT(e)   AST_    ## e,
#define KW(e, s) AST_KW_ ## e,
typedef struct {
	enum ast_ttype {
		AST_ERROR = 0,
		DANA_TYPES
		DANA_KEYWORDS
	} tag : 8;
	token_pos token;
	union {
		struct { ast_node_pos lhs, rhs; } op_data;
		struct { uint32_t value; ast_node_pos src; } pl_data;
		struct { ast_node_pos src; } name_data;
	};
} ast_node_t;
#undef OP
#undef TK
#undef LIT
#undef KW

typedef struct {
	lexer_t lexer;
	lex_token_t *tokens;
	lex_token_t *last;
	ast_node_t *ast;
} parser_t;

typedef struct { uint8_t lhs, rhs; } parser_bp_t;

#define PARSER_CLEANUP parser_t __attribute__((cleanup(parser_destroy)))
extern parser_t      parser_create (const alloc_t *alloc, const char *fname);
extern void          parser_destroy (const parser_t *this);
/* parser methods */
extern lex_token_t   parser_get_token (const parser_t *this, token_pos pos);
extern ast_node_t    parser_get_node (const parser_t *this, ast_node_pos pos);
extern slice_char_t  parser_token_val (const parser_t *this, lex_token_t tok);
static token_pos     parser_next_token (parser_t *this);
static token_pos     parser_peek_token (parser_t *this);
/* AST */
extern const char*   ast_tag_str (ast_node_t node);
extern void          ast_node_print (const parser_t *this, ast_node_pos pos);
/* pratt parser */
static ast_node_pos  _parse (parser_t *this, uint8_t min_bp);
extern ast_node_pos  parser_parse (parser_t *this);

#define PEEK_TOKEN(prs, tok, pos) ({                     \
		(pos) = parser_peek_token((prs));        \
		(tok) = parser_get_token((prs), (pos)); })
#define NEXT_TOKEN(prs, tok, pos) ({                     \
		(pos) = parser_next_token((prs));        \
		(tok) = parser_get_token((prs), (pos)); })
#define AST_APPEND(prs, node) ({                            \
		arr_push((prs)->ast, (node));               \
		(ast_node_pos){ arr_ulen((prs)->ast) - 1}; })

#ifdef PARSER_IMPLEMENT
/** parser destructor */
/* {{{ */
parser_t
parser_create (const alloc_t *alloc, const char *fname)
{
	parser_t ret = { .lexer = lexer_create(alloc, fname) };
	arr_push(ret.ast, (ast_node_t){});
	arr_push(ret.tokens, (lex_token_t){});
	return ret;
}

void
parser_destroy (const parser_t *this)
{
	arr_free(this->ast);
	arr_free(this->tokens);
	lexer_destroy(&this->lexer);
}
/* }}} */

/** parser methods */
/* {{{ */
lex_token_t
parser_get_token (const parser_t *this, token_pos pos)
{
	_assert(pos.pos < arr_ulen(this->tokens),
			"Token %u out of bounds (%lu)",
			pos.pos, arr_ulen(this->tokens));
	return this->tokens[pos.pos];
}

ast_node_t
parser_get_node (const parser_t *this, ast_node_pos pos)
{
	_assert(pos.pos < arr_ulen(this->ast),
			"Node %u out of bounds (%lu)",
			pos.pos, arr_ulen(this->ast));
	return this->ast[pos.pos];
}

inline slice_char_t
parser_token_val (const parser_t *this, lex_token_t tok)
{ return lex_token_val(&this->lexer, tok); }

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsequence-point"
static token_pos
parser_next_token (parser_t *this)
{
	lex_token_t tok = lex_next_token(&this->lexer, this->last);
	if (arr_ulen(this->tokens) == 1 || POS_CMP(arr_back(this->tokens).pos, tok.pos))
		arr_push(this->tokens, tok);
	this->last = &arr_back(this->tokens);
	return (token_pos){ arr_ulen(this->tokens) - 1 };
}
#pragma GCC diagnostic pop

static token_pos
parser_peek_token (parser_t *this)
{
	if (arr_ulen(this->tokens) == 1 || !POS_CMP(arr_back(this->tokens).pos, this->last->pos))
		arr_push(this->tokens, lex_next_token(&this->lexer, this->last));
	return (token_pos){ arr_ulen(this->tokens) - 1 };
}
/* }}} */

/** binding power */
/* {{{ */
#define PRE(e, bp)
#define IN(e, bp, ass) [AST_ ## e] = { ((bp) << 1) + (ass), ((bp) << 1) + 1 - (ass) },
#define POST(e, bp)
static parser_bp_t infix_table[] = {
	DANA_OPERATORS
};
#undef PRE
#undef IN
#undef POST

#define PRE(e, bp) [AST_ ## e] = { 0, (bp) << 1 },
#define IN(e, bp, ass)
#define POST(e, bp)
static parser_bp_t prefix_table[] = {
	DANA_OPERATORS
};
#undef PRE
#undef IN
#undef POST

#define PRE(e, bp)
#define IN(e, bp, ass)
#define POST(e, bp) [AST_ ## e] = { (bp) << 1, 0 },
static parser_bp_t postfix_table[] = {
	DANA_OPERATORS
};
#undef PRE
#undef IN
#undef POST
/* }}} */

/** AST */
/* {{{ */
#define OP(e, s) [AST_    ## e] = s,
#define TK(e, s)
#define LIT(e)   [AST_    ## e] = #e,
#define KW(e, s) [AST_KW_ ## e] = s,
static const char* tag_table[] = {
	[AST_ERROR] = "error",
	DANA_TYPES
	DANA_KEYWORDS
};
#undef OP
#undef TK
#undef LIT
#undef KW

inline const char*
ast_tag_str (ast_node_t node)
{ return tag_table[node.tag]; }

void
ast_node_print (const parser_t *this, ast_node_pos pos)
{
	ast_node_t node = parser_get_node(this, pos);

	switch (node.tag) {
	case AST_NUMBER:
		printf("%d", node.pl_data.value);
		break;
	case AST_NAME:
		printf("%.*s", UNSLICE(parser_token_val(this,
						parser_get_token(this, node.token))));
		break;
	case AST_OPEN_PAREN:
		ast_node_print(this, node.op_data.lhs); printf("(");
		ast_node_print(this, node.op_data.rhs); printf(")");
		break;
	case AST_OPEN_BRACKET:
		ast_node_print(this, node.op_data.lhs); printf("[");
		ast_node_print(this, node.op_data.rhs); printf("]");
		break;
	case AST_PREF_OPS:
	case AST_BIN_OPS:
		printf("(%s ", ast_tag_str(node));
		ast_node_print(this, node.op_data.lhs);
		if (node.op_data.rhs.pos) {
			printf(" "); ast_node_print(this, node.op_data.rhs);
		}
		printf(")");
		break;
	default:
		printf("(%s)", ast_tag_str(node));
		break;
	}
}

/* }}} */

/** pratt parser */
/* {{{ */
static ast_node_pos
_parse (parser_t *this, uint8_t min_bp)
{
	static uint16_t parse_level = 0;

	ast_node_t node;
	ast_node_pos lhs;
	parser_bp_t bp;
	lex_token_t tok;
	token_pos pos;

	++parse_level;
	NEXT_TOKEN(this, tok, pos);
	printf("parsing (min_bp = %u, level = %u)\tfirst_token = %10s\t|%.*s|\n",
			min_bp, parse_level, lex_ttype_str(tok),
			UNSLICE(parser_token_val(this, tok)));

	/* initial token */
	switch (tok.type) {
	case DANA_NAME:
		node = (ast_node_t){ .tag = AST_NAME, .token = pos };
		lhs = AST_APPEND(this, node);
		break;
	case DANA_NUMBER: {
		slice_char_t sl = parser_token_val(this, tok);
		LEX_TEMP_SLICE(sl);
		node = (ast_node_t){
			.tag = AST_NUMBER, .token = pos,
			.pl_data.value = atoi(sl.ptr),
		};
		lhs = AST_APPEND(this, node);
		break;
	}
	case DANA_OPEN_PAREN:
		lhs = _parse(this, 0);
		NEXT_TOKEN(this, tok, pos);
		_assert(tok.type == DANA_CLOSE_PAREN,
				"No closing paren found: found [%s] |%.*s|",
				lex_ttype_str(tok),
				UNSLICE(parser_token_val(this, tok)));
		node = parser_get_node(this, lhs);
		break;
	case DANA_PREF_OPS:
		node = (ast_node_t){ .tag = tok.type, .token = pos };
		bp = prefix_table[node.tag];
		node.op_data.lhs = _parse(this, bp.rhs);
		lhs = AST_APPEND(this, node);
		break;
	default:
		_assert(false, "First token not recognised: [%s] |%.*s|",
				lex_ttype_str(tok),
				UNSLICE(parser_token_val(this, tok)));
	}

	/* following */
	while (PEEK_TOKEN(this, tok, pos).type != DANA_EOF) {
		printf("peeking\t\t\t\tfirst_token = %10s\t|%.*s|\n",
				lex_ttype_str(tok),
				UNSLICE(parser_token_val(this, tok)));
		switch (tok.type) {
		case DANA_OPS:
			node = (ast_node_t){ .tag = tok.type, .token = pos };
			break;
		default:
			_assert(false, "Peeked token not recognised: [%s] |%.*s|",
					lex_ttype_str(tok),
					UNSLICE(parser_token_val(this, tok)));
		}

		/* postfix */
		if ((bp = postfix_table[node.tag]).lhs) {
			if (bp.lhs < min_bp) break;
			parser_next_token(this);
			node.op_data.lhs = lhs;
			switch (tok.type) {
			case DANA_OPEN_BRACKET:
				node.op_data.rhs = _parse(this, 0);
				NEXT_TOKEN(this, tok, pos);
				_assert(tok.type == DANA_CLOSE_BRACKET,
						"No closing bracket found: found [%s] |%.*s|",
						lex_ttype_str(tok),
						UNSLICE(parser_token_val(this, tok)));
				break;
			case DANA_OPEN_PAREN:
				node.op_data.rhs = _parse(this, 0);
				NEXT_TOKEN(this, tok, pos);
				_assert(tok.type == DANA_CLOSE_PAREN,
						"No closing paren found: found [%s] |%.*s|",
						lex_ttype_str(tok),
						UNSLICE(parser_token_val(this, tok)));
				break;
			default:
			}
			lhs = AST_APPEND(this, node);
			continue;
		}

		/* infix */
		if ((bp = infix_table[node.tag]).lhs) {
			if (bp.lhs < min_bp) break;
			parser_next_token(this);
			node.op_data.lhs = lhs;
			node.op_data.rhs = _parse(this, bp.rhs);
			lhs = AST_APPEND(this, node);
			continue;
		}
		break;
	}

	--parse_level;
	return lhs;
}

ast_node_pos parser_parse (parser_t *this) { return _parse(this, 0); }
/* }}} */
#endif // PARSER_IMPLEMENT
