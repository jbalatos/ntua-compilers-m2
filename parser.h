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
	} type : 8;
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

#define PARSER_CLEANUP parser_t __attribute__((cleanup(parser_destroy)))
extern parser_t      parser_create (const alloc_t *alloc, const char *fname);
extern void          parser_destroy (const parser_t *this);
/* parser methods */
extern lex_token_t   parser_get_token (const parser_t *this, token_pos pos);
extern ast_node_t    parser_get_node (const parser_t *this, ast_node_pos pos);
extern slice_char_t  parser_token_val (const parser_t *this, lex_token_t tok);
extern token_pos     _parser_next_token (parser_t *this);
extern token_pos     _parser_peek_token (parser_t *this);
/* pratt parser */
extern ast_node_pos  _parse_expr (parser_t *this, uint8_t min_bp);
extern ast_node_pos  parser_parse (parser_t *this);

#define PEEK_TOKEN(prs, tok, pos) ({                     \
		(pos) = _parser_peek_token((prs));        \
		(tok) = parser_get_token((prs), (pos)); })
#define NEXT_TOKEN(prs, tok, pos) ({                     \
		(pos) = _parser_next_token((prs));        \
		(tok) = parser_get_token((prs), (pos)); })
#define AST_APPEND(prs, node) ({                            \
		arr_push((prs)->ast, (node));               \
		(ast_node_pos){ arr_ulen((prs)->ast) - 1}; })

#ifdef PARSER_IMPLEMENT
	#define BP_IMPLEMENT
	#define AST_IMPLEMENT
#endif
#include "bind_power.h"
#include "ast.h"

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
token_pos
_parser_next_token (parser_t *this)
{
	lex_token_t tok = lex_next_token(&this->lexer, this->last);
	if (arr_ulen(this->tokens) == 1 || POS_CMP(arr_back(this->tokens).pos, tok.pos))
		arr_push(this->tokens, tok);
	this->last = &arr_back(this->tokens);
	return (token_pos){ arr_ulen(this->tokens) - 1 };
}
#pragma GCC diagnostic pop

token_pos
_parser_peek_token (parser_t *this)
{
	if (arr_ulen(this->tokens) == 1 || !POS_CMP(arr_back(this->tokens).pos, this->last->pos))
		arr_push(this->tokens, lex_next_token(&this->lexer, this->last));
	return (token_pos){ arr_ulen(this->tokens) - 1 };
}
/* }}} */

/** binding power */
/* {{{ */
/* }}} */

/** pratt parser */
/* {{{ */
ast_node_pos
_parse_expr (parser_t *this, uint8_t min_bp)
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
		node = (ast_node_t){ .type = AST_NAME, .token = pos };
		lhs = AST_APPEND(this, node);
		break;
	case DANA_NUMBER: {
		slice_char_t sl = parser_token_val(this, tok);
		LEX_TEMP_SLICE(sl);
		node = (ast_node_t){
			.type = AST_NUMBER, .token = pos,
			.pl_data.value = atoi(sl.ptr),
		};
		lhs = AST_APPEND(this, node);
		break;
	}
	case DANA_OPEN_PAREN:
		lhs = _parse_expr(this, 0);
		NEXT_TOKEN(this, tok, pos);
		_assert(tok.type == DANA_CLOSE_PAREN,
				"No closing paren found: found [%s] |%.*s|",
				lex_ttype_str(tok),
				UNSLICE(parser_token_val(this, tok)));
		node = parser_get_node(this, lhs);
		break;
	default:
		if (bp_is_prefix(tok, &bp)) {
			node = (ast_node_t){ .type = tok.type, .token = pos };
			node.op_data.lhs = _parse_expr(this, bp.rhs);
			lhs = AST_APPEND(this, node);
			break;
		}
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
		default:
			if (bp_is_infix(tok, &bp) || bp_is_postfix(tok, &bp)) {
				node = (ast_node_t){ .type = tok.type, .token = pos };
				break;
			}
			_assert(false, "Peeked token not recognised: [%s] |%.*s|",
					lex_ttype_str(tok),
					UNSLICE(parser_token_val(this, tok)));
		}

		if (bp_is_postfix(node, &bp)) {
			if (bp.lhs < min_bp) break;
			_parser_next_token(this);
			node.op_data.lhs = lhs;
			switch (tok.type) {
			case DANA_OPEN_BRACKET:
				node.op_data.rhs = _parse_expr(this, 0);
				NEXT_TOKEN(this, tok, pos);
				_assert(tok.type == DANA_CLOSE_BRACKET,
						"No closing bracket found: found [%s] |%.*s|",
						lex_ttype_str(tok),
						UNSLICE(parser_token_val(this, tok)));
				break;
			case DANA_OPEN_PAREN:
				node.op_data.rhs = _parse_expr(this, 0);
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

		if (bp_is_infix(node, &bp)) {
			if (bp.lhs < min_bp) break;
			_parser_next_token(this);
			node.op_data.lhs = lhs;
			node.op_data.rhs = _parse_expr(this, bp.rhs);
			lhs = AST_APPEND(this, node);
			continue;
		}
		break;
	}

	--parse_level;
	return lhs;
}

ast_node_pos parser_parse (parser_t *this) { return _parse_expr(this, 0); }
/* }}} */
#endif // PARSER_IMPLEMENT
