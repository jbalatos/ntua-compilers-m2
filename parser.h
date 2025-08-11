#pragma once

#include <stdio.h>
#include "lexer.h"
#include "util/dynamic_array.h"

#ifndef _assert
	#define _assert(cond, fmt, ...) \
		assert(cond && "ASSERT\t%s:%d\t" fmt, __FILE__, __LINE__)
#endif

POS_DECL(ast_node_pos, 24);
POS_DECL(lex_token_pos, 24);
POS_DECL(extra_pos, 24);

#define OP(e, s) AST_    ## e,
#define TK(e, s) AST_    ## e,
#define LIT(e)   AST_    ## e,
#define KW(e, s) AST_KW_ ## e,
#define KW_EX(e, al, s) LIT(al)
typedef struct {
	enum ast_ttype {
		AST_ERROR = 0,
		DANA_TYPES
		DANA_KEYWORDS
		AST_TTYPE_LEN,
	} type : 8;
	union {
		struct ast_bin_data {
			ast_node_pos lhs, rhs;
		} bin_data;
		struct ast_pl_data {
			uint32_t value;
		} pl_data;
		struct ast_named_data {
			lex_token_pos name; ast_node_pos block;
		} named_data;
		struct ast_extra_data {
			uint32_t length; extra_pos pos;
		} extra_data;
		struct ast_if_data {
			ast_node_pos cond, body;
		} if_data;
	};
} ast_node_t;
#undef OP
#undef TK
#undef LIT
#undef KW
#undef KW_EX

typedef struct {
	lexer_t       lexer;  /* underlying lexer */
	lex_token_t  *tokens; /* dynamic array of the tokens created by lexer */
	lex_token_t  *last;   /* pointer to last element of tokens */
	ast_node_t   *ast;    /* dynamic array of the AST nodes */
	ast_node_pos *extra;  /* dynamic array of slices of AST indices, used by
				 specific node types */
} parser_t;

#define PARSER_CLEANUP parser_t __attribute__((cleanup(parser_destroy)))
extern parser_t               parser_create (const alloc_t *alloc, const char *fname);
extern void                   parser_destroy (const parser_t *this);
/* parser methods */
extern lex_token_t            parser_get_token (const parser_t *this, lex_token_pos pos);
extern ast_node_t             parser_get_node (const parser_t *this, ast_node_pos pos);
extern ast_node_pos           parser_get_extra (const parser_t *this, extra_pos pos);
extern slice_char_t           parser_get_name (const parser_t *this, ast_node_t node);
extern slice_char_t           parser_token_val (const parser_t *this, lex_token_t tok);
extern struct ast_extra_data  parser_append_extras (parser_t *this, ast_node_pos *arr);
extern lex_token_pos         _parser_next_token (parser_t *this);
extern lex_token_pos         _parser_peek_token (parser_t *this);
extern lex_token_pos         _parser_pop_token (parser_t *this);
/* pratt parser */
extern ast_node_pos          _parse_expr (parser_t *this, uint8_t min_bp);
extern ast_node_pos          _parse_block (parser_t *this);
extern ast_node_pos          _parse_stmt (parser_t *this);
extern ast_node_pos           parser_parse (parser_t *this);

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
	arr_push(ret.extra, (ast_node_pos){});
	return ret;
}

void
parser_destroy (const parser_t *this)
{
	arr_free(this->tokens);
	arr_free(this->ast);
	arr_free(this->extra);
	lexer_destroy(&this->lexer);
}
/* }}} */

/** parser methods */
/* {{{ */
lex_token_t
parser_get_token (const parser_t *this, lex_token_pos pos)
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

ast_node_pos
parser_get_extra (const parser_t *this, extra_pos pos)
{
	_assert(pos.pos < arr_ulen(this->extra),
			"Extra %u out of bounds (%lu)",
			pos.pos, arr_ulen(this->extra));
	return this->extra[pos.pos];
}

slice_char_t
parser_get_name (const parser_t *this, ast_node_t node)
{
	lex_token_t tok = parser_get_token(this, node.named_data.name);
	switch (node.type) {
	AST_NAMED_NODE:
		if (node.named_data.name.pos)
			return parser_token_val(this, tok);
		else
			return (slice_char_t){};
	default:
		_assert(false, "Requesting name of unnamed node: %10s [%.*s]",
				lex_ttype_str(tok),
				UNSLICE(parser_token_val(this, tok)));
		return (slice_char_t){};
	}
}

inline slice_char_t
parser_token_val (const parser_t *this, lex_token_t tok)
{ return lex_token_val(&this->lexer, tok); }

struct ast_extra_data
parser_append_extras (parser_t *this, ast_node_pos *arr)
{
	struct ast_extra_data ret = {
		.length = arr_ulen(arr),
		.pos = { arr_ulen(this->extra) },
	};
	for (uint32_t i=0; i<arr_ulen(arr); ++i) arr_push(this->extra, arr[i]);
	arr_free(arr);
	return ret;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsequence-point"
lex_token_pos
_parser_next_token (parser_t *this)
{
	lex_token_t tok = lex_next_token(&this->lexer, this->last);
	if (arr_ulen(this->tokens) == 1 || POS_CMP(arr_back(this->tokens).pos, tok.pos))
		arr_push(this->tokens, tok);
	this->last = &arr_back(this->tokens);
	return (lex_token_pos){ arr_ulen(this->tokens) - 1 };
}
#pragma GCC diagnostic pop

lex_token_pos
_parser_peek_token (parser_t *this)
{
	if (arr_ulen(this->tokens) == 1 || !POS_CMP(arr_back(this->tokens).pos, this->last->pos))
		arr_push(this->tokens, lex_next_token(&this->lexer, this->last));
	this->last = &(this->tokens[arr_ulen(this->tokens) - 2]);
	return (lex_token_pos){ arr_ulen(this->tokens) - 1 };
}

lex_token_pos
_parser_pop_token (parser_t *this)
{
	assert(arr_ulen(this->tokens) > 1 && this->last != &arr_back(this->tokens));
	this->last = &arr_back(this->tokens);
	return (lex_token_pos){ arr_ulen(this->tokens) - 1 };
}
/* }}} */

/** binding power */
/* {{{ */
/* }}} */

/** pratt parser */
#include "pratt_parse.h"

ast_node_pos
parser_parse (parser_t *this)
{
	return _parse_block(this);
}
#endif // PARSER_IMPLEMENT
