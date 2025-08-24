#pragma once

#include "bind_power.h"
#include "lexer.h"
#include "types.h"
#include "util.h"
#include "util/dynamic_array.h"

struct parser_t {
	lexer_t       lexer;  /* underlying lexer */
	lex_token_t  *tokens; /* dynamic array of the tokens created by lexer */
	lex_token_t  *last;   /* pointer to last element of tokens */
	ast_node_t   *ast;    /* dynamic array of the AST nodes */
	ast_node_pos *extra;  /* dynamic array of slices of AST indices, used by
				 specific node types */
	char         *text;   /* dynamic array of const strings (0-terminated) */
	struct { uint32_t key; lex_token_pos value; } *names;
				/* hash map matching name ID => name position */
};

#define PARSER_CLEANUP parser_t __attribute__((cleanup(parser_destroy)))
extern parser_t               parser_create (const lexer_t lexer);
extern void                   parser_destroy (const parser_t *this);
/* parser methods */
extern const lex_token_t      parser_get_token (const parser_t *this, lex_token_pos pos);
extern const ast_node_t       parser_get_node (const parser_t *this, ast_node_pos pos);
extern const ast_node_pos     parser_get_extra (const parser_t *this, extra_pos pos);
extern const char*            parser_get_text (const parser_t *this, text_pos pos);
extern slice_char_t           parser_get_name (const parser_t *this, size_t hash);
extern slice_char_t           parser_get_value_by_tok (const parser_t *this, lex_token_t tok);
extern slice_char_t           parser_get_value_by_pos (const parser_t *this, lex_token_pos pos);
extern ast_extra_data         parser_append_extras (parser_t *this, ast_node_pos *arr);
extern text_pos               parser_append_text (parser_t *this, lex_token_t tok);
extern lex_token_pos         _parser_pop_token (parser_t *this);
/* pratt parser */
extern ast_node_pos           parse (parser_t *this);

#include "ast.h"

#ifdef PARSER_IMPLEMENT
/** private method definitions */
extern lex_token_pos _parser_next_token (parser_t *this);
extern lex_token_pos _parser_peek_token (parser_t *this);

/** parser constructor - destructor */
/* {{{ */
parser_t
parser_create (const lexer_t lexer)
{
	parser_t ret = { .lexer = lexer };
	arr_push(ret.ast, (ast_node_t){});
	arr_push(ret.tokens, (lex_token_t){});
	arr_push(ret.extra, (ast_node_pos){});
	arr_push(ret.text, '\0');
	return ret;
}

void
parser_destroy (const parser_t *this)
{
	arr_free(this->tokens);
	arr_free(this->ast);
	arr_free(this->extra);
	arr_free(this->text);
	hm_free(this->names);
	lexer_destroy(&this->lexer);
}
/* }}} */

/** parser methods */
/* {{{ */
const lex_token_t
parser_get_token (const parser_t *this, lex_token_pos pos)
{
	_assert(pos.pos < arr_ulen(this->tokens),
			"Token %u out of bounds (%lu)",
			pos.pos, arr_ulen(this->tokens));
	return this->tokens[pos.pos];
}

const ast_node_t
parser_get_node (const parser_t *this, ast_node_pos pos)
{
	_assert(pos.pos < arr_ulen(this->ast),
			"Node %u out of bounds (%lu)",
			pos.pos, arr_ulen(this->ast));
	return this->ast[pos.pos];
}

const ast_node_pos
parser_get_extra (const parser_t *this, extra_pos pos)
{
	_assert(pos.pos < arr_ulen(this->extra),
			"Extra %u out of bounds (%lu)",
			pos.pos, arr_ulen(this->extra));
	return this->extra[pos.pos];
}

const char*
parser_get_text (const parser_t *this, text_pos pos)
{
	_assert(pos.pos < arr_ulen(this->text),
			"Text %u out of bounds (%lu)",
			pos.pos, arr_ulen(this->text));
	return this->text + pos.pos;
}

inline slice_char_t
parser_get_name (const parser_t *this, size_t hash)
{ return  parser_get_value_by_pos(this, hm_get(this->names, hash)); }

inline slice_char_t
parser_get_value_by_tok (const parser_t *this, lex_token_t tok)
{
	return lex_token_val(&this->lexer, tok);
}

inline slice_char_t
parser_get_value_by_pos (const parser_t *this, lex_token_pos pos)
{
	return parser_get_value_by_tok(this, parser_get_token(this, pos));
}

ast_extra_data
parser_append_extras (parser_t *this, ast_node_pos *arr)
{
	ast_extra_data ret = {
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

text_pos
parser_append_text (parser_t *this, lex_token_t tok)
{
	slice_char_t sl = parser_get_value_by_tok(this, tok);
	text_pos ret = {0};

	if (!sl.length) return ret;
	ret.pos = arr_ulen(this->text);
	for (uint32_t i=0; i<sl.length; ++i)
		arr_push(this->text, sl.ptr[i]);
	arr_push(this->text, '\0');
	return ret;
}

lex_token_pos
_parser_pop_token (parser_t *this)
{
	assert(arr_ulen(this->tokens) > 1 && this->last != &arr_back(this->tokens));
	this->last = &arr_back(this->tokens);
	return (lex_token_pos){ arr_ulen(this->tokens) - 1 };
}
/* }}} */

/** parsing functions */
#include "pratt_parser.c"

inline ast_node_pos
parse (parser_t *this)
{ return _parse_var(this, true); }
#endif // PARSER_IMPLEMENT
