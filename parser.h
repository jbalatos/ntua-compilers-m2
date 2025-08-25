#pragma once

#ifdef PARSER_IMPLEMENT
	#define BP_IMPLEMENT
	#define AST_IMPLEMENT
	#define LEX_IMPLEMENT
	#define DA_IMPLEMENT
#endif

#include "bind_power.h"
#include "ast.h"
#include "lexer.h"
#include "util.h"
#include "util/dynamic_array.h"

POS_DECL(lex_token_pos, 24);

typedef struct { size_t key; lex_token_pos value; } parser_name_t;

typedef struct {
	lexer_t        lexer;  /* underlying lexer */
	lex_token_t   *tokens; /* dynamic array of the tokens created by lexer */
	lex_token_t   *last;   /* pointer to last element of tokens */
	ast_node_t    *ast;    /* dynamic array of the AST nodes */
	ast_node_pos  *extra;  /* dynamic array of slices of AST indices, used by
		         	 specific node types */
	char          *text;   /* dynamic array of const strings (0-terminated) */
	parser_name_t *names;  /* hash map matching name ID => name position */
} parser_t;

#define extra_for_each(p, ex, i, it, body...) do {                           \
	for (uint32_t i = 0; i < (ex).length; ++ i) {                        \
		ast_node_pos it = parser_get_extra(p, POS_ADV((ex).pos, i)); \
		body                                                         \
	}                                                                    \
} while (0)

#define PARSE_CLEANUP         __attribute__((cleanup(parser_destroy)))
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
/* pratt parser */
#define parser_print_node(this, pos) _parser_print_node(this, pos, "")
extern ast_node_pos           parse (parser_t *this);

#ifdef PARSER_IMPLEMENT
/** private method definitions */
lex_token_pos _parser_next_token (parser_t *this);
lex_token_pos _parser_peek_token (parser_t *this);
lex_token_pos _parser_pop_token (parser_t *this);
void          _parser_print_node (const parser_t *this, ast_node_pos pos, const char *end);
void          _parser_print_decl (const parser_t *this, const ast_node_t node, const char *end);

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
	return lex_get_token(&this->lexer, tok);
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

void
_parser_print_decl (const parser_t *this, const ast_node_t node, const char *end)
{
	printf("%.*s (params: ",
			UNSLICE(parser_get_name(this, node.name_data.name)));
	if (POS_OK(node.name_data.body))
		parser_print_node(this, node.name_data.body);
	printf(")"); printf(end);
}

/* }}} */

/** parser print */
void
_parser_print_node (const parser_t *this, ast_node_pos pos, const char *end)
{/* {{{ */
	ast_node_t node = parser_get_node(this, pos);
	lex_token_pos tok;

	switch (node.type) {
	/* literals */
	case AST_NUMBER:
		printf("%d", node.pl_data.num);
	break;	case AST_BOOL:
		printf("%s", node.pl_data.num ? "true" : "false");
	break;	case AST_CHAR:
		printf("'%c'", node.pl_data.ch);
	break;	case AST_STRING:
		printf("\"%s\"", parser_get_text(this, node.pl_data.str));
	break;	case AST_NAME:
		tok = hm_get(this->names, node.pl_data.name);
		printf("#%.*s", UNSLICE(parser_get_value_by_pos(this, tok)));
	/* variables */
	break;	case AST_INT ... AST_BYTE:
		case AST_REF_INT ... AST_ARR_BYTE:
		printf("(%s %.*s", ast_get_type_str(node),
				UNSLICE(parser_get_name(this, node.var_data.name)));
		if (node.type == AST_ARR_INT || node.type == AST_ARR_BYTE)
			for (uint8_t i=0; i<4; ++i) {
				if (node.var_data.dim[i])
					printf("[%u]", node.var_data.dim[i]);
				else if (i == 0)
					printf("[]");
			}
		printf(")");
		if (POS_OK(node.var_data.next))
			parser_print_node(this, node.var_data.next);
	/* operators */
	break;	case AST_PLUS ... AST_CMP_GEQ:
		printf("(%s ", ast_get_type_str(node));
		_parser_print_node(this, node.op_data.lhs, " ");
		_parser_print_node(this, node.op_data.rhs, ")");
	break;	case AST_ARR_AT:
		_parser_print_node(this, node.op_data.lhs, "[");
		_parser_print_node(this, node.op_data.rhs, "]");
	/* func call */
	break;	case AST_FUNC:
		printf("func-call %.*s", UNSLICE(parser_get_name(this, node.name_data.name)));
		_parser_print_node(this, node.name_data.body, ")");
	break;	case AST_PROC:
		printf("proc-call %.*s", UNSLICE(parser_get_name(this, node.name_data.name)));
		_parser_print_node(this, node.name_data.body, ")");
	break;	case AST_ARGS:
		printf("(args: ");
		extra_for_each(this, node.extra_data, i, it,
			if (i) printf(", ");
			parser_print_node(this, it);
		);
		printf(")");
	/* function decl-def */
	break;	case AST_DECL_PROC ... AST_DECL_BYTE:
		printf("(%s ", ast_get_type_str(node));
		_parser_print_decl(this, node, ")");
	break;	case AST_DEF_PROC ... AST_DEF_BYTE:
		printf("(%s ", ast_get_type_str(node));
		_parser_print_decl(this, parser_get_node(this, node.op_data.lhs),
				" : ");
		_parser_print_node(this, node.op_data.rhs, ")");
	break; case AST_LOCAL_DEF:
		if (node.extra_data.length > 1) {
			printf("(defs:");
			extra_for_each(this, node.extra_data, i, it,
				if (i == node.extra_data.length - 1) break;
				printf(" ");
				parser_print_node(this, it);
			);
			printf(")");
		} else {
			printf("(no defs)");
		}
		printf(" => ");
		parser_print_node(this, parser_get_extra(this, POS_ADV(
						node.extra_data.pos,
						node.extra_data.length - 1)));
	/* statements */
	break;	case AST_BLOCK:
		printf("[");
		extra_for_each(this, node.extra_data, i, it,
			if (i) printf(", ");
			parser_print_node(this, it);
		);
		printf("]");
	break; case AST_BLOCK_SIMPLE:
		printf("[");
		_parser_print_node(this, node.name_data.body, "]");
	break;	case AST_SKIP:
		case AST_EXIT:
		case AST_BREAK:
		case AST_CONT:
		printf("(%s", ast_get_type_str(node));
		if (node.name_data.name)
			printf(" #%.*s", UNSLICE(parser_get_name(this,
							node.name_data.name)));
		printf(")");
	break;	case AST_RETURN:
		printf("(return ");
		_parser_print_node(this, node.op_data.lhs, ")");
	break;	case AST_ASSIGN:
		printf("(");
		_parser_print_node(this, node.op_data.lhs, " := ");
		_parser_print_node(this, node.op_data.rhs, ")");
	break;	case AST_LOOP:
		printf("(loop ");
		if (node.name_data.name)
			printf("#%.*s ", UNSLICE(parser_get_name(this,
							node.name_data.name)));
		_parser_print_node(this, node.name_data.body, ")");
	break;	case AST_COND_SIMPLE:
		printf("(if ");
		_parser_print_node(this, node.op_data.lhs, " => ");
		_parser_print_node(this, node.op_data.rhs, ")");
	break;	case AST_COND:
		printf("(if");
		extra_for_each(this, node.extra_data, i, it,
			if (i % 2 == 0 && i != node.extra_data.length - 1) {
				printf(" ");
				_parser_print_node(this, it, " => ");
			} else {
				if (i % 2 == 0) printf(" else => ");
				parser_print_node(this, it);
			}
		);
		printf(")");
	break;	default:
		printf("%s(%u) -- PENDING", ast_get_type_str(node), node.type);
	}
	printf(end);
}/* }}} */

/** parsing functions */
#include "pratt_parser.c"

inline ast_node_pos
parse (parser_t *this)
{ return _parse_decl(this, DANA_KW_DEF); }
#endif // PARSER_IMPLEMENT
