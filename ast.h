#pragma once

#include "parser.h"
#include "symbols.h"
#include "types.h"

#define OP(l, p, s)  AST_ ## p,
#define TK(l, p, s)  AST_ ## p,
#define LT(l, p, s)  AST_ ## p,
#define KW(l, p, s)  AST_ ## p,
#define LT_PAR(p, s) AST_ ## p,
#define KW_PAR(p, s) AST_ ## p,
/* just for alignment between lex_type and ast_type */
#define OP_LEX(l, s) AST_UNUSED_ ## l,
#define TK_LEX(l, s) AST_UNUSED_ ## l,
#define LT_LEX(l, s) AST_UNUSED_ ## l,
#define KW_LEX(l, s) AST_UNUSED_ ## l,

struct ast_node_t {
	enum ast_type {
		AST_ERROR = 0,
		DANA_TYPES
		DANA_KEYWORDS
		AST_TYPES_LEN,
	} type : 8;
	union {
		union ast_pl_data {
			int16_t  num;
			char     ch;
			text_pos str;
			size_t   name;
		} pl_data;
		struct ast_op_data {
			ast_node_pos lhs, rhs;
		} op_data;
		struct ast_name_data {
			size_t name; ast_node_pos body;
		} name_data;
		struct ast_extra_data {
			uint32_t length; extra_pos pos;
		} extra_data;
		struct ast_var_data {
			size_t name; uint16_t dim[4]; ast_node_pos next;
		} var_data;
	};
};

#undef OP
#undef TK
#undef LT
#undef KW
#undef LT_PAR
#undef KW_PAR
#undef OP_LEX
#undef TK_LEX
#undef LT_LEX
#undef KW_LEX

extern const char* ast_get_type_str (ast_node_t node);
extern void       _ast_print_decl (const parser_t *this, const ast_node_t node, const char *end);
extern void       _ast_node_print (const parser_t *this, ast_node_pos pos, const char *end);

#define ast_node_print(this, pos) _ast_node_print(this, pos, "")

#ifdef AST_IMPLEMENT

/** ast_symbol_arr */
/* {{{ */
#define OP(l, p, s)  [AST_ ## p] = s,
#define TK(l, p, s)  [AST_ ## p] = s,
#define LT(l, p, s)  [AST_ ## p] = s,
#define KW(l, p, s)  [AST_ ## p] = s,
#define LT_PAR(p, s) [AST_ ## p] = s,
#define KW_PAR(p, s) [AST_ ## p] = s,
#define OP_LEX(...)
#define TK_LEX(...)
#define LT_LEX(...)
#define KW_LEX(...)

static const char* ast_symbol_arr[AST_TYPES_LEN] = {
	[AST_ERROR] = "ERR",
	DANA_TYPES
	DANA_KEYWORDS
};

#undef OP
#undef TK
#undef LT
#undef KW
#undef LT_PAR
#undef OP_LEX
#undef TK_LEX
#undef LT_LEX
#undef KW_LEX
/* }}} */

inline const char*
ast_get_type_str (ast_node_t node)
{ return ast_symbol_arr[node.type]; }

void
_ast_print_decl (const parser_t *this, const ast_node_t node, const char *end)
{
	printf("%.*s (params: ",
			UNSLICE(parser_get_name(this, node.name_data.name)));
	if (node.name_data.body.pos)
		ast_node_print(this, node.name_data.body);
	printf(")"); printf(end);
}

void
_ast_node_print (const parser_t *this, ast_node_pos pos, const char *end)
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
		if (node.var_data.next.pos)
			ast_node_print(this, node.var_data.next);
	/* operators */
	break;	case AST_PLUS ... AST_CMP_GEQ:
		printf("(%s ", ast_get_type_str(node));
		_ast_node_print(this, node.op_data.lhs, " ");
		_ast_node_print(this, node.op_data.rhs, ")");
	break;	case AST_ARR_AT:
		_ast_node_print(this, node.op_data.lhs, "[");
		_ast_node_print(this, node.op_data.rhs, "]");
	/* func call */
	break;	case AST_FUNC:
		printf("func-call %.*s", UNSLICE(parser_get_name(this, node.name_data.name)));
		_ast_node_print(this, node.name_data.body, ")");
	break;	case AST_PROC:
		printf("proc-call %.*s", UNSLICE(parser_get_name(this, node.name_data.name)));
		_ast_node_print(this, node.name_data.body, ")");
	break;	case AST_ARGS:
		printf("(args: ");
		for (uint32_t i=0; i<node.extra_data.length; ++i) {
			if (i) printf(", ");
			ast_node_print(this, parser_get_extra(this,
						POS_ADV(node.extra_data.pos, i)));
		}
		printf(")");
	/* function decl-def */
	break;	case AST_DECL_PROC ... AST_DECL_BYTE:
		printf("(%s ", ast_get_type_str(node));
		_ast_print_decl(this, node, ")");
	break;	case AST_DEF_PROC ... AST_DEF_BYTE:
		printf("(%s ", ast_get_type_str(node));
		_ast_print_decl(this, parser_get_node(this, node.op_data.lhs),
				" : ");
		_ast_node_print(this, node.op_data.rhs, ")");
	break; case AST_LOCAL_DEF:
		if (node.extra_data.length > 1) {
			printf("(defs:");
			for (uint32_t i=0; i < node.extra_data.length - 1; ++i) {
				printf(" ");
				ast_node_print(this, parser_get_extra(this,
							POS_ADV(node.extra_data.pos, i)));
			}
			printf(")");
		} else {
			printf("(no defs)");
		}
		printf(" => ");
		ast_node_print(this, parser_get_extra(this, POS_ADV(
						node.extra_data.pos,
						node.extra_data.length - 1)));
	/* statements */
	break;	case AST_BLOCK:
		printf("[");
		for (uint32_t i=0; i<node.extra_data.length; ++i) {
			if (i) printf(", ");
			ast_node_print(this, parser_get_extra(this,
						POS_ADV(node.extra_data.pos, i)));
		}
		printf("]");
	break; case AST_BLOCK_SIMPLE:
		printf("[");
		_ast_node_print(this, node.name_data.body, "]");
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
		_ast_node_print(this, node.op_data.lhs, ")");
	break;	case AST_ASSIGN:
		printf("(");
		_ast_node_print(this, node.op_data.lhs, " := ");
		_ast_node_print(this, node.op_data.rhs, ")");
	break;	case AST_LOOP:
		printf("(loop ");
		if (node.name_data.name)
			printf("#%.*s ", UNSLICE(parser_get_name(this,
							node.name_data.name)));
		_ast_node_print(this, node.name_data.body, ")");
	break;	case AST_COND_SIMPLE:
		printf("(if ");
		_ast_node_print(this, node.op_data.lhs, " => ");
		_ast_node_print(this, node.op_data.rhs, ")");
	break;	case AST_COND:
		uint32_t i = 0;
		printf("(if ");
		for (; i + 1 < node.extra_data.length; i += 2) {
			_ast_node_print(this, parser_get_extra(
						this,
						POS_ADV(node.extra_data.pos, i)
						), " => ");
			_ast_node_print(this, parser_get_extra(
						this,
						POS_ADV(node.extra_data.pos, i + 1)
						), " ");
		}
		if (i < node.extra_data.length) {
			printf("else => ");
			ast_node_print(this, parser_get_extra(
						this,
						POS_ADV(node.extra_data.pos, i)
						));
		}
		printf(")");
	break;	default:
		printf("%s(%u) -- PENDING", ast_get_type_str(node), node.type);
	}
	printf(end);
}/* }}} */
#endif // AST_IMPLEMENT

