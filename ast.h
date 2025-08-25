#pragma once

#include "symbols.h"

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

typedef union ast_pl_data ast_pl_data;
typedef struct ast_op_data ast_op_data;
typedef struct ast_name_data ast_name_data;
typedef struct ast_extra_data ast_extra_data;
typedef struct ast_var_data ast_var_data;

POS_DECL(ast_node_pos, 24);
POS_DECL(extra_pos, 24);
POS_DECL(text_pos, 32);

typedef struct {
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
} ast_node_t;

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
#undef KW_PAR
/* }}} */

inline const char*
ast_get_type_str (ast_node_t node)
{ return ast_symbol_arr[node.type]; }

#endif // AST_IMPLEMENT

