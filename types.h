#pragma once

#include "util.h"

typedef slice(char) slice_char_t;
POS_DECL(lex_buf_pos, 24);
POS_DECL(lex_token_pos, 24);
POS_DECL(ast_node_pos, 24);
POS_DECL(extra_pos, 24);
POS_DECL(dtype_pos, 24);
POS_DECL(func_pos, 24);
POS_DECL(text_pos, 32);

typedef struct lexer_t lexer_t;
typedef struct lex_token_t lex_token_t;

typedef struct parser_t parser_t;
typedef struct parser_bp_t parser_bp_t;

typedef struct ast_node_t ast_node_t;

typedef union ast_pl_data ast_pl_data;
typedef struct ast_op_data ast_op_data;
typedef struct ast_name_data ast_name_data;
typedef struct ast_extra_data ast_extra_data;
typedef struct ast_var_data ast_var_data;
typedef struct ast_func_data ast_func_data;
