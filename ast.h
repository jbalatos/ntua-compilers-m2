#pragma once

extern const char*   ast_type_str (ast_node_t node);
extern void          ast_node_print (const parser_t *this, ast_node_pos pos);

#ifdef AST_IMPLEMENT

#define OP(e, s) [AST_    ## e] = s,
#define TK(e, s)
#define LIT(e)   [AST_    ## e] = #e,
#define KW(e, s) [AST_KW_ ## e] = s,
static const char* type_table[] = {
	[AST_ERROR] = "error",
	DANA_TYPES
	DANA_KEYWORDS
};
#undef OP
#undef TK
#undef LIT
#undef KW

inline const char*
ast_type_str (ast_node_t node)
{ return type_table[node.type]; }

void
ast_node_print (const parser_t *this, ast_node_pos pos)
{
	ast_node_t node = parser_get_node(this, pos);

	switch (node.type) {
	case AST_NUMBER:
		printf("%d", node.pl_data.value);
		break;
	case AST_NAME:
		printf("%.*s", UNSLICE(parser_token_val(this,
						parser_get_token(this, node.name_data.tok))));
		break;
	case AST_OPEN_PAREN:
		ast_node_print(this, node.op_data.lhs); printf("(");
		ast_node_print(this, node.op_data.rhs); printf(")");
		break;
	case AST_OPEN_BRACKET:
		ast_node_print(this, node.op_data.lhs); printf("[");
		ast_node_print(this, node.op_data.rhs); printf("]");
		break;
	default:
		if (bp_is_infix(node, NULL) || bp_is_prefix(node, NULL)) {
			printf("(%s ", ast_type_str(node));
			ast_node_print(this, node.op_data.lhs);
			if (node.op_data.rhs.pos) {
				printf(" "); ast_node_print(this, node.op_data.rhs);
			}
			printf(")");
			break;
		}
		printf("(%s)", ast_type_str(node));
		break;
	}
}

#endif // AST_IMPLEMENT
