#pragma once

extern const char*   ast_type_str (ast_node_t node);
extern void          ast_node_print (const parser_t *this, ast_node_pos pos);

#ifdef AST_IMPLEMENT

#define OP(e, s) [AST_    ## e] = s,
#define TK(e, s)
#define LIT(e)   [AST_    ## e] = #e,
#define KW(e, s) [AST_KW_ ## e] = s,
#define KW_EX(e, al, s) LIT(al)
static const char* ast_type_table[] = {
	[AST_ERROR] = "error",
	DANA_TYPES
	DANA_KEYWORDS
};
#undef OP
#undef TK
#undef LIT
#undef KW
#undef KW_EX

inline const char*
ast_type_str (ast_node_t node)
{ return ast_type_table[node.type]; }

void
ast_node_print (const parser_t *this, ast_node_pos pos)
{
	ast_node_t node = parser_get_node(this, pos);
	uint32_t i;

	switch (node.type) {
	case AST_SIMPLE_BLOCK:
		printf("{");
		ast_node_print(this, node.bin_data.lhs); printf("}");
		break;
	case AST_BLOCK:
		printf("{#%u#", node.extra_data.length);
		for (i=0; i<node.extra_data.length; ++i) {
			printf(" ");
			ast_node_print(this, parser_get_extra(this,
						POS_ADV(node.extra_data.pos, i))
					);
		}
		printf("}");
		break;
	case AST_KW_RETURN:
		printf("(return ");
		ast_node_print(this, node.bin_data.lhs); printf(")");
		break;
	case AST_KW_SKIP:
	case AST_KW_EXIT:
		printf("(%s)", ast_type_str(node));
		break;
	case AST_KW_BREAK:
	case AST_KW_CONT:
		printf("(%s", ast_type_str(node));
		if (node.named_data.name.pos)
			printf(" \"%.*s\"", UNSLICE(parser_get_name(this, node)));
		printf(")");
		break;
	case AST_LOOP:
		printf("(loop ");
		if (node.named_data.name.pos)
			printf("\"%.*s\" ", UNSLICE(parser_get_name(this, node)));
		ast_node_print(this, node.named_data.block);
		printf(")");
		break;
	case AST_COND:
		printf("(if [%u]", node.extra_data.length);
		for (i=0; i + 1<node.extra_data.length; i += 2) {
			printf(" ");
			ast_node_print(this, parser_get_extra(this,
						POS_ADV(node.extra_data.pos, i)
						));
			printf(" => ");
			ast_node_print(this, parser_get_extra(this,
						POS_ADV(node.extra_data.pos, i + 1)
						));
		}
		if (i < node.extra_data.length) {
			printf(" else => ");
			ast_node_print(this, parser_get_extra(this,
						POS_ADV(node.extra_data.pos, i)
						));
		}
		printf(")");
		break;
	case AST_NUMBER:
		printf("%d", node.pl_data.value);
		break;
	case AST_NAME:
		printf("%.*s", UNSLICE(parser_get_name(this, node)));
		break;
	BOOLEAN:
		printf(node.type == AST_KW_TRUE ? "TRUE" : "FALSE");
		break;
	case AST_OPEN_PAREN:
		ast_node_print(this, node.bin_data.lhs); printf("(");
		ast_node_print(this, node.bin_data.rhs); printf(")");
		break;
	case AST_OPEN_BRACKET:
		ast_node_print(this, node.bin_data.lhs); printf("[");
		ast_node_print(this, node.bin_data.rhs); printf("]");
		break;
	default:
		if (bp_is_infix(node, NULL) || bp_is_prefix(node, NULL)) {
			printf("(%s ", ast_type_str(node));
			ast_node_print(this, node.bin_data.lhs);
			if (node.bin_data.rhs.pos) {
				printf(" "); ast_node_print(this, node.bin_data.rhs);
			}
			printf(")");
			break;
		}
		printf("(%s - NOT IMPL)", ast_type_str(node));
		break;
	}
}

#endif // AST_IMPLEMENT
