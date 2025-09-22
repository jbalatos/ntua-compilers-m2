#include <errno.h>
#include <stdio.h>

#define ALLOC_IMPLEMENT
#include "util/alloc.h"

#define PARSER_IMPLEMENT
#include "parser.h"

//#define CGEN_IMPLEMENT
//#include "codegen.h"

const char fname[] = "etc/sample.dana";
#define LENGTH(x) (sizeof(x)/sizeof(x[0]))

int main (int argc, char *argv[argc])
{
	--argc, ++argv;

	printf("---DEBUG INFO (%lu):---\n", sizeof(uint8_t));
	printf("\tLEXER:\r");
	printf("\t\t lexer_t size: %lu\n", sizeof(lexer_t));
	printf("\t\t lex_token_t size: %lu\n", sizeof(lex_token_t));
	printf("\t\t lex_buf_pos size: %lu\n", sizeof(lex_buf_pos));
	printf("\t\t lex_types length: %u\n", LEX_TYPE_LEN);
	printf("\tPARER:\r");
	printf("\t\t parser_t size: %lu\n", sizeof(parser_t));
	printf("\t\t ast_node_t size: %lu\n", sizeof(ast_node_t));
	printf("\t\t ast_ttype size: %lu\n", sizeof(enum ast_type));
	printf("\t\t ast_ttype length: %u\n", AST_TYPE_LEN);
	printf("---DEBUG END---\n\n");

	// lexer_t LEX_CLEANUP lexer = lexer_create(&LIBC, fname);
	// lex_token_t tok = lex_next_token(&lexer, NULL);
	// for (; tok.type != DANA_EOF; tok = lex_next_token(&lexer, &tok)) {
		// printf("%3u: %10s\t\t%20.*s\t\t(%u @ %p)\n",
				// tok.pos.pos, lex_get_type_str(tok),
				// UNSLICE(lex_get_token(&lexer, tok)),
				// UNSLICE(lex_get_token(&lexer, tok)));
	// }

	parser_t PARSER_CLEANUP parser = parser_create(
			lexer_create(&LIBC, argc ? *argv : fname)
			);
	ast_node_pos root = parse(&parser);
	if (!POS_OK(root)) return 1;

	printf("\n=== AST ARRAY ===\n");	
	uint16_t arr_len;
	for (size_t i=0; i<arr_ulen(parser.nodes); ++i)
		switch (parser.nodes[i].type){
		case AST_TRUE ... AST_FALSE:
			printf("%3lu :\t %10s\t\t\t(1)\n", i,
					ast_get_type_str(parser.nodes[i]));
			break;
		case AST_NUMBER:
			printf("%3lu :\t %10s\t%8d\t(1)\n", i,
					ast_get_type_str(parser.nodes[i]),
					parser.nodes[i].pl_data.num);
			break;
		case AST_STRING:
			printf("%3lu :\t %10s\t%10s\t(1)\n", i,
					ast_get_type_str(parser.nodes[i]),
					par_get_text(&parser, parser.nodes[i].pl_data.str));
			break;
		case AST_NAME:
		case AST_ARRAY_AT:
		case AST_FUNC_CALL:
		case AST_PROC_CALL:
		case AST_BREAK:
		case AST_CONT:
		case AST_LOOP:
		case AST_INT ... AST_BYTE:
		case AST_REF_INT ... AST_REF_BYTE:
		case AST_DECL_PROC ... AST_DECL_BYTE:
			printf("%3lu :\t %10s\t%8.*s (%u)\t(%u)\n", i,
					ast_get_type_str(parser.nodes[i]),
					UNSLICE(par_get_name(
							&parser, parser.nodes[i]
							)),
					parser.nodes[i].name,
					parser.nodes[i].length);
			break;
		case AST_DEF_PROC ... AST_DEF_BYTE:
			printf("%3lu :\t %10s\t%8.*s (%u)\t(%u) (locals at +%u, body at +%u)\n", i,
					ast_get_type_str(parser.nodes[i]),
					UNSLICE(par_get_name(
							&parser, parser.nodes[i]
							)),
					parser.nodes[i].name,
					parser.nodes[i].length,
					parser.nodes[i].def_data.local_off,
					parser.nodes[i].def_data.body_off);
			break;
		case AST_ARRAY:
			printf("%3lu :\t %10s\t%8.*s (%u)\t(%u)\n", i,
					ast_get_type_str(parser.nodes[i]),
					UNSLICE(par_get_name(
							&parser, parser.nodes[i]
							)),
					parser.nodes[i].name,
					parser.nodes[i].length);
			dtype_t it = par_get_type(&parser, parser.nodes[i].var_data.array);
			printf("\t->\t%s", it.type & DTYPE_BYTE ? "byte" : "int");
			for (dtype_pos j = parser.nodes[i].var_data.array;
					POS_DIFF(parser.nodes[i].var_data.array, j) < it.length;
					j = POS_ADV(j, 1))
				if ((arr_len = par_get_type(&parser, j).array_length))
					printf("[%u]", arr_len);
				else
					printf("[]");
			printf("\n");
			break;
		default:
			printf("%3lu :\t %10s\t\t\t(%u)\n", i,
					ast_get_type_str(parser.nodes[i]),
					parser.nodes[i].length);
		}
	printf("\n");

	printf("\n=== LINE MAP ===\n");
	for (size_t i=0; i<hm_ulen(parser.lexer.lines); ++i)
		printf("Line %4lu\t|%6u\n", i,
				parser.lexer.lines[i].pos);
	printf("\n");

	printf("\n=== AST PRETTY PRINT ===\n");
	par_print(&parser, root);
	printf("\n");

	printf("\n=== NAME TABLE ===\n");
	for (size_t i=0; i<arr_ucap(parser.names); ++i) {
		hm_cell_t it = arr_header(parser.names)->hash_table[i];
		if (it.hash == 0) continue;
		printf("%4u\t%.*s\n", parser.names[it.index].value,
				UNSLICE(parser.names[it.index].decl));
	}
	printf("\n");

	/*printf("\n=== CODEGEN ===\n");
	cgen_t CGEN_CLEANUP cgen;
	cgen_create(&cgen);
	cgen_generate_code(&cgen, &parser, root);*/

	return 0;
}
