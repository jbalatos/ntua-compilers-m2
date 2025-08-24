#include <errno.h>
#include <stdio.h>

#define ALLOC_IMPLEMENT
#include "util/alloc.h"

#define BP_IMPLEMENT
#define DA_IMPLEMENT
#define LEX_IMPLEMENT
#define AST_IMPLEMENT
#define PARSER_IMPLEMENT
#include "parser.h"

const char fname[] = "sample.dana";
#define LENGTH(x) (sizeof(x)/sizeof(x[0]))

int main (int argc, char *argv[argc])
{

	printf("---DEBUG INFO (%lu):---\n", sizeof(uint8_t));
	printf("\tLEXER:\r");
	printf("\t\t lexer_t size: %lu\n", sizeof(lexer_t));
	printf("\t\t lex_token_t size: %lu\n", sizeof(lex_token_t));
	printf("\t\t lex_buf_pos size: %lu\n", sizeof(lex_buf_pos));
	printf("\t\t lex_types length: %u\n", LEX_TYPES_LEN);
	printf("\tPARER:\r");
	printf("\t\t parser_t size: %lu\n", sizeof(parser_t));
	printf("\t\t ast_node_t size: %lu\n", sizeof(ast_node_t));
	printf("\t\t ast_ttype size: %lu\n", sizeof(enum ast_type));
	printf("\t\t ast_ttype length: %u\n", AST_TYPES_LEN);
	printf("---DEBUG END---\n\n");

	// LEXER_CLEANUP lexer = lexer_create(&LIBC, fname);
	// lex_token_t tok = lex_next_token(&lexer, NULL);
	// for (; tok.type != DANA_EOF; tok = lex_next_token(&lexer, &tok)) {
		// printf("%3u: %10s\t\t%20.*s\t\t(%u @ %p)\n",
				// tok.pos.pos, lex_get_type_str(tok),
				// UNSLICE(lex_get_token(&lexer, tok)),
				// UNSLICE(lex_get_token(&lexer, tok)));
	// }

	PARSER_CLEANUP parser = parser_create(lexer_create(&LIBC, fname));
	ast_node_pos root = parse(&parser);
	ast_node_print(&parser, root);

	return 0;
}
