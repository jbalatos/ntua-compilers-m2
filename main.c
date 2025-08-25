#include <errno.h>
#include <stdio.h>

#define ALLOC_IMPLEMENT
#include "util/alloc.h"

#define PARSER_IMPLEMENT
#include "parser.h"

#define SEM_DEBUG
#define SEM_IMPLEMENT
#include "semantic.h"

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

	parser_t PARSE_CLEANUP parser = parser_create(lexer_create(&LIBC, fname));
	ast_node_pos root = parse(&parser);

	printf("\n=== AST ===\n");
	parser_print_node(&parser, root);
	printf("\n");

	printf("\n=== AST ARRAY ===\n");
	for (size_t i=0; i<arr_ulen(parser.ast); ++i)
		printf("%3lu :\t %s\n", i, ast_get_type_str(parser.ast[i]));
	printf("\n");

	printf("\n=== NAME TABLE ===\n");
	for (size_t i=0; i<arr_ucap(parser.names); ++i) {
		hm_cell_t it = arr_header(parser.names)->hash_table[i];
		if (it.hash == 0) continue;
		printf("%20lu\t%.*s\n", parser.names[it.index].key,
				UNSLICE(parser_get_value_by_pos(&parser,
						parser.names[it.index].value))
				);
	}
	printf("\n");

	printf("\n=== SEMANTICS ===\n");
	symbol_table_t ST_CLEANUP st = st_create();
	sem_error_t sem = sem_check(&parser, &st, root);
	printf("\n=== Result:\t\t%s ===\n", sem.msg);

	return 0;
}
