#include <errno.h>
#include <stdio.h>

#define ALLOC_IMPLEMENT
#include "util/alloc.h"
#define LEX_IMPLEMENT
#define DA_IMPLEMENT
#include "lexer.h"

const char fname[] = "sample.dana";
#define LENGTH(x) (sizeof(x)/sizeof(x[0]))

int main (int argc, char *argv[argc])
{

	LEX_READER_CLEANUP lexer = lex_reader_create(&LIBC, fname);
	lex_token_t tok;

	while ((tok = lex_next_token(&lexer)).type != DANA_EOF) {
		printf("%3u: %10s\t\t", tok.pos.pos, lex_token_type(tok));
		switch (tok.type) {
		case DANA_NUMBER: printf("%d\n", tok.pl_num);
				  break;
		case DANA_NAME: printf("%.*s\n", untie_slice(lex_string_at(
								  &lexer, tok.pos, tok.pl_len
								  )));
				  break;
		case DANA_CHAR:
		case DANA_STRING: printf("\"%.*s\"\n", untie_slice(lex_string_at(
								  &lexer, tok.pos, tok.pl_len
								  )));
				  break;
		default: printf("\n");
		}
	}

	return 0;
}
