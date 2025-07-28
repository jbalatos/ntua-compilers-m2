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
	lex_token_t tok = lex_next_token(&lexer, NULL);

	for (; tok.type != DANA_EOF; tok = lex_next_token(&lexer, &tok)) {
		printf("%3u: %10s\t\t%20.*s\t\t(%u @ %p)\n",
				tok.pos, lex_token_type(tok),
				UNSLICE(lex_token_val(&lexer, tok)),
				UNSLICE(lex_token_val(&lexer, tok)));
	}

	return 0;
}
