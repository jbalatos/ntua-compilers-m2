#pragma once

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include "util.h"
#include "util/alloc.h"
#include "util/dynamic_array.h"

typedef slice(char) slice_char_t;

typedef struct {
	slice_char_t buffer;
	const alloc_t *alloc;
	const char *fname;
	uint32_t pos;
} lex_reader_t;

#define LEX_SPACES_PER_TAB 4

#define DANA_TYPES					\
	OP(PLUS,		"+")	/* math */	\
	OP(MINUS,		"-")			\
	OP(MULT,		"*")			\
	OP(DIV,			"/")			\
	OP(MOD,			"%")			\
	OP(NOT,			"!")	/* bitwise */	\
	OP(AND,			"&")			\
	OP(OR,			"|")			\
	OP(EQ,			"=")	/* compare */	\
	OP(NEQ,			"<>")			\
	OP(LT,			"<")			\
	OP(LEQ,			"<=")			\
	OP(GT,			">")			\
	OP(GEQ,			">=")			\
	TK(OPEN_PAREN,		"(")	/* dividers */	\
	TK(CLOSE_PAREN,		")")			\
	TK(OPEN_BRACKET,	"[")			\
	TK(CLOSE_BRACKET,	"]")			\
	TK(COMMA,		",")			\
	TK(COLON,		":")			\
	TK(ASSIGN,		":=")			\
	LIT(OPEN_INDENT)				\
	LIT(CLOSE_INDENT)				\
	LIT(NAME)					\
	LIT(NUMBER)					\
	LIT(CHAR)					\
	LIT(STRING)					\
	LIT(EOF)

#define DANA_KEYWORDS				\
	KW(AND,		"and")			\
	KW(AS,		"as")			\
	KW(BEGIN,	"begin")		\
	KW(BREAK,	"break")		\
	KW(BYTE,	"byte")			\
	KW(CONT,	"continue")		\
	KW(DECL,	"decl")			\
	KW(DEF,		"def")			\
	KW(ELIF,	"elif")			\
	KW(ELSE,	"else")			\
	KW(END,		"end")			\
	KW(EXIT,	"exit")			\
	KW(FALSE,	"false")		\
	KW(IF,		"if")			\
	KW(IS,		"is")			\
	KW(INT,		"int")			\
	KW(LOOP,	"loop")			\
	KW(NOT,		"not")			\
	KW(OR,		"or")			\
	KW(REF,		"ref")			\
	KW(RETURN,	"return")		\
	KW(SKIP,	"skip")			\
	KW(TRUE,	"true")			\
	KW(VAR,		"var")			\

typedef struct { int pos; } lex_bufpos_t;

#define OP(e, s) DANA_    ## e,
#define TK(e, s) DANA_    ## e,
#define LIT(e)   DANA_    ## e,
#define KW(e, s) DANA_KW_ ## e,
typedef struct {
	lex_bufpos_t pos;
	union {
		int32_t pl_num;
		uint64_t pl_hash;
		uint32_t pl_len;
	};
	enum __attribute__((__packed__)) lex_ttype {
		DANA_ERROR = 0,
		DANA_TYPES
		DANA_KEYWORDS
	} type;
} lex_token_t;
#undef OP
#undef TK
#undef LIT
#undef KW

#define LEX_READER_CLEANUP lex_reader_t \
	__attribute__((cleanup(lex_reader_destroy)))
lex_reader_t lex_reader_create (const alloc_t *alloc, const char *fname);
void         lex_reader_destroy (lex_reader_t *this);

slice_char_t lex_string_at (lex_reader_t *this, lex_bufpos_t pos, uint32_t len);
const char*  lex_token_type (lex_token_t tok);
lex_token_t  lex_next_token (lex_reader_t *this);

#ifdef LEX_IMPLEMENT
/** constructor - destructor */
/* {{{ */
lex_reader_t
lex_reader_create (const alloc_t *alloc, const char *fname)
{
	lex_reader_t ret = { .alloc = alloc, .fname = fname };
	FILE *fp = fopen(fname, "r");

	if (fp) {
		fseek(fp, 0, SEEK_END);
		set_slice(ret.buffer, allocate(*alloc, char, ftell(fp)));
		rewind(fp);
		fread(ret.buffer.ptr, 1, ret.buffer.length, fp);
		fclose(fp);
	}

	return ret;
}

void
lex_reader_destroy (lex_reader_t *this)
{
	if (this->buffer.ptr) deallocate(*this->alloc, this->buffer);
}
/* }}} */

/** cleanup trick to turn slices into c-strings */
/* {{{ */
typedef struct { char *ptr; char backup; } lex_temp_t;
static void lex_temp_cleanup (lex_temp_t *this) { *this->ptr = this->backup; }

#define LEX_TEMP_SLICE(sl)                                            \
	lex_temp_t __attribute__((cleanup(lex_temp_cleanup)))         \
		__LEX__ ## __LINE__ = {                               \
			(sl).ptr + (sl).length, (sl).ptr[(sl).length] \
		};                                                    \
	(sl).ptr[(sl).length] = '\0'
/* }}} */

/** private methods on lex_reader_t */
/* {{{ */
static inline bool
lex_eof (lex_reader_t *this)
{ return this->pos == this->buffer.length; }

static inline lex_bufpos_t
lex_pos (lex_reader_t *this)
{ return (lex_bufpos_t){ this->pos }; }

static inline void
lex_rewind (lex_reader_t *this, lex_bufpos_t pos)
{ this->pos = pos.pos; }

static char
lex_front (lex_reader_t *this)
{ return lex_eof(this) ? 0 : this->buffer.ptr[this->pos]; }

static char
lex_peek (lex_reader_t *this, uint32_t offset)
{
	return this->pos + offset < this->buffer.length ?
		this->buffer.ptr[this->pos + offset] : 0;
}

static slice_char_t
lex_peekn (lex_reader_t *this, uint32_t offset, uint32_t length)
{
	if (this->pos + offset + length > this->buffer.length)
		return (slice_char_t){};
	return (slice_char_t) { this->buffer.ptr + this->pos + offset, length };
}

static inline char
lex_pop (lex_reader_t *this)
{ return lex_eof(this) ? 0 : this->buffer.ptr[this->pos++]; }

static slice_char_t
lex_popn (lex_reader_t *this, uint32_t length)
{
	if (this->pos + length > this->buffer.length)
		return (slice_char_t){};
	slice_char_t ret = { this->buffer.ptr + this->pos, length };
	this->pos += length;
	return ret;
}

static char
lex_pop_if (lex_reader_t *this, int (*check_fn)(int))
{
	if (check_fn(this->buffer.ptr[this->pos]))
		return this->buffer.ptr[this->pos++];
	else
		return 0;
}
/* }}} */

/** symbol table */
/* {{{ */
static struct { const char *key; enum lex_ttype value; } *symbol_table;

#define OP(e, s) [DANA_ ## e] = s,
#define TK(e, s) [DANA_ ## e] = s,
#define LIT(e)   [DANA_ ## e] = #e,
#define KW(e, s) [DANA_KW_ ## e] = s,
static const char* symbol_arr[] = {
	[DANA_ERROR] = "ERROR",
	DANA_TYPES
	DANA_KEYWORDS
};
#undef OP
#undef TK
#undef LIT
#undef KW

static void __attribute__((constructor))
create_symbol_table (void)
{
	size_t table_size = sizeof(symbol_arr) / sizeof(symbol_arr[0]);
	for (size_t i=0; i<table_size; ++i) if (symbol_arr[i])
		hm_put(symbol_table, symbol_arr[i], i);
}

static void __attribute__((destructor))
destroy_symbol_table (void)
{
	hm_free(symbol_table);
}

const char*
lex_token_type (lex_token_t tok)
{ return symbol_arr[tok.type]; }
/* }}} */

/** indentation stack */
/* {{{ */
static uint16_t *indent_stack = {0};

static void __attribute__((constructor))
create_indent_stack (void)
{ arr_push(indent_stack, 0); }

static void __attribute__((destructor))
destroy_indent_stack (void)
{ arr_free(indent_stack); }
/* }}} */

/** tokenizer */
/* {{{ */
slice_char_t
lex_string_at (lex_reader_t *this, lex_bufpos_t pos, uint32_t len)
{
	return (slice_char_t){ this->buffer.ptr + pos.pos, len };
}

static int
isiden (int ch)
{ return isalpha(ch) || isdigit(ch) || ch == '_'; }

static enum {
	NO_COMMENT = 0,
	ONELINE_COMMENT,
	COMMENT_BEGIN,
	COMMENT_END,
} iscomment (lex_reader_t *this, lex_bufpos_t pos)
{
	char lhs, rhs;

	if (lex_eof(this))
		return NO_COMMENT;
	if ((lhs = this->buffer.ptr[pos.pos]) == '#')
		return ONELINE_COMMENT;
	if (pos.pos + 1 >= this->buffer.length)
		return NO_COMMENT;
	rhs = this->buffer.ptr[pos.pos + 1];
	if (lhs == '(' && rhs == '*') return COMMENT_BEGIN;
	if (lhs == '*' && rhs == ')') return COMMENT_END;
	return NO_COMMENT;
}

static void
lex_consume_comment (lex_reader_t *this)
{
	switch (iscomment(this, lex_pos(this))) {
		case NO_COMMENT     : return;
		case COMMENT_END    : assert(!"Comment ending before it begins");
		case COMMENT_BEGIN  : lex_popn(this, 2); break;
		case ONELINE_COMMENT: while (!lex_eof(this) && lex_pop(this) != '\n'); return;
	}
	while (!lex_eof(this)) switch (iscomment(this, lex_pos(this))) {
		case ONELINE_COMMENT:
		case COMMENT_BEGIN  : lex_consume_comment(this); break;
		case COMMENT_END    : lex_popn(this, 2);         return;
		case NO_COMMENT     : lex_popn(this, 1);         break;
	}
	assert(!"Comment never ends");
}

static void
lex_consume_whitespace (lex_reader_t *this)
{
	for (; !lex_eof(this) && isspace(lex_front(this)); lex_pop(this)) {
	}
}

lex_token_t
lex_next_token (lex_reader_t *this)
{
	uint32_t length;
	slice_char_t sl;
	lex_bufpos_t init_pos;

	lex_consume_whitespace(this);
	if (lex_eof(this)) return (lex_token_t){ .type = DANA_EOF };

	init_pos = lex_pos(this);

	if (iscomment(this, init_pos)) {
		lex_consume_comment(this);
		return lex_next_token(this);
	}

	if (isdigit(lex_front(this)) || (lex_front(this) == '-'
				&& isdigit(lex_peek(this, 1)))) {
		for (length = 2; isdigit(lex_peek(this, length)); ++length);
		set_slice(sl, lex_popn(this, length));
		LEX_TEMP_SLICE(sl);
		return (lex_token_t){
			.type = DANA_NUMBER,
			.pos = init_pos, .pl_num = atoi(sl.ptr),
		};
	}

	if (lex_front(this) == '"' || lex_front(this) == '\'') {
		for (length = 1; lex_peek(this, length) &&
				lex_peek(this, length) != lex_front(this); ++length);
		assert(lex_peek(this, length) && "Non-finishing string");
		lex_popn(this, length + 1);
		return (lex_token_t){
			.type = lex_front(this) == '"' ? DANA_STRING : DANA_CHAR,
			.pos = { init_pos.pos + 1 }, .pl_len = length - 1,
		};
	}

	if (isalpha(lex_front(this)) || lex_front(this) == '_') {
		for (length = 1; isiden(lex_peek(this, length)); ++length);
		set_slice(sl, lex_popn(this, length));
		LEX_TEMP_SLICE(sl);
		if (hm_geti(symbol_table, sl.ptr) != -1)
			return (lex_token_t) {
				.type = hm_get(symbol_table, sl.ptr),
				.pos = init_pos, .pl_hash = 0,
			};
		else
			return (lex_token_t) {
				.type = DANA_NAME,
				.pos = init_pos, .pl_len = length,
			};
	}

	{
		set_slice(sl, lex_peekn(this, 0, 2));
		LEX_TEMP_SLICE(sl);
		if (hm_geti(symbol_table, sl.ptr) != -1) {
			lex_popn(this, 2);
			return (lex_token_t){
				.type = hm_get(symbol_table, sl.ptr),
				.pos = init_pos,
			};
		}
	}
	{
		set_slice(sl, lex_peekn(this, 0, 1));
		LEX_TEMP_SLICE(sl);
		if (hm_geti(symbol_table, sl.ptr) != -1) {
			lex_popn(this, 2);
			return (lex_token_t){
				.type = hm_get(symbol_table, sl.ptr),
				.pos = init_pos,
			};
		}
	}

	this->pos = this->buffer.length;
	return (lex_token_t){};
}
/* }}} */
#endif // LEX_IMPLEMENT
