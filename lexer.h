#pragma once

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include "util.h"
#include "util/alloc.h"
#include "util/dynamic_array.h"

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

typedef slice(char) slice_char_t;

typedef struct {
	slice_char_t buffer;
	const alloc_t *alloc;
	const char *fname;
} lex_reader_t;

#define OP(e, s) DANA_    ## e,
#define TK(e, s) DANA_    ## e,
#define LIT(e)   DANA_    ## e,
#define KW(e, s) DANA_KW_ ## e,
typedef struct {
	enum lex_ttype {
		DANA_ERROR = 0,
		DANA_COMMENT,
		DANA_TYPES
		DANA_KEYWORDS
	} type : 8;
	uint32_t pos : 24;
} lex_token_t;
#undef OP
#undef TK
#undef LIT
#undef KW

/* lex_reader_t init */
#define LEX_READER_CLEANUP lex_reader_t \
	__attribute__((cleanup(lex_reader_destroy)))
extern lex_reader_t   lex_reader_create (const alloc_t *alloc, const char *fname);
extern void           lex_reader_destroy (const lex_reader_t *this);
/* lex_reader_t methods */
static bool           lex_eof (const lex_reader_t *this, uint32_t pos);
static char           lex_read_char (const lex_reader_t *this, uint32_t pos);
static slice_char_t   lex_read_str (const lex_reader_t *this, uint32_t pos, uint32_t length);
static bool           lex_matches (const lex_reader_t *this, uint32_t pos, const char *str);
/* symbol table */
extern const char*    lex_ttype_str (lex_token_t tok);
static enum lex_ttype lex_scan_sym_table (const lex_reader_t *this, uint32_t pos, uint32_t len);
/* tokenizer */
extern lex_token_t    lex_next_token (const lex_reader_t *this, const lex_token_t *prev);
extern slice_char_t   lex_get_token (const lex_reader_t *this, lex_token_t tok);
extern slice_char_t   lex_token_val (const lex_reader_t *this, lex_token_t tok);
static int            isiden (int ch);
static int            ishex (int ch);
static uint32_t       lex_skip_spaces (const lex_reader_t *this, uint32_t pos);
static uint32_t       lex_scan_char (const lex_reader_t *this, uint32_t pos);
static lex_token_t    lex_token_at (const lex_reader_t *this, uint32_t pos);
static uint32_t       lex_token_end (const lex_reader_t *this, lex_token_t tok); /* exclusive */

#ifdef LEX_IMPLEMENT
/** lex_reader_t constructor - destructor */
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
lex_reader_destroy (const lex_reader_t *this)
{
	if (this->buffer.ptr) deallocate(*this->alloc, this->buffer);
}
/* }}} */

/** lex_reader_t methods */
/* {{{ */
static inline bool
lex_eof (const lex_reader_t *this, uint32_t pos)
{ return pos >= this->buffer.length; }
static inline char
lex_read_char (const lex_reader_t *this, uint32_t pos)
{ return lex_eof(this, pos) ? 0 : this->buffer.ptr[pos]; }

static inline slice_char_t
lex_read_str (const lex_reader_t *this, uint32_t pos, uint32_t length)
{
	return lex_eof(this, pos + length - 1)
		? (slice_char_t){}
		: (slice_char_t){ this->buffer.ptr + pos, length };
}

static bool
lex_matches (const lex_reader_t *this, uint32_t pos, const char *str)
{
	uint32_t len = strlen(str);
	slice_char_t sl = lex_read_str(this, pos, len);
	if (sl.ptr) return !strncmp(sl.ptr, str, len);
	else return false;
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

/** symbol table */
/* {{{ */
static struct { const char *key; enum lex_ttype value; } *symbol_table;

#define OP(e, s) [DANA_ ## e] = s,
#define TK(e, s) [DANA_ ## e] = s,
#define LIT(e)   [DANA_ ## e] = #e,
#define KW(e, s) [DANA_KW_ ## e] = s,
static const char* symbol_arr[] = {
	[DANA_ERROR] = "",
	[DANA_COMMENT] = "",
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

static enum lex_ttype
lex_scan_sym_table (const lex_reader_t *this, uint32_t pos, uint32_t len)
{
	slice_char_t sl = lex_read_str(this, pos, len);
	if (!sl.ptr) return DANA_ERROR;
	LEX_TEMP_SLICE(sl);
	return hm_get(symbol_table, sl.ptr);
}
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
static int
isiden (int ch)
{ return isalpha(ch) || isdigit(ch) || ch == '_'; }

static int
ishex (int ch)
{ return isdigit(ch) || ('a' <= ch && ch <= 'f'); }

static uint32_t
lex_skip_spaces (const lex_reader_t *this, uint32_t pos)
{
	for (; isspace(lex_read_char(this, pos)); ++pos);
	return pos;
}

static uint32_t
lex_scan_char (const lex_reader_t *this, uint32_t pos)
{
	uint32_t closing = pos + 2;

	if (lex_read_char(this, pos + 1) == '\'') return pos + 2;
	if (lex_read_char(this, pos + 1) == '\\')
		switch (lex_read_char(this, pos + 2)) {
		case 'n':
		case 't':
		case 'r':
		case '\\':
		case '\'':
		case '"':
			closing = pos + 3; break;
		case 'x':
			if (ishex(lex_read_char(this, pos + 3)) &&
					ishex(lex_read_char(this, pos + 4)))
				closing = pos + 5;
		}
	return lex_read_char(this, closing) == '\'' ? closing + 1 : 0;
}

static lex_token_t
lex_token_at (const lex_reader_t *this, uint32_t pos)
{
	uint32_t end;
	enum lex_ttype kword;

	if (lex_matches(this, pos, "(*"))
		return (lex_token_t){ DANA_COMMENT, pos };

	switch (lex_read_char(this, pos)) {
	case '#':
		return (lex_token_t){ DANA_COMMENT, pos };
	case '-':
	case '0' ... '9':
		return (lex_token_t){ DANA_NUMBER, pos };
	case '"':
		return (lex_token_t){ DANA_STRING, pos };
	case '\'':
		return (lex_token_t){ DANA_CHAR, pos };
	case '_':
	case 'a' ... 'z':
	case 'A' ... 'Z':
		for (end=pos + 1; isiden(lex_read_char(this, end)); ++end);
		kword = lex_scan_sym_table(this, pos, end - pos);
		if (kword) return (lex_token_t){ kword, pos };
		else       return (lex_token_t){ DANA_NAME, pos };
	default: /* operator */
		if ((kword = lex_scan_sym_table(this, pos, 2)))
			return (lex_token_t){ kword, pos };
		if ((kword = lex_scan_sym_table(this, pos, 1)))
			return (lex_token_t){ kword, pos };
	}
	return (lex_token_t){ DANA_ERROR, pos };
}

static uint32_t
lex_token_end (const lex_reader_t *this, lex_token_t tok)
{
	uint32_t end = tok.pos;

	switch (tok.type) {
	case DANA_COMMENT:
		if (lex_read_char(this, tok.pos) == '#') {
			for (++end; lex_read_char(this, end) != '\n'; ++end);
			return end;
		} else {
			for (end += 2; lex_read_char(this, end+1) &&
					!lex_matches(this, end,"*)");
					++end)
				if (lex_matches(this, end,"(*"))
					end = lex_token_end(this, (lex_token_t){
							DANA_COMMENT, end
							});
			if (lex_matches(this, end,"*)"))
				return end + 2;
			tok.type = DANA_ERROR;
			return 0;
		}
	case DANA_NUMBER:
		for (++end; isdigit(lex_read_char(this, end)); ++end);
		return end;
	case DANA_STRING:
		for (++end; lex_read_char(this, end) != '"'; ++end)
			if (lex_matches(this, end, "\\\"")) ++end;
		return end + 1;
	case DANA_CHAR:
		return lex_scan_char(this, tok.pos);
	case DANA_NAME:
	case DANA_KW_AND ... DANA_KW_VAR:
		for (++end; isiden(lex_read_char(this, end)); ++end);
		return end;
	default: /* operator */
		return end + strlen(lex_ttype_str(tok));
	}
}

inline const char*
lex_ttype_str (lex_token_t tok)
{ return symbol_arr[tok.type]; }

inline slice_char_t
lex_token_val (const lex_reader_t *this, lex_token_t tok)
{ return lex_read_str(this, tok.pos, lex_token_end(this, tok) - tok.pos); }

lex_token_t
lex_next_token (const lex_reader_t *this, const lex_token_t *prev)
{
	uint32_t start;
	lex_token_t ret;

	start = prev ? lex_token_end(this, *prev) : 0;
	start = lex_skip_spaces(this, start);
	if (lex_eof(this, start))
		return (lex_token_t){ DANA_EOF };

	ret = lex_token_at(this, start);
	if (ret.type == DANA_COMMENT)
		return lex_next_token(this, &ret);
	return ret;
}
/* }}} */
#endif // LEX_IMPLEMENT
