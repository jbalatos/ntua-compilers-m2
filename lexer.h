#pragma once

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include "symbols.h"
#include "types.h"
#include "util.h"
#include "util/alloc.h"
#include "util/dynamic_array.h"

#define LEX_SPACES_PER_TAB 4

struct lexer_t {
	slice_char_t buffer;
	const alloc_t *alloc;
	const char *fname;
};

#define OP_LEX(l, s) DANA_    ## l,
#define TK_LEX(l, s) DANA_    ## l,
#define LT_LEX(l, s) DANA_    ## l,
#define KW_LEX(l, s) DANA_KW_ ## l,
#define OP(l, p, s) OP_LEX(l, s)
#define TK(l, p, s) TK_LEX(l, s)
#define LT(l, p, s) LT_LEX(l, s)
#define KW(l, p, s) KW_LEX(l, s)
/* just for alignment between lex_type and ast_type */
#define LT_PAR(p, s) DANA_UNUSED_ ## p,
#define KW_PAR(p, s) DANA_UNUSED_KW_ ## p,

struct lex_token_t {
	enum lex_type {
		DANA_ERROR = 0,
		DANA_TYPES
		DANA_KEYWORDS
		DANA_COMMENT,
		LEX_TYPES_LEN,
	} type : 8;
	lex_buf_pos pos;
};

#undef OP_LEX
#undef TK_LEX
#undef LT_LEX
#undef KW_LEX
#undef OP
#undef TK
#undef LT
#undef KW
#undef LT_PAR
#undef KW_PAR

/* lexer_t init */
#define LEXER_CLEANUP lexer_t \
	__attribute__((cleanup(lexer_destroy)))
extern lexer_t         lexer_create (const alloc_t *alloc, const char *fname);
extern void            lexer_destroy (const lexer_t *this);
/* lexer_t methods */
extern lex_token_t     lex_next_token (const lexer_t *this, const lex_token_t *prev);
extern slice_char_t    lex_get_token (const lexer_t *this, lex_token_t tok);
/* symbol table */
extern const char*     lex_get_type_str (lex_token_t tok);

#ifdef LEX_IMPLEMENT
/** private declarations */
bool           _lex_eof (const lexer_t *this, lex_buf_pos pos);
char           _lex_read_char (const lexer_t *this, lex_buf_pos pos);
slice_char_t   _lex_read_str (const lexer_t *this, lex_buf_pos pos, uint32_t length);
bool           _lex_matches (const lexer_t *this, lex_buf_pos pos, const char *str);
enum lex_type  _lex_scan_sym_table (const lexer_t *this, lex_buf_pos pos, uint32_t len);
int             isiden (int ch);
int             ishex (int ch);
lex_buf_pos    _lex_skip_spaces (const lexer_t *this, lex_buf_pos pos);
lex_buf_pos    _lex_scan_char (const lexer_t *this, lex_buf_pos pos);
lex_token_t    _lex_token_at (const lexer_t *this, lex_buf_pos pos);
lex_buf_pos    _lex_token_end (const lexer_t *this, lex_token_t tok); /* exclusive */

/** lexer_t constructor - destructor */
/* {{{ */
lexer_t
lexer_create (const alloc_t *alloc, const char *fname)
{
	lexer_t ret = { .alloc = alloc, .fname = fname };
	FILE *fp = fopen(fname, "r");

	if (fp) {
		fseek(fp, 0, SEEK_END);
		set_slice(ret.buffer, allocate(*alloc, char, ftell(fp) + 1));
		ret.buffer.length -= 1;
		rewind(fp);
		fread(ret.buffer.ptr, 1, ret.buffer.length, fp);
		ret.buffer.ptr[ret.buffer.length] = '\0';
		fclose(fp);
	}

	return ret;
}

void
lexer_destroy (const lexer_t *this)
{
	if (this->buffer.ptr) deallocate(*this->alloc, this->buffer);
}
/* }}} */

/** lexer_t methods */
/* {{{ */
inline bool
_lex_eof (const lexer_t *this, lex_buf_pos pos)
{ return pos.pos >= this->buffer.length; }

inline char
_lex_read_char (const lexer_t *this, lex_buf_pos pos)
{ return _lex_eof(this, pos) ? 0 : this->buffer.ptr[pos.pos]; }

inline slice_char_t
_lex_read_str (const lexer_t *this, lex_buf_pos pos, uint32_t length)
{
	return _lex_eof(this, POS_ADV(pos, length - 1))
		? (slice_char_t){}
		: (slice_char_t){ this->buffer.ptr + pos.pos, length };
}

bool
_lex_matches (const lexer_t *this, lex_buf_pos pos, const char *str)
{
	uint32_t len = strlen(str);
	slice_char_t sl = _lex_read_str(this, pos, len);
	if (sl.ptr) return !strncmp(sl.ptr, str, len);
	else return false;
}
/* }}} */

/** cleanup trick to turn slices into c-strings */
/* {{{ */
typedef struct { char *ptr; char backup; } lex_temp_t;
void lex_temp_cleanup (lex_temp_t *this) { *this->ptr = this->backup; }

#define LEX_TEMP_SLICE(sl)                                            \
	lex_temp_t __attribute__((cleanup(lex_temp_cleanup)))         \
		__LEX__ ## __LINE__ = {                               \
			(sl).ptr + (sl).length, (sl).ptr[(sl).length] \
		};                                                    \
	(sl).ptr[(sl).length] = '\0'
/* }}} */

/** symbol table */
/* {{{ */
static struct { const char *key; enum lex_type value; } *lex_symbol_table;

#define OP_LEX(l, s) [DANA_    ## l] = s,
#define TK_LEX(l, s) [DANA_    ## l] = s,
#define KW_LEX(l, s) [DANA_KW_ ## l] = s,
#define LT_LEX(l, s)
#define LT_PAR(p, s)
#define KW_PAR(p, s)
#define OP(l, p, s) OP_LEX(l, s)
#define TK(l, p, s) TK_LEX(l, s)
#define LT(l, p, s) LT_LEX(l, s)
#define KW(l, p, s) KW_LEX(l, s)

static const char* lex_symbol_arr[] = {
	[DANA_ERROR] = NULL,
	DANA_TYPES
	DANA_KEYWORDS
	[DANA_COMMENT] = NULL,
};

#undef OP_LEX
#undef TK_LEX
#undef LT_LEX
#undef KW_LEX
#undef LT_PAR
#undef KW_PAR
#undef OP
#undef TK
#undef LT
#undef KW

void __attribute__((constructor))
create_lex_symbol_table (void)
{
	size_t table_size = sizeof(lex_symbol_arr) / sizeof(lex_symbol_arr[0]);
	for (size_t i=0; i<table_size; ++i) if (lex_symbol_arr[i])
		hm_put(lex_symbol_table, lex_symbol_arr[i], i);
}

void __attribute__((destructor))
destroy_lex_symbol_table (void)
{
	hm_free(lex_symbol_table);
}

const char*
lex_get_type_str (lex_token_t tok)
{ return lex_symbol_arr[tok.type]; }

enum lex_type
_lex_scan_sym_table (const lexer_t *this, lex_buf_pos pos, uint32_t len)
{
	slice_char_t sl = _lex_read_str(this, pos, len);
	if (!sl.ptr) return DANA_ERROR;
	LEX_TEMP_SLICE(sl);
	return hm_get(lex_symbol_table, sl.ptr);
}
/* }}} */

/** indentation stack */
/* {{{ */
static uint16_t *indent_stack = {0};

void __attribute__((constructor))
create_indent_stack (void)
{ arr_push(indent_stack, 0); }

void __attribute__((destructor))
destroy_indent_stack (void)
{ arr_free(indent_stack); }
/* }}} */

/** tokenizer */
/* {{{ */
int
isiden (int ch)
{ return isalpha(ch) || isdigit(ch) || ch == '_'; }

int
ishex (int ch)
{ return isdigit(ch) || ('a' <= ch && ch <= 'f'); }

lex_buf_pos
_lex_skip_spaces (const lexer_t *this, lex_buf_pos pos)
{
	for (; isspace(_lex_read_char(this, pos)); ++pos.pos);
	return pos;
}

lex_buf_pos
_lex_scan_char (const lexer_t *this, lex_buf_pos pos)
{
	lex_buf_pos closing = POS_ADV(pos, 2);

	_assert(_lex_read_char(this, POS_ADV(pos, 1)) != '\'',
			"Char literal cannot be empty");
	if (_lex_read_char(this, POS_ADV(pos, 1)) == '\\')
		switch (_lex_read_char(this, POS_ADV(pos, 2))) {
		case 'n':
		case 't':
		case 'r':
		case '\\':
		case '\'':
		case '"':
			closing = POS_ADV(pos, 3); break;
		case 'x':
			if (ishex(_lex_read_char(this, POS_ADV(pos, 3))) &&
					ishex(_lex_read_char(this, POS_ADV(pos, 4))))
				closing = POS_ADV(pos, 5);
		}
	return _lex_read_char(this, closing) == '\''
		? POS_ADV(closing, 1) : (lex_buf_pos){0};
}

lex_token_t
_lex_token_at (const lexer_t *this, lex_buf_pos pos)
{
	lex_buf_pos end;
	enum lex_type kword;

	if (_lex_matches(this, pos, "(*"))
		return (lex_token_t){ DANA_COMMENT, pos };

	switch (_lex_read_char(this, pos)) {
	case '#':
		return (lex_token_t){ DANA_COMMENT, pos };
	case '-':
		if (!isdigit(_lex_read_char(this, POS_ADV(pos, 1))))
			goto lex_operator;
	case '0' ... '9':
		return (lex_token_t){ DANA_NUMBER, pos };
	case '"':
		return (lex_token_t){ DANA_STRING, pos };
	case '\'':
		return (lex_token_t){ DANA_CHAR, pos };
	case '_':
	case 'a' ... 'z':
	case 'A' ... 'Z':
		for (end=POS_ADV(pos, 1); isiden(_lex_read_char(this, end));
				++end.pos);
		kword = _lex_scan_sym_table(this, pos, POS_DIFF(pos, end));
		if (kword) return (lex_token_t){ kword, pos };
		else       return (lex_token_t){ DANA_NAME, pos };
lex_operator:
	default:
		if ((kword = _lex_scan_sym_table(this, pos, 2)))
			return (lex_token_t){ kword, pos };
		if ((kword = _lex_scan_sym_table(this, pos, 1)))
			return (lex_token_t){ kword, pos };
	}
	return (lex_token_t){ DANA_ERROR, pos };
}

lex_buf_pos
_lex_token_end (const lexer_t *this, lex_token_t tok)
{
	lex_buf_pos end = tok.pos;

	switch (tok.type) {
	case DANA_COMMENT:
		if (_lex_read_char(this, tok.pos) == '#') {
			for (++end.pos; _lex_read_char(this, end) != '\n'; ++end.pos);
			return end;
		} else {
			for (end.pos += 2; _lex_read_char(this, POS_ADV(end, 1)) &&
					!_lex_matches(this, end,"*)");
					++end.pos)
				if (_lex_matches(this, end,"(*"))
					end = _lex_token_end(this, (lex_token_t){
							DANA_COMMENT, end
							});
			if (_lex_matches(this, end,"*)"))
				return POS_ADV(end, 2);
			tok.type = DANA_ERROR;
			return (lex_buf_pos){};
		}
	case DANA_NUMBER:
		for (++end.pos; isdigit(_lex_read_char(this, end)); ++end.pos);
		return end;
	case DANA_STRING:
		for (++end.pos; _lex_read_char(this, end) != '"'; ++end.pos)
			if (_lex_matches(this, end, "\\\"")) ++end.pos;
		return POS_ADV(end, 1);
	case DANA_CHAR:
		return _lex_scan_char(this, tok.pos);
	case DANA_NAME:
	case DANA_KW_AND ... DANA_KW_VAR:
		for (++end.pos; isiden(_lex_read_char(this, end)); ++end.pos);
		return end;
	default: /* operator */
		return POS_ADV(end, strlen(lex_get_type_str(tok)));
	}
}

inline slice_char_t
lex_get_token (const lexer_t *this, lex_token_t tok)
{ return _lex_read_str(this, tok.pos, POS_DIFF(tok.pos, _lex_token_end(this, tok))); }

lex_token_t
lex_next_token (const lexer_t *this, const lex_token_t *prev)
{
	lex_buf_pos start = {0};
	lex_token_t ret;

	if (prev) start =  _lex_token_end(this, *prev);
	start = _lex_skip_spaces(this, start);
	if (_lex_eof(this, start))
		return (lex_token_t){ DANA_EOF };

	ret = _lex_token_at(this, start);
	if (ret.type == DANA_COMMENT)
		return lex_next_token(this, &ret);
	return ret;
}
/* }}} */
#endif // LEX_IMPLEMENT
