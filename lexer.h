#pragma once

#ifdef LEX_IMPLEMENT
	#define ALLOC_IMPLEMENT
	#define DA_IMPLEMENT
#endif

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include "symbols.h"
#include "util.h"
#include "util/alloc.h"
#include "util/dynamic_array.h"

#pragma region TYPES

typedef slice(char) slice_char_t;
POS_DECL(lex_buf_pos, 24);

typedef struct {
	slice_char_t   buffer;
	const alloc_t *alloc;
	const char    *fname;
	lex_buf_pos   *lines; /* dynamic array of beginnings-of-line */
} lexer_t;

typedef struct { uint16_t line, column; } file_pos;
typedef struct { uint8_t tabs, spaces;  } indent_info_t;

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

typedef struct {
	enum lex_type {
		DANA_ERROR = 0,
		DANA_TYPES
		DANA_KEYWORDS
		DANA_COMMENT,
		LEX_TYPE_LEN,
	} type : 7;
	bool is_bol: 1;
	lex_buf_pos pos;
} lex_token_t;

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

#pragma endregion

#pragma region METHOD DELCARATIONS
#define LEX_CLEANUP    __attribute__((cleanup(lexer_destroy)))
extern lexer_t         lexer_create(const alloc_t *alloc, const char *fname);
extern void            lexer_destroy(const lexer_t *this);
extern lex_token_t     lex_next_token(lexer_t *this, const lex_token_t *prev);
extern slice_char_t    lex_get_token(const lexer_t *this, lex_token_t tok);
extern char            lex_get_char(const lexer_t *this, lex_token_t tok);
extern const char*     lex_get_type_str(lex_token_t tok);

#ifdef LEX_IMPLEMENT
/** private declarations */
bool           _lex_eof(const lexer_t *this, lex_buf_pos pos);
char           _lex_read_char(const lexer_t *this, lex_buf_pos pos);
slice_char_t   _lex_read_str(const lexer_t *this, lex_buf_pos pos, uint32_t length);
bool           _lex_matches(const lexer_t *this, lex_buf_pos pos, const char *str);
enum lex_type  _lex_scan_sym_table(const lexer_t *this, lex_buf_pos pos, uint32_t len);
int             isiden(int ch);
int             ishex(int ch);
char           _parse_hex(const char buf[2]);
lex_buf_pos    _lex_skip_spaces(const lexer_t *this, lex_buf_pos pos);
lex_buf_pos    _lex_scan_char(const lexer_t *this, lex_buf_pos pos);
lex_token_t    _lex_token_at(const lexer_t *this, lex_buf_pos pos);
lex_buf_pos    _lex_token_end(const lexer_t *this, lex_token_t tok); /* exclusive */
#endif
#pragma endregion

#pragma region INDENTATION
extern int indent_cmp(indent_info_t a, indent_info_t b);

#ifdef LEX_IMPLEMENT
inline int
indent_cmp (indent_info_t a, indent_info_t b)
{
	if (a.tabs != b.tabs) return a.tabs - b.tabs;
	return a.spaces - b.spaces;
}
#endif
#pragma endregion

#pragma region SYMBOL TABLE
#ifdef LEX_IMPLEMENT

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

#define OP_LEX(l, s)
#define TK_LEX(l, s)
#define LT_LEX(l, s) lex_symbol_arr[DANA_ ## l] = s;
#define LT_PAR(p, s)
#define OP(l, p, s) OP_LEX(l, s)
#define TK(l, p, s) TK_LEX(l, s)
#define LT(l, p, s) LT_LEX(l, s)

void __attribute__((constructor))
create_lex_symbol_table (void)
{
	size_t table_size = sizeof(lex_symbol_arr) / sizeof(lex_symbol_arr[0]);
	for (size_t i=0; i<table_size; ++i) if (lex_symbol_arr[i])
		hm_put(lex_symbol_table, lex_symbol_arr[i], i);
	/* add literals on symbol array */
	DANA_TYPES
}

#undef OP_LEX
#undef TK_LEX
#undef LT_LEX
#undef LT_PAR
#undef OP
#undef TK
#undef LT

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
	SLICE_TMP_STR(sl);
	return hm_get(lex_symbol_table, sl.ptr);
}

#endif
#pragma endregion

#pragma region POSITIONING SYSTEM
extern void     lex_push_line(lexer_t *this, lex_buf_pos pos);
extern file_pos lex_get_position(const lexer_t *this, lex_buf_pos pos);

#ifdef LEX_IMPLEMENT
inline void
lex_push_line (lexer_t *this, lex_buf_pos pos)
{
	arr_push(this->lines, pos);
}

file_pos
lex_get_file_pos (const lexer_t *this, lex_buf_pos pos)
{
	size_t idx = 0;

	for (size_t jmp = arr_ulen(this->lines) / 2; jmp >= 1; jmp /= 2)
		while (idx + jmp < arr_ulen(this->lines) &&
				POS_DIFF(this->lines[idx + jmp], pos) >= 0)
			idx += jmp;
	assert(pos.pos >= this->lines[idx].pos);

	return (file_pos){
		(uint16_t) idx + 1, POS_DIFF(this->lines[idx], pos) + 1,
	};
}
#endif
#pragma endregion

#ifdef LEX_IMPLEMENT
#pragma region METHODS
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

	if (ret.buffer.ptr) {
		lex_push_line(&ret, (lex_buf_pos){0});
		for (uint32_t i = 0; i < ret.buffer.length; ++i)
			if (ret.buffer.ptr[i] == '\n')
				lex_push_line(&ret, (lex_buf_pos){ i + 1 });
	}

	return ret;
}

void
lexer_destroy (const lexer_t *this)
{
	if (this->buffer.ptr) deallocate(*this->alloc, this->buffer);
	arr_free(this->lines);
}

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
#pragma endregion

#pragma region TOKENIZER
int
isiden (int ch)
{ return isalpha(ch) || isdigit(ch) || ch == '_'; }

int
ishex (int ch)
{ return isdigit(ch) || ('a' <= ch && ch <= 'f'); }

inline char
_parse_hex (const char buf[2])
{
	return (('0' <= buf[0] && buf[0] <= '9' ? buf[0] - '0' : 9 + buf[0] - 'a') << 4)
		+ ('0' <= buf[1] && buf[1] <= '9' ? buf[1] - '0' : 9 + buf[1] - 'a');
}

lex_buf_pos
_lex_skip_spaces (const lexer_t *this, lex_buf_pos pos)
{
	for (; isspace(_lex_read_char(this, pos)); pos = POS_ADV(pos, 1));
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

char
lex_get_char (const lexer_t *this, lex_token_t tok)
{
	slice_char_t sl = lex_get_token(this, tok);
	char buf[2];

	switch (sl.length) {
	case 2: return sl.ptr[1];
	case 3: switch(sl.ptr[2]) {
		case 'n': return '\n';
		case 't': return '\t';
		case 'r': return '\r';
		case '0': return '\0';
		case '\\': return '\\';
		case '\'': return '\'';
		case '"': return '"';
		default: _assert(false, "Invalid character passed scan");
		}
	case 5:
		buf[0] = sl.ptr[3]; buf[1] = sl.ptr[4];
		return _parse_hex(buf);
	default: _assert(false, "Invalid character passed scan");
	}
}

lex_token_t
_lex_token_at (const lexer_t *this, lex_buf_pos pos)
{
	lex_buf_pos end;
	enum lex_type kword;
	bool is_bol = false;

	for (; isspace(_lex_read_char(this, pos)); ++pos.pos) {
		if (_lex_read_char(this, pos) == '\n') is_bol = true;
	}

	if (_lex_eof(this, pos))
		return (lex_token_t){ .type = DANA_EOF, .pos = pos };
	if (_lex_matches(this, pos, "(*"))
		return (lex_token_t){ .type = DANA_COMMENT, .pos = pos };

	switch (_lex_read_char(this, pos)) {
	case '#':
		return (lex_token_t){
			.type = DANA_COMMENT, .pos = pos, .is_bol = is_bol,
		};
	case '0' ... '9':
		return (lex_token_t){
			.type = DANA_NUMBER, .pos = pos, .is_bol = is_bol,
		};
	case '"':
		return (lex_token_t){
			.type = DANA_STRING, .pos = pos, .is_bol = is_bol,
		};
	case '\'':
		return (lex_token_t){
			.type = DANA_CHAR, .pos = pos, .is_bol = is_bol,
		};
	case '_':
	case 'a' ... 'z':
	case 'A' ... 'Z':
		for (end=POS_ADV(pos, 1); isiden(_lex_read_char(this, end));
				++end.pos);
		kword = _lex_scan_sym_table(this, pos, POS_DIFF(pos, end));
		if (kword) return (lex_token_t){
			.type = kword, .pos = pos, .is_bol = is_bol,
		};
		else return (lex_token_t){
			.type = DANA_NAME, .pos = pos, .is_bol = is_bol,
		};
	default:
		if ((kword = _lex_scan_sym_table(this, pos, 2)))
			return (lex_token_t){
				.type = kword, .pos = pos, .is_bol = is_bol,
			};
		if ((kword = _lex_scan_sym_table(this, pos, 1)))
			return (lex_token_t){
				.type = kword, .pos = pos, .is_bol = is_bol,
			};
	}
	return (lex_token_t){ .type = DANA_ERROR, .pos = pos };
}

lex_buf_pos
_lex_token_end (const lexer_t *this, lex_token_t tok)
{
	lex_buf_pos end = _lex_skip_spaces(this, tok.pos);

	switch (tok.type) {
	case DANA_EOF: return end;
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
							.type = DANA_COMMENT,
							.pos = end,
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
lex_next_token (lexer_t *this, const lex_token_t *prev)
{
	lex_buf_pos start = {0};
	lex_token_t ret;

	do {
		if (prev) start =  _lex_token_end(this, *prev);
		if (_lex_eof(this, start))
			return (lex_token_t){ DANA_EOF };
		ret = _lex_token_at(this, start);
		prev = &ret;
	} while (ret.type == DANA_COMMENT);
	return ret;
}
#pragma endregion
#endif // LEX_IMPLEMENT
