#pragma once

#ifdef PARSER_IMPLEMENT
	#define LEX_IMPLEMENT
	#define DA_IMPLEMENT
#endif

#include "lexer.h"
#include "util.h"
#include "util/dynamic_array.h"

#pragma region TYPES

POS_DECL(par_token_pos, 32);
POS_DECL(ast_node_pos, 32);
POS_DECL(par_text_pos, 16);

#define LT_PAR(p, s) AST_ ## p,
#define KW_PAR(p, s) AST_ ## p,
#define OP(l, p, s)  AST_ ## p,
#define TK(l, p, s)  AST_ ## p,
#define LT(l, p, s)  LT_PAR(p, s)
#define KW(l, p, s)  KW_PAR(p, s)
/* just for alignment between lex_type and ast_type */
#define OP_LEX(l, s) AST_UNUSED_    ## l,
#define TK_LEX(l, s) AST_UNUSED_    ## l,
#define LT_LEX(l, s) AST_UNUSED_    ## l,
#define KW_LEX(l, s) AST_UNUSED_KW_ ## l,

typedef struct {
	enum ast_type {
		AST_ERROR = 0,
		DANA_TYPES
		DANA_KEYWORDS
		AST_TYPE_LEN
	} __attribute__((packed)) type;
	lex_buf_pos src;
	uint16_t length;
	uint16_t name;
	union {
		union {
			uint16_t num;
			par_text_pos str;
			uint16_t name;
			char ch;
		} pl_data;
		struct {
			uint16_t array;
		} var_data;
	};
} ast_node_t;

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

typedef struct {
	enum dtype {
		DTYPE_INT   = 1,
		DTYPE_BYTE  = 2,
		DTYPE_ARRAY = 4,
		DTYPE_VAR   = 8,
	} __attribute__((packed)) type;
	uint16_t dim;
	uint16_t next;
} dtype_t;

typedef struct {
	lexer_t      lexer;  /* underlying lexer */
	lex_token_t *tokens; /* dynamic array of the tokens created by lexer */
	ast_node_t  *nodes;  /* dynamic array of the AST nodes */
	char        *text;   /* dynamic array of const strings (0-terminated) */
	struct {
		dtype_t key; uint16_t value;
	} *types;            /* hash map matching type => id */
	struct name_record {
		size_t key; uint16_t value;
		slice_char_t decl; /* first declaration of name, dbg only */
	} *names;            /* hash map matching name => id */
	bool has_peeked;     /* allows peeking */
} parser_t;

typedef struct {
	ast_node_t *node;
	const parser_t *parser;
	ast_node_pos pos, end;
} ast_node_it;

typedef struct { uint8_t lhs, rhs; } parser_bp_t;
typedef enum { PREFIX = 1, INFIX, POSTFIX } op_type;

#pragma endregion

#pragma region METHOD DECLARATIONS
/** public methods */
#define PARSER_CLEANUP    __attribute__((cleanup(parser_destroy)))
extern parser_t           parser_create(const lexer_t lex);
extern void               parser_destroy(parser_t *this);
extern ast_node_pos       parse(parser_t *this);
/** private methods */
#define par_emplace_node(p, decl...) \
	par_push_node((p), (ast_node_t){ .length = 1, decl })
extern const slice_char_t par_get_name(const parser_t *this, uint16_t id);
extern const ast_node_t   par_get_node(const parser_t *this, ast_node_pos pos);
extern const char*        par_get_text(const parser_t *this, par_text_pos pos);
extern const lex_token_t  par_get_token(const parser_t *this, par_token_pos pos);
extern const dtype_t      par_get_type(const parser_t *this, uint16_t pos);
extern const slice_char_t par_get_value_by_pos(const parser_t *this, par_token_pos pos);
extern const slice_char_t par_get_value_by_tok(const parser_t *this, lex_token_t tok);
extern ast_node_t*        par_node_at(const parser_t *this, ast_node_pos pos);
extern lex_token_t        par_peek_token(parser_t *this);
extern lex_token_t        par_pop_token(parser_t *this);
extern uint16_t           par_push_name(parser_t *this, lex_token_t tok);
extern ast_node_pos       par_push_node(parser_t *this, ast_node_t node);
extern par_text_pos       par_push_text(parser_t *this, lex_token_t tok);
extern uint16_t           par_push_type(parser_t *this, dtype_t type);
extern ast_node_pos       par_reserve_node(parser_t *this);
extern void               par_reverse_range(parser_t *this, ast_node_pos begin, ast_node_pos end);
/** parser functions */
extern ast_node_pos       parse_args(parser_t *this, enum ast_type to_match);
extern ast_node_pos       parse_block(parser_t *this);
extern ast_node_pos       parse_decl(parser_t *this, enum lex_type to_match);
extern ast_node_pos       parse_expr(parser_t *this, uint8_t thrs);
extern ast_node_pos       parse_local_defs(parser_t *this);
extern ast_node_pos       parse_lvalue(parser_t *this);
extern ast_node_pos       parse_stmt(parser_t *this);
extern ast_node_pos       parse_var(parser_t *this, enum lex_type to_match);
/** utility macros */
#define node_at(p, pos) (*par_node_at(p, pos))
#define par_get_value(p, x) _Generic((x),    \
	lex_token_t  : par_get_value_by_tok, \
	par_token_pos: par_get_value_by_pos  \
)(p, x)
#define par_pop_require(this, t) ({                                             \
		lex_token_t __ret__ = par_pop_token(this);                      \
		file_pos __pos__ = lex_get_file_pos(&this->lexer, __ret__.pos); \
		_assert(__ret__.type == t,                                      \
				"REQUIRE ERR at (%u:%u):\tExpected %s, got %s", \
				__pos__.line, __pos__.column,                   \
				lex_symbol_arr[t], lex_get_type_str(__ret__));  \
				__ret__;                                       })

#pragma endregion

#pragma region AST_TYPE PRINTING
/** declarations */
extern const char*   ast_get_type_str(ast_node_t node);

#ifdef PARSER_IMPLEMENT

#define LT_PAR(p, s) [AST_ ## p] = s,
#define KW_PAR(p, s) [AST_ ## p] = s,
#define OP(l, p, s)  [AST_ ## p] = s,
#define TK(l, p, s)  [AST_ ## p] = s,
#define LT(l, p, s) LT_PAR(p, s)
#define KW(l, p, s) KW_PAR(p, s)
#define OP_LEX(l, s)
#define TK_LEX(l, s)
#define KW_LEX(l, s)
#define LT_LEX(l, s)

static const char* ast_symbol_arr[] = {
	[AST_ERROR] = NULL,
	DANA_TYPES
	DANA_KEYWORDS
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
#define LT_LEX(l, s)
#define LT_PAR(p, s) ast_symbol_arr[AST_ ## p] = s;
#define OP(l, p, s) OP_LEX(l, s)
#define TK(l, p, s) TK_LEX(l, s)
#define LT(l, p, s) LT_PAR(p, s)

static void __attribute__((constructor))
fix_ast_sym_arr (void)
{
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

inline const char*
ast_get_type_str (ast_node_t node)
{ return ast_symbol_arr[node.type]; }

#endif
#pragma endregion

#pragma region AST ITERATOR

extern ast_node_it ast_get_child(const parser_t *parser, ast_node_pos parent);
extern bool        ast_is_child(ast_node_it it);
extern ast_node_it ast_next_child(ast_node_it it);

#ifdef PARSER_IMPLEMENT
inline ast_node_it
ast_get_child (const parser_t *parser, ast_node_pos parent)
{
	return (ast_node_it){
		.parser = parser,
		.node = parser->nodes + parent.pos + 1,
		.pos = POS_ADV(parent, 1),
		.end = POS_ADV(parent, node_at(parser, parent).length),
	};
}

inline bool
ast_is_child (ast_node_it it)
{ return POS_CMP(it.end, it.pos) > 0; }

inline ast_node_it
ast_next_child (ast_node_it it)
{
	_assert(it.pos.pos < arr_ulen(it.parser->nodes),
			"Iterator post array end");
	return (ast_node_it){
		.parser = it.parser, .end = it.end,
		.pos = POS_ADV(it.pos, it.node->length),
		.node = it.node + it.node->length,
	};

}
#endif
#pragma endregion

#pragma region PRETTY PRINTER

#define par_print(this, pos) par_full_print(stdout, this, pos, 0)
#define par_fprint(f, this, pos) par_full_print(f, this, pos, 0)
#define PAR_INLINE -2
extern void par_full_print(FILE *f, const parser_t *this, ast_node_pos pos, int8_t depth);
extern void par_begin_line(FILE *f, const parser_t *this, ast_node_pos pos, int8_t depth);

#ifdef PARSER_IMPLEMENT

#pragma push_macro("log")
#pragma push_macro("log_plain")
#pragma push_macro("log_nl")
#define log(fmt...)       (depth >= 0 ? log_nl(fmt) : log_plain(fmt))
#define log_plain(fmt...) fprintf(f, fmt)
#define log_nl(fmt, ...)  fprintf(f, fmt "\n" __VA_OPT__(,) __VA_ARGS__)

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat-zero-length"
void
par_full_print (FILE *f, const parser_t *this, ast_node_pos pos, int8_t depth)
{
#define _NAME_ UNSLICE(par_get_name(this, node.name))
#define _TYPE_ ast_get_type_str(node)
	if (pos.pos >= arr_ulen(this->nodes)) return;
	if (depth < 0) depth = PAR_INLINE;

	ast_node_t node = node_at(this, pos);
	ast_node_it it = ast_get_child(this, pos);
	par_begin_line(f, this, pos, depth);

	switch (node.type) {
	/* literals */
	case AST_TRUE ... AST_FALSE:
		log("%s", node.type == AST_TRUE ? "true" : "false");
	break; case AST_NUMBER:
		log("%u", node.pl_data.num);
	break; case AST_CHAR:
		log("'%c'", node.pl_data.ch);
	break; case AST_STRING:
		log("\"%s\"", par_get_text(this, node.pl_data.str));
	break; case AST_NAME:
		log("%.*s", _NAME_);
	/* variables */
	break; case AST_INT ... AST_BYTE:
	       case AST_REF_INT ... AST_REF_BYTE:
		log("%s %.*s", _TYPE_, _NAME_);
	break; case AST_ARRAY:
		dtype_t type = par_get_type(this, node.var_data.array);
		assert(type.type & DTYPE_ARRAY);
		if (type.type & DTYPE_INT) log_plain("int ");
		else                       log_plain("byte ");
		if (type.type & DTYPE_VAR) log_plain("var-array ");
		else                       log_plain("array ");
		log_plain("%.*s", _NAME_);
		for (; type.next; type = par_get_type(this, type.next))
			if (type.dim) log_plain("[%u]", type.dim);
			else          log_plain("[]");
		log("");
	/* expressions */
	break; case AST_PLUS ... AST_CMP_GEQ:
	       case AST_BOOL_AND ... AST_BOOL_OR:
		log_plain("(%s ", _TYPE_);
		{
			assert(ast_is_child(it));
			par_full_print(f, this, it.pos, PAR_INLINE);
			it = ast_next_child(it);
		}
		{
			assert(ast_is_child(it));
			log_plain(" ");
			par_full_print(f, this, it.pos, PAR_INLINE);
			it = ast_next_child(it);
		}
		log(")");
	/* blocks */
	break; case AST_BLOCK_SIMPLE:
		log_plain("[");
		par_full_print(f, this, it.pos, PAR_INLINE);
		log("]");
	break; case AST_BLOCK:
		log("begin");
		for (; ast_is_child(it); it = ast_next_child(it))
			par_full_print(f, this, it.pos, depth + 1);
		par_begin_line(f, this, pos, depth); log("end");
	/* statements */
	break; case AST_SKIP: case AST_EXIT:
		log("%s", _TYPE_);
	break; case AST_BREAK: case AST_CONT:
	       case AST_LOOP:
		if (node.name) log("%s \"%.*s\"", _TYPE_, _NAME_);
		else           log("%s", _TYPE_);
		if (node.type != AST_LOOP) break;
		for (; ast_is_child(it); it = ast_next_child(it))
			par_full_print(f, this, it.pos, depth + 1);
	break; case AST_PROC_CALL:
		log_plain("proc %.*s ", _NAME_);
		{
			assert(ast_is_child(it));
			par_full_print(f, this, it.pos, PAR_INLINE);
		}
		log("");
	break; case AST_ASSIGN:
		log_plain("assign ");
		{
			assert(ast_is_child(it));
			par_full_print(f, this, it.pos, PAR_INLINE);
			it = ast_next_child(it);
		}
		{
			assert(ast_is_child(it));
			log_plain(" := ");
			par_full_print(f, this, it.pos, PAR_INLINE);
		}
		log("");
	break; case AST_RETURN:
		log_plain("return ");
		{
			assert(ast_is_child(it));
			par_full_print(f, this, it.pos, PAR_INLINE);
		}
		log("");
	break; case AST_COND:
		bool is_cond;
		{
			assert(ast_is_child(it));
			log_plain("if ");
			par_full_print(f, this, it.pos, PAR_INLINE);
			it = ast_next_child(it);
		}
		{
			assert(ast_is_child(it));
			log(" then");
			par_full_print(f, this, it.pos, depth + 1);
			it = ast_next_child(it);
		}
		for (is_cond = true; ast_is_child(it);
				it = ast_next_child(it), is_cond = !is_cond)
			if (is_cond && ast_is_child(ast_next_child(it))) {
				par_begin_line(f, this, pos, depth);
				log_plain("else if ");
				par_full_print(f, this, it.pos, PAR_INLINE);
			} else if (is_cond) {
				par_begin_line(f, this, pos, depth);
				log("else");
				par_full_print(f, this, it.pos, depth + 1);
			} else {
				log(" then");
				par_full_print(f, this, it.pos, depth + 1);
			}
	/* function declarations / definitions */
	break; case AST_DEF_PROC ... AST_DEF_BYTE:
	       case AST_DECL_PROC ... AST_DECL_BYTE:
		log_plain("%s %.*s ", _TYPE_, _NAME_);
		{
			assert(ast_is_child(it));
			par_full_print(f, this, it.pos, PAR_INLINE);
			it = ast_next_child(it);
		}
		log("");
		if (AST_DECL_PROC <= node.type && node.type <= AST_DECL_BYTE)
			break;
		{
			assert(ast_is_child(it));
			par_full_print(f, this, it.pos, depth + 1);
			it = ast_next_child(it);
		}
		{
			assert(it.node);
			par_full_print(f, this, it.pos, depth + 1);
		}
	break; case AST_VARS: case AST_ARGS:
		log_plain("(");
		for (; ast_is_child(it); it=ast_next_child(it)) {
			if (POS_CMP(it.pos, POS_ADV(pos, 1))) log_plain(", ");
			par_full_print(f, this, it.pos, PAR_INLINE);
		}
		log(")");
	break; case AST_LOCAL_DEF:
		log("local defs");
		for (; ast_is_child(it); it = ast_next_child(it))
			par_full_print(f, this, it.pos, depth + 1);
	break; default:
		log("{{ %s - UNSUPPORTED }}", _TYPE_);
	}
#undef _NAME_
#undef _TYPE_
}
#pragma GCC diagnostic pop

inline void
par_begin_line (FILE *f, const parser_t *this, ast_node_pos pos, int8_t depth)
{
	file_pos ln = lex_get_file_pos(&this->lexer, node_at(this, pos).src);
	for (uint8_t i=0; i<depth; ++i) log_plain(" |");
	// if (depth >= 0) log_plain(" (%u) ", pos.pos);
	// if (depth >= 0) log_plain(" ");
	if (depth >= 0) log_plain(" (%u:%u) ", ln.line, ln.column);
}

#pragma pop_macro("log")
#pragma pop_macro("log_plain")
#pragma pop_macro("log_nl")

#endif
#pragma endregion

#pragma region BINDING POWER

extern parser_bp_t prefix_table[LEX_TYPE_LEN],
		   infix_table[LEX_TYPE_LEN],
		   postfix_table[LEX_TYPE_LEN];

#define bp_lhs(x, mode) _bp_lhs((x).type, mode)
#define bp_rhs(x, mode) _bp_rhs((x).type, mode)
extern uint8_t _bp_lhs(uint8_t type, op_type mode);
extern uint8_t _bp_rhs(uint8_t type, op_type mode);
#define bp_operator_is(x, mode)  _bp_operator_is((x).type, mode)
extern bool    _bp_operator_is(uint8_t type, op_type mode);

#ifdef PARSER_IMPLEMENT
/** table definitions */
#define PRE(e, bp) [DANA_ ## e] = { 0, (bp) << 1 },
#define IN(e, bp, ass)
#define POST(e, bp)
parser_bp_t prefix_table[LEX_TYPE_LEN] = { DANA_OPERATORS };
#undef PRE
#undef IN
#undef POST

#define PRE(e, bp)
#define IN(e, bp, ass) [DANA_ ## e] = { ((bp) << 1) + (ass), ((bp) << 1) + 1 - (ass) },
#define POST(e, bp)
parser_bp_t infix_table[LEX_TYPE_LEN] = { DANA_OPERATORS };
#undef PRE
#undef IN
#undef POST

#define PRE(e, bp)
#define IN(e, bp, ass)
#define POST(e, bp) [DANA_ ## e] = { (bp) << 1, 0 },
parser_bp_t postfix_table[LEX_TYPE_LEN] = { DANA_OPERATORS };
#undef PRE
#undef IN
#undef POST
#endif

/** methods */
inline uint8_t
_bp_lhs (uint8_t type, op_type mode)
{
	switch (mode) {
	case PREFIX : return prefix_table[type].lhs;
	case INFIX  : return infix_table[type].lhs;
	case POSTFIX: return postfix_table[type].lhs;
	default     : return 0;
	}
}

inline uint8_t
_bp_rhs (uint8_t type, op_type mode)
{
	switch (mode) {
	case PREFIX : return prefix_table[type].rhs;
	case INFIX  : return infix_table[type].rhs;
	case POSTFIX: return postfix_table[type].rhs;
	default     : return 0;
	}
}

inline bool
_bp_operator_is (uint8_t type, op_type mode)
{
	switch (mode) {
	case PREFIX : return prefix_table[type].rhs;
	case INFIX  : return infix_table[type].lhs && infix_table[type].rhs;
	case POSTFIX: return postfix_table[type].lhs;
	default     : return false;
	}
}

#pragma endregion

#ifdef PARSER_IMPLEMENT
#pragma region PARSER METHODS
parser_t
parser_create(const lexer_t lex)
{
	parser_t ret = { .lexer = lex };
	arr_push(ret.tokens, (lex_token_t){});
	arr_push(ret.nodes, (ast_node_t){});
	arr_push(ret.text, '\0');
	return ret;
}

void
parser_destroy (parser_t *this)
{
	arr_free(this->tokens);
	arr_free(this->nodes);
	arr_free(this->text);
	hm_free(this->names);
	hm_free(this->types);
}

inline ast_node_pos
parse (parser_t *this)
{
	return parse_decl(this, DANA_KW_DEF);
}

inline const slice_char_t
par_get_name (const parser_t *this, uint16_t id)
{
	if (id == 0) return (slice_char_t){};
	for (size_t i=0; i<arr_ucap(this->names); ++i)
		if (this->names[i].value == id) return this->names[i].decl;
	_assert(false, "Name ID %u not found", id);
}

inline const ast_node_t
par_get_node (const parser_t *this, ast_node_pos pos)
{
	_assert(0 <= pos.pos && pos.pos < arr_ulen(this->nodes),
			"Node index %u out of bounds [0, %lu]",
			pos.pos, arr_ulen(this->nodes));
	return this->nodes[pos.pos];
}

inline const char*
par_get_text (const parser_t *this, par_text_pos pos)
{
	_assert(0 <= pos.pos && pos.pos < arr_ulen(this->text),
			"Text index %u out of bounds [0, %lu]",
			pos.pos, arr_ulen(this->text));
	return this->text + pos.pos;
}

inline const lex_token_t
par_get_token (const parser_t *this, par_token_pos pos)
{
	_assert(0 <= pos.pos && pos.pos < arr_ulen(this->tokens),
			"Token index %u out of bounds [0, %lu]",
			pos.pos, arr_ulen(this->tokens));
	return this->tokens[pos.pos];
}

inline const dtype_t
par_get_type (const parser_t *this, uint16_t id)
{
	for (size_t i=0; i<arr_ucap(this->types); ++i)
		if (this->types[i].value == id) return this->types[i].key;
	_assert(false, "Type ID %u not found", id);
}

inline const slice_char_t
par_get_value_by_pos (const parser_t *this, par_token_pos pos)
{
	return lex_get_token(&this->lexer, par_get_token(this, pos));
}

inline const slice_char_t
par_get_value_by_tok (const parser_t *this, lex_token_t tok)
{
	return lex_get_token(&this->lexer, tok);
}

ast_node_t*
par_node_at (const parser_t *this, ast_node_pos pos)
{
	_assert(0 <= pos.pos && pos.pos < arr_ulen(this),
			"Invalid node at(): %u >= %lu",
			pos.pos, arr_ulen(this->nodes));
	return this->nodes + pos.pos;
}

lex_token_t
par_peek_token (parser_t *this)
{
	lex_token_t ret = par_pop_token(this);
	this->has_peeked = true;
	return ret;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsequence-point"
lex_token_t
par_pop_token (parser_t *this)
{
	if (!this->has_peeked) {
		if (arr_ulen(this->tokens) == 1)
			arr_push(this->tokens, lex_next_token(
						&this->lexer, NULL
						));
		else
			arr_push(this->tokens, lex_next_token(
					&this->lexer, &arr_back(this->tokens)
					));
	}
	this->has_peeked = 0;
	return arr_back(this->tokens);
}
#pragma GCC diagnostic pop

uint16_t
par_push_name (parser_t *this, lex_token_t tok)
{
	uint16_t id;
	slice_char_t sl = par_get_value(this, tok);
	size_t hash = hm_hash_bytes(sl.ptr, sl.length);

	if ((id = hm_get(this->names, hash)) == 0) {
		id = hm_ulen(this->names) + 1;
		hm_put(this->names, hash, id);
		hm_getp(this->names, hash)->decl = sl;
	}
	printf("NAME: %10.*s - %u\n", UNSLICE(sl), id);
	return id;
}

inline ast_node_pos
par_push_node (parser_t *this, ast_node_t node)
{
	arr_push(this->nodes, node);
	return (ast_node_pos){ arr_ulen(this->nodes) - 1 };
}

par_text_pos
par_push_text (parser_t *this, lex_token_t tok)
{
	slice_char_t sl = par_get_value(this, tok);
	par_text_pos ret = { arr_ulen(this->text) };
	uint32_t i;

	for (i = 1; i < sl.length - 1; ++i) arr_push(this->text, sl.ptr[i]);
	arr_push(this->text, '\0');
	return ret;
}

uint16_t
par_push_type (parser_t *this, dtype_t type)
{
	uint16_t id;
	if ((id = hm_get(this->types, type)) == 0) {
		id = hm_ulen(this->types) + 1;
		hm_put(this->types, type, id);
	}
	return id;
}

inline ast_node_pos
par_reserve_node (parser_t *this)
{
	return par_push_node(this, (ast_node_t){0});
}

void
par_reverse_range (parser_t *this, ast_node_pos begin, ast_node_pos end)
{
	uint32_t length = POS_DIFF(begin, end), i;

	for (i = 0; i < length / 2; ++i)
		swap(this->nodes[begin.pos + i], this->nodes[end.pos - i]);
}
#pragma endregion

#pragma region PRATT FUNCTIONS
ast_node_pos
parse_args (parser_t *this, enum ast_type to_match)
{
	ast_node_pos node = par_emplace_node(this,
		.type = AST_ARGS,
		.src = par_peek_token(this).pos,
	);

	if (to_match == AST_FUNC_CALL &&
			par_peek_token(this).type == DANA_CLOSE_PAREN)
		return node;

	dbg(node_at(this, node).length, "ARG:\tno. %u");
	parse_expr(this, 0);
	node_at(this, node).length += 1;

	while (par_peek_token(this).type == DANA_COMMA) {
		par_pop_token(this);
		dbg(node_at(this, node).length, "ARG:\tno. %u");
		parse_expr(this, 0);
		node_at(this, node).length += 1;
	}

	return node;
}

ast_node_pos
parse_block (parser_t *this)
{
	ast_node_pos node;

	switch (par_peek_token(this).type) {
	case DANA_KW_BEGIN:
		node = par_emplace_node(this,
			.type = AST_BLOCK, .src = par_pop_token(this).pos,
		);
		while (par_peek_token(this).type != DANA_KW_END) {
			_assert(par_peek_token(this).type != DANA_EOF,
					"EOF before block end");
			node_at(this, node).length +=
				node_at(this, parse_stmt(this)).length;
		}
		par_pop_token(this);
		return node;
	default:
		node = par_emplace_node(this,
			.type = AST_BLOCK_SIMPLE,
			.src = par_peek_token(this).pos,
		);
		node_at(this, node).length +=
			node_at(this, parse_stmt(this)).length;
		return node;
	}
}

ast_node_pos
parse_cond (parser_t *this)
{
	ast_node_pos node = par_emplace_node(this,
		.type = AST_COND,
		.src = par_pop_require(this, DANA_KW_IF).pos,
	);

	node_at(this, node).length += node_at(this, parse_expr(this, 0)).length;
	par_pop_require(this, DANA_COLON);
	node_at(this, node).length += node_at(this, parse_block(this)).length;

	while (par_peek_token(this).type == DANA_KW_ELIF) {
		par_pop_token(this);
		node_at(this, node).length += node_at(this, parse_expr(this, 0)).length;
		par_pop_require(this, DANA_COLON);
		node_at(this, node).length += node_at(this, parse_block(this)).length;
	}

	if (par_peek_token(this).type == DANA_KW_ELSE) {
		par_pop_token(this);
		par_pop_require(this, DANA_COLON);
		node_at(this, node).length += node_at(this, parse_block(this)).length;
	}

	return node;
}

ast_node_pos
parse_decl (parser_t *this, enum lex_type to_match)
{
	ast_node_pos node;

	_assert(to_match == DANA_KW_DECL || to_match == DANA_KW_DEF,
			"Invalid parameter on func-decl");
	node = par_emplace_node(this,
		.type = to_match,
		.src = par_pop_require(this, to_match).pos,
		.name = par_push_name(this, par_pop_require(this, DANA_NAME)),
	);

	if (par_peek_token(this).type == DANA_KW_IS) {
		par_pop_token(this);
		node_at(this, node).type += SWITCH(par_pop_token(this).type, int, {
		case DANA_KW_INT : return 1;
		case DANA_KW_BYTE: return 2;
		default: _assert(false, "Invalid return type");
		});
	}

	printf("DEF:\t %s\t(%u) %.*s\n", ast_get_type_str(node_at(this, node)),
			node_at(this, node).name,
			UNSLICE(par_get_name(this, node_at(this, node).name))
			);

	if (par_peek_token(this).type == DANA_COLON) {
		do {
			par_pop_token(this);
			dbg(lex_get_type_str(par_peek_token(this)), "%s");
			node_at(this, node).length += node_at(this,
					parse_var(this, DANA_KW_AS)).length;
		} while (par_peek_token(this).type == DANA_COMMA);
	} else {
		node_at(this, node).length += 1;
		par_emplace_node(this, .type = AST_VARS);
	}

	if (to_match == DANA_KW_DEF) {
		node_at(this, node).length +=
			node_at(this, parse_local_defs(this)).length;
		node_at(this, node).length +=
			node_at(this, parse_block(this)).length;
	}

	return node;
}

ast_node_pos
parse_expr (parser_t *this, uint8_t thrs)
{
	ast_node_pos node = {0};
	lex_token_t tok = par_pop_token(this);

	dbg(lex_get_type_str(tok), "EXPR:\tfirst %s");
	if (tok.type == DANA_NUMBER) {
		slice_char_t sl = par_get_value(this, tok);
		SLICE_TMP_STR(sl);
		node = par_emplace_node(this,
			.type = AST_NUMBER, .src = tok.pos,
			.pl_data.num = atoi(sl.ptr),
		);
	} else node = SWITCH(tok.type, ast_node_pos, {
	/* literal */
	case DANA_KW_TRUE ... DANA_KW_FALSE:
		return par_emplace_node(this, .type = tok.type, .src = tok.pos);
	case DANA_CHAR:
		return par_emplace_node(this,
			.type = AST_CHAR, .src = tok.pos,
			.pl_data.ch = lex_get_char(&this->lexer, tok),
		);
	/* lvalue */
	case DANA_STRING:
	case DANA_NAME:
		this->has_peeked = true; /* unpop token */
		return parse_lvalue(this);
	/* prefix operators */
	case DANA_OPEN_PAREN:
		node = parse_expr(this, 0);
		par_pop_require(this, DANA_CLOSE_PAREN);
		return node;
	default:
		_assert(bp_operator_is(tok, PREFIX), "Invalid start of expr");
		node = par_emplace_node(this, .type = tok.type, .src = tok.pos);
		node_at(this, node).length += node_at(this,
				parse_expr(this, bp_rhs(tok, PREFIX))).length;
		return node;
	});

	while (par_peek_token(this).type != DANA_EOF) {
		tok = par_peek_token(this);
		dbg(lex_get_type_str(tok), "EXPR:\tloop %s");

		if (bp_operator_is(tok, POSTFIX)) {
			if (bp_lhs(tok, POSTFIX) < thrs) break;
			par_pop_token(this);
			if (tok.type == DANA_OPEN_PAREN) {
				_assert(node_at(this, node).type == AST_NAME,
						"Func-call must be done on name");
				uint16_t id = node_at(this, node).name;

				arr_pop(this->nodes); /* remove node */
				node = par_emplace_node(this,
					.type = AST_FUNC_CALL,
					.src = tok.pos,
					.name = id,
				);
				node_at(this, node).length += node_at(this,
						parse_args(this, AST_FUNC_CALL)).length;
				par_pop_require(this, DANA_CLOSE_PAREN);
			} else {
				uint32_t length = node_at(this, node).length;
				arr_ins(this->nodes, node.pos, ((ast_node_t){
					.type = tok.type,
					.src = tok.pos,
					.length = 1 + length,
				}));
			}
		} else if (bp_operator_is(tok, INFIX)) {
			if (bp_lhs(tok, INFIX) < thrs) break;
			par_pop_token(this);
			uint32_t length = node_at(this, node).length;
			arr_ins(this->nodes, node.pos, ((ast_node_t){
				.type = tok.type,
				.src = tok.pos,
				.length = 1 + length,
			}));
			node_at(this, node).length += node_at(this,
					parse_expr(this, bp_rhs(tok, INFIX))).length;
		} else {
			dbg(lex_get_type_str(tok), "EXPR:\tbreaking at %s");
		}
		break;
	}

	return node;
}

ast_node_pos
parse_local_defs (parser_t *this)
{
	ast_node_pos node = par_emplace_node(this,
		.type = AST_LOCAL_DEF, .src = par_peek_token(this).pos,
	);
	enum lex_type type;

	while (true) {
		switch ((type = par_peek_token(this).type)) {
		case DANA_KW_DECL:
		case DANA_KW_DEF:
			node_at(this, node).length += node_at(this,
					parse_decl(this, type)).length;
			continue;
		case DANA_KW_VAR:
			par_pop_token(this);
			node_at(this, node).length += node_at(this,
					parse_var(this, DANA_KW_IS)).length;
			continue;
		default:
		}
		break;
	}

	if (node_at(this, node).length == 1)
		node_at(this, node).src = (lex_buf_pos){0};
	return node;
}

ast_node_pos
parse_lvalue (parser_t *this)
{
	ast_node_pos node;
	lex_token_t tok = par_pop_token(this);

	node = SWITCH(tok.type, ast_node_pos, {
	case DANA_NAME:
		return par_emplace_node(this,
			.type = AST_NAME, .src = tok.pos,
			.name = par_push_name(this, tok)
		);
	case DANA_STRING:
		return par_emplace_node(this,
			.type = AST_STRING, .src = tok.pos,
			.pl_data.str = par_push_text(this, tok)
		);
	default:
		_assert(false, "Invalid token for begin of lvalue: %s",
				lex_get_type_str(tok));
	});

	if (par_peek_token(this).type == DANA_OPEN_BRACKET) {
		_assert(node_at(this, node).type == AST_NAME,
				"Only names have [] operator");
		node_at(this, node).type = AST_ARRAY_AT;

		while (par_pop_token(this).type == DANA_OPEN_BRACKET) {
			node_at(this, node).length += par_get_node(
					this, parse_expr(this, 0)
					).length;
			par_pop_require(this, DANA_CLOSE_BRACKET);
		}
	}

	return node;
}

ast_node_pos
parse_stmt (parser_t *this)
{
	ast_node_pos node;
	lex_token_t tok;
	uint16_t id, length = 0;

	if (par_peek_token(this).type == DANA_KW_IF) return parse_cond(this);
	else switch ((tok = par_pop_token(this)).type) {
	case DANA_KW_SKIP:
	case DANA_KW_EXIT:
		return par_emplace_node(this, .type = tok.type, .src = tok.pos);
	case DANA_KW_RETURN:
		par_pop_require(this, DANA_COLON);
		node = par_emplace_node(this, .type = AST_RETURN, .src = tok.pos);
		length = node_at(this, parse_expr(this, 0)).length;
		node_at(this, node).length += length;
		return node;
	case DANA_KW_BREAK:
	case DANA_KW_CONT:
		node = par_emplace_node(this, .type = tok.type, .src = tok.pos);
		if (par_peek_token(this).type == DANA_COLON) {
			par_pop_token(this);
			id = par_push_name(this, par_pop_token(this));
			node_at(this, node).name = id;
		}
		return node;
	case DANA_KW_LOOP:
		node = par_emplace_node(this, .type = AST_LOOP, .src = tok.pos);
		if (par_peek_token(this).type == DANA_NAME) {
			id = par_push_name(this, par_pop_token(this));
			node_at(this, node).name = id;
		}
		par_pop_require(this, DANA_COLON);
		length = node_at(this, parse_block(this)).length;;
		node_at(this, node).length += length;
		return node;
	/* assign - proc-call */
	case DANA_NAME:
		this->has_peeked = true; /* turn previous pop into peek */
		node = parse_lvalue(this);

		switch (par_peek_token(this).type) {
		case DANA_ASSIGN:
			length = node_at(this, node).length;
			arr_ins(this->nodes, node.pos, ((ast_node_t){
				.type = AST_ASSIGN,
				.src = par_pop_token(this).pos,
				.length = 1 + length,
			}));
			length = node_at(this, parse_expr(this, 0)).length;
			node_at(this, node).length += length;
			return node;
		case DANA_COLON:
			_assert(node_at(this, node).type == AST_NAME,
					"proc-call must be on name");
			par_pop_token(this);
			length = node_at(this,
					parse_args(this, AST_PROC_CALL)).length;
		default:
			id = node_at(this, node).name;
			node_at(this, node) = (ast_node_t){
				.type = AST_PROC_CALL,
				.src = tok.pos,
				.pl_data.name = id,
				.length = 1 + length,
			};
			return node;
		}
	default: _assert(false, "Invalid token at start of stmt");
	}
}

ast_node_pos
parse_var (parser_t *this, enum lex_type to_match)
{
	ast_node_pos node = par_emplace_node(this,
		.type = AST_VARS, .src = par_peek_token(this).pos,
	);
	ast_node_t template = { .length = 1 };
	lex_token_t tok;
	enum dtype arr_type;
	uint16_t *names = {0}, *dimen = {0};
	lex_buf_pos *pos = {0};

	do {
		arr_push(pos, par_peek_token(this).pos);
		arr_push(names, par_push_name(this,
					par_pop_require(this, DANA_NAME)));
	} while (par_peek_token(this).type == DANA_NAME);
	par_pop_require(this, to_match);
	/* base types */
	switch ((tok = par_pop_token(this)).type) {
	case DANA_KW_REF:
		_assert(to_match == DANA_KW_AS, "ref not allowed in this type def");
		switch((tok = par_pop_token(this)).type) {
		case DANA_KW_INT:
			template.type = AST_REF_INT;
			break;
		case DANA_KW_BYTE:
			template.type = AST_REF_BYTE;
			break;
		default: _assert(false, "Invalid ref type");
		}
		break;
	case DANA_KW_INT ... DANA_KW_BYTE:
		template.type = tok.type;
		break;
	default: _assert(false, "Invalid var type");
	}
	/* array */
	if (template.type != AST_ERROR &&
			par_peek_token(this).type == DANA_OPEN_BRACKET) {
		arr_type = DTYPE_ARRAY | (
			template.type == AST_INT ? DTYPE_INT :
			template.type == AST_BYTE ? DTYPE_BYTE :
			(_assert(false, "Cannot make ref of array"), 0)
		);
		template.type = AST_ARRAY;

		while (par_peek_token(this).type == DANA_OPEN_BRACKET) {
			par_pop_token(this);
			switch ((tok = par_pop_token(this)).type) {
			case DANA_CLOSE_BRACKET:
				_assert(arr_empty(dimen), "Only first dimension can be variable");
				printf("\t\tVar-array\n");
				arr_type |= DTYPE_VAR;
				arr_push(dimen, 0);
				break;
			case DANA_NUMBER:
				{
					slice_char_t sl = par_get_value(this, tok);
					SLICE_TMP_STR(sl);
					arr_push(dimen, atoi(sl.ptr));
				}
				dbg(arr_back(dimen), "%u");
				par_pop_require(this, DANA_CLOSE_BRACKET);
				break;
			default: _assert(false, "Invalid arr-def subscript type");
			}
		}

		template.var_data.array = par_push_type(this, (dtype_t){
			.type = arr_type & (~DTYPE_ARRAY) & (~DTYPE_VAR)
		});
		for (; !arr_empty(dimen); arr_pop(dimen)) {
			template.var_data.array = par_push_type(this, (dtype_t){
				.type = arr_ulen(dimen) == 1
					? arr_type : arr_type & (~DTYPE_VAR),
				.dim = arr_back(dimen),
				.next = template.var_data.array,
			});
		}
	}

	for (uint32_t i=0; i<arr_ulen(names); ++i) {
		template.name = names[i];
		template.src = pos[i];
		node_at(this, node).length += node_at(this,
				par_push_node(this, template)).length;
	}

	hm_free(names), hm_free(dimen), hm_free(pos);
	return node;
}
#pragma endregion
#endif
