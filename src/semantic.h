#pragma once

#ifdef SEM_IMPLEMENT
#	define DA_IMPLEMENT
#	define STACK_IMPLEMENT
#endif

#include "parser.h"
#include "util.h"
#include "util/dynamic_array.h"
#include "util/stack.h"

#pragma region TYPES
POS_DECL(st_arg_pos, 16);

// TODO: add ref checking
typedef struct {
	enum dtype type;
	bool is_position : 1;
	union {
		dtype_pos pos;
		struct { st_arg_pos begin; uint8_t count; } args;
		uint16_t array_length;
	};
} st_type_t;

#define DTYPE_POS(par, p)    (st_type_t){ \
	.type = par_get_type(par, p).type, .is_position = true, .pos = (p) \
}
#define DTYPE_INLINE(t, ...) (st_type_t){ .type = (t), __VA_OPT__(__VA_ARGS__) }
#define DTYPE_ISNUM(t)    (((t).type == DTYPE_INT)  || ((t).type == DTYPE_BYTE) || ((t).type == DTYPE_BOOL))
#define DTYPE_ISBOOL(t)   (((t).type == DTYPE_BOOL) || ((t).type == DTYPE_BYTE))
#define DTYPE_ARR_TYPE(t) ((t).type & ~DTYPE_VAR_ARRAY)

// TODO: add declaration positions
typedef struct {
	uint16_t key;
	st_type_t value;
} st_cell_t;

typedef st_cell_t *st_scope_t; /* Hash Map u16 -> st_type_t */

typedef struct {
	st_scope_t *scopes; /* stack of scopes */
	st_type_t  *defs;   /* stack of open definitions (funcs, loops) */
	st_type_t  *args;   /* dynamic array of function arguments */
} sym_table_t;
#pragma endregion

#pragma region SYMBOL TABLE
#define ST_CLEANUP __attribute__((cleanup(sym_table_destroy)))
extern sym_table_t sym_table_create(void);
extern void        sym_table_destroy(sym_table_t *this);
extern void        sem_add_base_lib(const parser_t *this, sym_table_t *st);
extern st_type_t   st_get_symbol(const sym_table_t *this, uint16_t name);
extern st_type_t   st_get_last_def(const sym_table_t *this, enum dtype type);
extern st_type_t   st_get_arg(const sym_table_t *this, st_arg_pos pos);
extern void        st_pop_def(sym_table_t *this);
extern void        st_pop_scope(sym_table_t *this);
extern void        st_push_arg(sym_table_t *this, st_type_t *func, st_type_t arg);
extern void        st_push_def(sym_table_t *this, st_type_t def);
extern void        st_push_scope(sym_table_t *this, st_type_t def);
extern bool        st_push_symbol(sym_table_t *this, uint16_t name, st_type_t type);
extern void        st_rename_scope(sym_table_t *this, st_type_t def);
extern bool        st_scope_has_symbol(const sym_table_t *this, uint16_t name);
#pragma endregion

#pragma region SEMANTIC CHECK
extern bool         sem_check(const parser_t *this, ast_node_pos pos);
extern bool        _sem_check(const parser_t *this, sym_table_t *st, ast_node_pos pos);
extern bool         sem_check_args(const parser_t *this, sym_table_t *st, ast_node_pos pos);
extern bool         sem_check_cond(const parser_t *this, sym_table_t *st, ast_node_pos pos);
extern bool         sem_check_decl(const parser_t *this, sym_table_t *st, ast_node_pos pos);
extern bool         sem_check_def(const parser_t *this, sym_table_t *st, ast_node_pos pos);
extern st_type_t    sem_eval_expr(const parser_t *this, sym_table_t *st, ast_node_pos pos);
/* utility methods */
extern st_type_t    sem_get_node_type(const parser_t *this, const sym_table_t *st, ast_node_t node);
extern const char*  sem_get_type_str(st_type_t type);
extern bool         sem_type_eq(const parser_t *this, const sym_table_t *st, st_type_t a, st_type_t b);
#pragma endregion

#pragma region DEBUGGING
#ifdef SEM_DEBUG
	#define log(fmt,...) \
		printf("SEM: " fmt "\n" __VA_OPT__(,) __VA_ARGS__)
#else
	#define log(...)
#endif
#pragma endregion

#ifdef SEM_IMPLEMENT
// #pragma GCC diagnostic push
// #pragma GCC diagnostic ignored "-Wunused-variable"

#pragma region BASE LIBRARY

#define PROC(name, ...)  #name,
#define IFUNC(name, ...) #name,
#define BFUNC(name, ...) #name,
static const char* st_lib_names[] = { DANA_LIBRARY };
#undef PROC
#undef IFUNC
#undef BFUNC

#define INT    DTYPE_INLINE(DTYPE_INT)
#define BYTE   DTYPE_INLINE(DTYPE_BYTE)
#define STRING DTYPE_INLINE(DTYPE_VAR_ARRAY | DTYPE_BYTE)
#define PROC(name, ...)  { .func = DTYPE_INLINE(DTYPE_FUNC),             .args = { __VA_ARGS__ } },
#define IFUNC(name, ...) { .func = DTYPE_INLINE(DTYPE_FUNC | DTYPE_INT), .args = { __VA_ARGS__ } },
#define BFUNC(name, ...) { .func = DTYPE_INLINE(DTYPE_FUNC | DTYPE_INT), .args = { __VA_ARGS__ } },
static struct {
	st_type_t func;
	st_type_t args[2];
} st_lib_types[] = { DANA_LIBRARY };
#undef INT
#undef BYTE
#undef STRING
#undef PROC
#undef IFUNC
#undef BFUNC

void
sem_add_base_lib (const parser_t *this, sym_table_t *st)
{
	/* push used functions */
	bool used[LENGTH(st_lib_names)];
	memset(used, 0, sizeof(used));

	for (size_t i=0; i<hm_ulen(this->names); ++i) {
		for (size_t j=0; j<LENGTH(st_lib_names); ++j) if (!used[j]) {
			if (strlen(st_lib_names[j]) != this->names[i].decl.length)
				continue;
			if (memcmp(this->names[i].decl.ptr, st_lib_names[j],
						this->names[i].decl.length))
				continue;
			used[j] = true;
			log("Adding library function %s...", st_lib_names[j]);
			log("found at this->names[%u]", this->names[i].value);
			if (st_lib_types[j].args[0].type)
				st_push_arg(st, &st_lib_types[j].func, st_lib_types[j].args[0]);
			if (st_lib_types[j].args[1].type)
				st_push_arg(st, &st_lib_types[j].func, st_lib_types[j].args[1]);
			st_push_symbol(st, this->names[i].value, st_lib_types[j].func);
			break;
		}
	}
}
#pragma endregion

#pragma region SYMBOL TABLE
sym_table_t
sym_table_create (void)
{
	sym_table_t ret = {0};
	stack_push(ret.scopes, (st_scope_t){0});
	stack_push(ret.defs,   (st_type_t) {0});
	arr_push(ret.args,     (st_type_t) {0});
	return ret;
}

void
sym_table_destroy (sym_table_t *this)
{
	stack_free(this->scopes);
	stack_free(this->defs);
	arr_free(this->args);
}

st_type_t __attribute__((pure))
st_get_symbol (const sym_table_t *this, uint16_t name)
{
	ptrdiff_t idx;
	for (st_scope_t *u = this->scopes; u; u = stack_next(u))
		if ((idx = hm_geti(*u, name)) != -1)
			return (*u)[idx].value;
	return (st_type_t){0};
}

inline st_type_t __attribute__((pure))
st_get_last_def (const sym_table_t *this, enum dtype type)
{
	for (st_type_t *u = this->defs; u; u = stack_next(u))
		if (u->type & type) return *u;
	return (st_type_t){0};
}

inline st_type_t __attribute__((pure))
st_get_arg (const sym_table_t *this, st_arg_pos pos)
{
	_assert(0 <= pos.pos && pos.pos < arr_ulen(this->args),
			"Arg index %u out of bounds [0, %lu]",
			pos.pos, arr_ulen(this->args));
	return this->args[pos.pos];
}

inline void
st_pop_def (sym_table_t *this)
{
	stack_pop(this->defs);
}

inline void
st_pop_scope (sym_table_t *this)
{
	stack_pop(this->scopes); stack_pop(this->defs);
}

inline void __attribute__((nonnull()))
st_push_arg (sym_table_t *this, st_type_t *func, st_type_t arg)
{
	arr_push(this->args, arg);
	if (!POS_OK(func->args.begin))
		func->args.begin = (st_arg_pos){ arr_ulen(this->args) - 1 };
	func->args.count += 1;
}

inline void
st_push_def (sym_table_t *this, st_type_t def)
{
	stack_push(this->defs, def);
}

inline void
st_push_scope (sym_table_t *this, st_type_t def)
{
	stack_push(this->scopes, (st_scope_t){0}); stack_push(this->defs, def);
}

inline bool
st_push_symbol (sym_table_t *this, uint16_t name, st_type_t type)
{
	if (st_scope_has_symbol(this, name)) return false;
	hm_put(stack_top(this->scopes), name, type);
	return true;
}

inline void
st_rename_scope (sym_table_t *this, st_type_t def)
{
	stack_top(this->defs) = def;
}

inline bool __attribute__((pure))
st_scope_has_symbol (const sym_table_t *this, uint16_t name)
{
	return hm_geti(stack_top(this->scopes), name) != -1;
}
#pragma endregion

#pragma region SEMANTIC CHECK
inline bool
sem_check (const parser_t *this, ast_node_pos pos)
{
	sym_table_t ST_CLEANUP st = sym_table_create();
	sem_add_base_lib(this, &st);
	bool ret;
	log("BEGIN CHECK");
	ret = _sem_check(this, &st, pos);
	log("END CHECK");
	return ret;
}

bool
_sem_check (const parser_t *this, sym_table_t *st, ast_node_pos pos)
{
#define CHECK_CHLD() for (; ast_is_child(it); it = ast_next_child(it)) \
	try(_sem_check(this, st, it.pos), PAR_FSTR, PAR_FPOS(this, node))

	ast_node_t node = node_at(this, pos);
	ast_node_it it = ast_get_child(this, pos);
	st_type_t lhs, rhs;

	log(">>> " PAR_FSTR "%s", PAR_FPOS(this, node), ast_get_type_str(node));

	switch (node.type) {
	/* scope-creating nodes */
	break; case AST_BLOCK:
	       case AST_INDENT:
		CHECK_CHLD();
	break; case AST_COND:
		try(sem_check_cond(this, st, pos), PAR_FSTR, PAR_FPOS(this, node));
	break; case AST_LOOP:
		lhs = sem_get_node_type(this, st, node);
		if (node.name)
			throw_if(st_get_symbol(st, node.name).type != DTYPE_NONE,
				bool,
				PAR_FSTR "loop name %.*s already being used in current scope",
				PAR_FPOS(this, node),
				UNSLICE(par_get_name(this, node))
				);
		st_push_scope(st, lhs);
		if (node.name) st_push_symbol(st, node.name, lhs);
		try(_sem_check(this, st, it.pos), PAR_FSTR, PAR_FPOS(this, node));
		st_pop_scope(st);
	break; case AST_DECL_PROC ... AST_DECL_BYTE:
	       case AST_DEF_PROC ... AST_DEF_BYTE:
		if (AST_DEF_PROC <= node.type && node.type <= AST_DEF_BYTE)
			try(sem_check_def(this, st, pos), PAR_FSTR, PAR_FPOS(this, node));
		else
			try(sem_check_decl(this, st, pos), PAR_FSTR, PAR_FPOS(this, node));
	/* variable definitions */
	break; case AST_INT ... AST_BYTE:
	       case AST_REF_INT ... AST_REF_BYTE:
	       case AST_ARRAY:
		try_typed(st_push_symbol(st, node.name,
					sem_get_node_type(this, st, node)), bool,
				PAR_FSTR "varialble %.*s already defined",
				PAR_FPOS(this, node),
				UNSLICE(par_get_name(this, node)));
	/* statements */
	break; case AST_SKIP:
	break; case AST_EXIT:
		throw_if(st_get_last_def(st, DTYPE_FUNC).type
				& (DTYPE_INT | DTYPE_BYTE), bool,
				PAR_FSTR "%s must be called inside a procedure definition",
				PAR_FPOS(this, node), ast_get_type_str(node));
	break; case AST_BREAK: case AST_CONT:
		throw_if(st_get_last_def(st, DTYPE_LOOP).type == DTYPE_NONE, bool,
				PAR_FSTR "%s must be called inside a loop body",
				PAR_FPOS(this, node), ast_get_type_str(node));
		if (node.name) throw_if(
				st_get_symbol(st, node.name).type != DTYPE_LOOP,
				bool,
				PAR_FSTR "name %.*s does not correspond to a loop",
				PAR_FPOS(this, node),
				UNSLICE(par_get_name(this, node))
				);
	break; case AST_RETURN:
		lhs = try_typed(sem_eval_expr(this, st, it.pos), bool,
				PAR_FSTR, PAR_FPOS(this, *it.node));
		rhs = st_get_last_def(st, DTYPE_FUNC); rhs.type &= ~DTYPE_FUNC;
		throw_if(!sem_type_eq(this, st, lhs, rhs), bool,
				PAR_FSTR "return value of type %s cannot be casted to expected type %s",
				PAR_FPOS(this, node),
				sem_get_type_str(lhs), sem_get_type_str(rhs));
	/* procedure call */
	break; case AST_PROC_CALL:
		lhs = st_get_symbol(st, node.name);
		throw_if((lhs.type & DTYPE_FUNC) == 0, bool,
				PAR_FSTR "no procedure named %.*s in scope",
				PAR_FPOS(this, node),
				UNSLICE(par_get_name(this, node)));
		throw_if(lhs.type & (DTYPE_INT | DTYPE_BYTE), bool,
				PAR_FSTR "%.*s previously declared as a function",
				PAR_FPOS(this, node),
				UNSLICE(par_get_name(this, node)));
		try(sem_check_args(this, st, pos),
				PAR_FSTR "in procedure call for %.*s",
				PAR_FPOS(this, node),
				UNSLICE(par_get_name(this, node)));
	break; case AST_ASSIGN:
		lhs = try_typed(sem_eval_expr(this, st, it.pos), bool,
				PAR_FSTR, PAR_FPOS(this, node));
		it = ast_next_child(it);
		rhs = try_typed(sem_eval_expr(this, st, it.pos), bool,
				PAR_FSTR, PAR_FPOS(this, node));
		throw_if(!sem_type_eq(this, st, rhs, lhs), bool,
				PAR_FSTR "type mismatch on assignment: %s cannot be converted to %s",
				PAR_FPOS(this, node),
				sem_get_type_str(lhs), sem_get_type_str(rhs)
				);
	break; default:
		log("\tPENDING...");
	}

	log("<<< " PAR_FSTR "%s", PAR_FPOS(this, node), ast_get_type_str(node));
	return true;
#undef CHECK_CHLD
}

bool
sem_check_args (const parser_t *this, sym_table_t *st, ast_node_pos pos)
{
	uint16_t argc = 0;
	ast_node_t node = par_get_node(this, pos);
	ast_node_it it = ast_get_child(this, pos);
	st_type_t func = st_get_symbol(st, node.name);
	st_arg_pos arg_proto = func.args.begin;
	st_type_t arg;

	for (; ast_is_child(it) && argc < func.args.count;
			it = ast_next_child(it), ++argc, ++arg_proto.pos) {
		arg = try_typed(sem_eval_expr(this, st, it.pos), bool,
				PAR_FSTR, PAR_FPOS(this, node));
		throw_if(!sem_type_eq(this, st, arg, st_get_arg(st, arg_proto)), bool,
				PAR_FSTR "argument type %s cannot be converted to expected type %s",
				PAR_FPOS(this, *it.node),
				sem_get_type_str(arg),
				sem_get_type_str(st_get_arg(st, arg_proto)));
	}
	throw_if(ast_is_child(it) || argc < func.args.count, bool,
			PAR_FSTR "function %.*s called with different argument count than declared",
			PAR_FPOS(this, node), UNSLICE(par_get_name(this, node)));
	return true;
}

bool
sem_check_decl (const parser_t *this, sym_table_t *st, ast_node_pos pos)
{
	ast_node_t node = par_get_node(this, pos);
	ast_node_it it = ast_get_child(this, pos);
	st_type_t func;

	throw_if(st_scope_has_symbol(st, node.name), bool,
			PAR_FSTR "tried to redeclare function named %.*s",
			PAR_FPOS(this, node),
			UNSLICE(par_get_name(this, node)));
	/* create func type */
	func = sem_get_node_type(this, st, node);
	func.args.begin = (st_arg_pos){ arr_ulen(st->args) };
	st_push_def(st, func);
	/* arguments */
	for (; ast_is_child(it); it = ast_next_child(it)) {
		try(_sem_check(this, st, it.pos), PAR_FSTR, PAR_FPOS(this, node));
		st_push_arg(st, &func, sem_get_node_type(this, st, *it.node));
	}
	st_push_symbol(st, node.name, func);

	log("--- Function decl: %.*s: %s(", UNSLICE(par_get_name(this, node)),
			sem_get_type_str(func));
	for (st_arg_pos i = func.args.begin;
			POS_DIFF(func.args.begin, i) < func.args.count;
			i = POS_ADV(i, 1))
		log("%s, ", sem_get_type_str(st_get_arg(st, i)));
	log(")\n");

	return true;
}

bool
sem_check_def (const parser_t *this, sym_table_t *st, ast_node_pos pos)
{
	ast_node_t node = par_get_node(this, pos);
	ast_node_it it = ast_get_child(this, pos);
	st_type_t func;
	size_t i;

	if ((func = st_get_symbol(st, node.name)).type != DTYPE_NONE) {
		st_push_scope(st, func);
		/* check declaration - definition matching and register args */
		for (i = 0; POS_CMP(it.pos, par_func_locals(this, pos));
				it = ast_next_child(it), ++i) {
			try(_sem_check(this, st, it.pos), PAR_FSTR, PAR_FPOS(this, node));
			throw_if(!sem_type_eq(this, st, sem_get_node_type(this, st, *it.node),
						st_get_arg(st, POS_ADV(func.args.begin, i))),
					bool,
					PAR_FSTR "type mismatch between declaration and definition of function %.*s",
					PAR_FPOS(this, *it.node),
					UNSLICE(par_get_name(this, *it.node)));
		}
		log("previously declared func: OK");
	} else {
		/* create func type and register args */
		func = sem_get_node_type(this, st, node);
		func.args.begin = (st_arg_pos){ arr_ulen(st->args) };
		st_push_scope(st, func);
		/* arguments */
		for (; POS_CMP(it.pos, par_func_locals(this, pos));
					it = ast_next_child(it)) {
			try(_sem_check(this, st, it.pos), PAR_FSTR, PAR_FPOS(this, node));
			st_push_arg(st, &func, sem_get_node_type(this, st, *it.node));
		}
		st_rename_scope(st, func);
		log("--- Function def: %.*s: %s(", UNSLICE(par_get_name(this, node)),
				sem_get_type_str(func));
		for (st_arg_pos it = func.args.begin;
				POS_DIFF(func.args.begin, it) < func.args.count;
				it = POS_ADV(it, 1))
			log("%s, ", sem_get_type_str(st_get_arg(st, it)));
		log(")\n");
	}

	/* register function in scope */
	st_push_symbol(st, node.name, func);
	/* local defs */
	for (; POS_CMP(it.pos, par_func_body(this, pos)); it = ast_next_child(it))
		try(_sem_check(this, st, it.pos), PAR_FSTR, PAR_FPOS(this, node));
	/* body */
	try(_sem_check(this, st, it.pos), PAR_FSTR, PAR_FPOS(this, node));
	st_pop_scope(st);
	/* push function for subsequent use */
	st_push_symbol(st, node.name, func);

	return true;
}

bool
sem_check_cond (const parser_t *this, sym_table_t *st, ast_node_pos pos)
{
	ast_node_t node = par_get_node(this, pos);
	ast_node_it it = ast_get_child(this, pos);
	st_type_t cond;
	bool is_cond;

	for (is_cond = true; ast_is_child(it); it = ast_next_child(it), is_cond ^= 1) {
		if (is_cond && ast_is_child(ast_next_child(it))) {
			cond = try_typed(sem_eval_expr(this, st, it.pos), bool,
					PAR_FSTR "in condition",
					PAR_FPOS(this, node));
			try(sem_type_eq(this, st, cond, DTYPE_INLINE(DTYPE_BOOL)),
					PAR_FSTR "condition must be boolean",
					PAR_FPOS(this, node));
		} else {
			try(_sem_check(this, st, it.pos),
					PAR_FSTR "in body of conditional",
					PAR_FPOS(this, node));
		}
	}
	return true;
}

st_type_t
sem_eval_expr (const parser_t *this, sym_table_t *st, ast_node_pos pos)
{
	ast_node_t node = par_get_node(this, pos);
	ast_node_it lhs = ast_get_child(this, pos),
		    rhs = ast_next_child(lhs);
	st_type_t ltype, rtype;

	switch(node.type) {

	case AST_TRUE ... AST_FALSE:
	case AST_CHAR ... AST_STRING:
	case AST_NAME:
	case AST_NUMBER:
		return sem_get_node_type(this, st, node);

	case AST_ARRAY_AT:
		throw_if(!ast_is_child(lhs), st_type_t,
				PAR_FSTR "subscript operator must have at least 1 dimension",
				PAR_FPOS(this, *lhs.node));

		st_type_t arr_type = try(st_get_symbol(st, node.name),
				PAR_FSTR "array name '%.*s' doesn't exist in current scope",
				PAR_FPOS(this, node),
				UNSLICE(par_get_name(this, node)));

		for (; ast_is_child(lhs); lhs = ast_next_child(lhs)) {
			rtype = sem_eval_expr(this, st, lhs.pos);
			throw_if((arr_type.type & DTYPE_ARRAY) == 0, st_type_t,
					PAR_FSTR "subscript operator on non-array type",
					PAR_FPOS(this, *lhs.node));
			throw_if(!DTYPE_ISNUM(rtype), st_type_t,
					PAR_FSTR "subscript index must be number",
					PAR_FPOS(this, *lhs.node));
			/* TODO: add subscript range checking */
			/* move to next type */
			arr_type = arr_type.is_position && par_get_type(this, arr_type.pos).length > 1
				? DTYPE_POS(this, POS_ADV(arr_type.pos, 1))
				: DTYPE_INLINE(DTYPE_ARR_TYPE(arr_type));
		}

		return arr_type;

	case AST_FUNC_CALL:
		rtype = st_get_symbol(st, node.name);
		throw_if((rtype.type & DTYPE_FUNC) == 0, st_type_t,
				PAR_FSTR "no procedure named %.*s in scope",
				PAR_FPOS(this, node),
				UNSLICE(par_get_name(this, node)));
		throw_if((rtype.type & (DTYPE_INT | DTYPE_BYTE)) == 0, st_type_t,
				PAR_FSTR "%.*s previously declared as a procedure",
				PAR_FPOS(this, node),
				UNSLICE(par_get_name(this, node)));
		try_typed(sem_check_args(this, st, pos), st_type_t,
				PAR_FSTR "in function call for %.*s",
				PAR_FPOS(this, node),
				UNSLICE(par_get_name(this, node)));
		return DTYPE_INLINE(rtype.type & ~DTYPE_FUNC);

	case AST_PLUS ... AST_MINUS:
		if (!ast_is_child(rhs)) {
			ltype = sem_eval_expr(this, st, lhs.pos);
			throw_if(ltype.type != DTYPE_INT, st_type_t,
					PAR_FSTR "operand of unary %s is not int",
					PAR_FPOS(this, *lhs.node),
					ast_get_type_str(node));
			return DTYPE_INLINE(DTYPE_INT);
		}
	case AST_MULT ... AST_MOD:
	case AST_CMP_EQ ... AST_CMP_GEQ:
		ltype = sem_eval_expr(this, st, lhs.pos);
		rtype = sem_eval_expr(this, st, rhs.pos);
		throw_if(!DTYPE_ISNUM(ltype), st_type_t,
				PAR_FSTR "LHS operand of operator %s is not numeric type",
				PAR_FPOS(this, *lhs.node),
				ast_get_type_str(node));
		throw_if(!DTYPE_ISNUM(rtype), st_type_t,
				PAR_FSTR "RHS operand of operator %s is not numeric type",
				PAR_FPOS(this, *rhs.node),
				ast_get_type_str(node));
		throw_if(!sem_type_eq(this, st, ltype, rtype), st_type_t,
				PAR_FSTR "operand type mismatch for operator %s",
				PAR_FPOS(this, *rhs.node),
				ast_get_type_str(node));
		return node.type <= AST_MOD ? ltype : DTYPE_INLINE(DTYPE_BOOL);

	case AST_BIT_NOT:
		ltype = sem_eval_expr(this, st, lhs.pos);
		throw_if(sem_get_node_type(this, st, *lhs.node).type != DTYPE_BYTE,
				st_type_t,
				PAR_FSTR "operand of bitwise not is not byte",
				PAR_FPOS(this, *lhs.node));
		return DTYPE_INLINE(DTYPE_BYTE);

	case AST_BOOL_NOT:
		ltype = sem_eval_expr(this, st, lhs.pos);
		throw_if(!DTYPE_ISBOOL(sem_get_node_type(this, st, *lhs.node)) ||
			 !DTYPE_ISBOOL(sem_get_node_type(this, st, *lhs.node)),
				st_type_t,
				PAR_FSTR "operand of boolean not is not boolean",
				PAR_FPOS(this, *lhs.node));
		return DTYPE_INLINE(DTYPE_BOOL);

	case AST_BOOL_AND ... AST_BOOL_OR:
		ltype = sem_eval_expr(this, st, lhs.pos);
		rtype = sem_eval_expr(this, st, rhs.pos);
		throw_if(!ast_is_child(rhs), st_type_t,
				PAR_FSTR "operator %s expected 2 operand, got 1",
				PAR_FPOS(this, *rhs.node),
				ast_get_type_str(node));
		throw_if(ltype.type != DTYPE_BOOL, st_type_t,
				PAR_FSTR "LHS operand of operator %s is not boolean",
				PAR_FPOS(this, *lhs.node),
				ast_get_type_str(node));
		throw_if(rtype.type != DTYPE_BOOL, st_type_t,
				PAR_FSTR "RHS operand of operator %s is not boolean",
				PAR_FPOS(this, *rhs.node),
				ast_get_type_str(node));
		return DTYPE_INLINE(DTYPE_BOOL);

	case AST_BIT_AND ... AST_BIT_OR:
		ltype = sem_eval_expr(this, st, lhs.pos);
		rtype = sem_eval_expr(this, st, rhs.pos);
		throw_if(ltype.type != DTYPE_BYTE, st_type_t,
				PAR_FSTR "LHS operand of operator %s is not byte",
				PAR_FPOS(this, *lhs.node),
				ast_get_type_str(node));
		throw_if(rtype.type != DTYPE_BYTE, st_type_t,
				PAR_FSTR "RHS operand of operator %s is not byte",
				PAR_FPOS(this, *rhs.node),
				ast_get_type_str(node));
		return DTYPE_INLINE(DTYPE_BYTE);

	default: return (st_type_t){0};
	}
}

inline st_type_t __attribute__((const))
sem_get_node_type (const parser_t *this, const sym_table_t *st, ast_node_t node)
{
	switch(node.type) {
	case AST_TRUE ... AST_FALSE: return DTYPE_INLINE(DTYPE_BOOL);
	case AST_CHAR     :
	case AST_BYTE     :
	case AST_REF_BYTE : return DTYPE_INLINE(DTYPE_BYTE);
	case AST_INT      :
	case AST_REF_INT  :
	case AST_NUMBER   : return DTYPE_INLINE(DTYPE_INT);
	case AST_NAME     : return try(st_get_symbol(st, node.name),
					    PAR_FSTR "variable name %.*s not found",
					    PAR_FPOS(this, node),
					    UNSLICE(par_get_name(this, node)));
	case AST_LOOP     : return DTYPE_INLINE(DTYPE_LOOP);
	case AST_STRING   : return DTYPE_INLINE(DTYPE_ARRAY | DTYPE_BYTE,
					.array_length = strlen(par_get_text(this, node.pl_data.str)));
	case AST_ARRAY    : return DTYPE_POS(this, node.var_data.array);
	case AST_DEF_PROC :
	case AST_DECL_PROC: return DTYPE_INLINE(DTYPE_FUNC);
	case AST_DEF_INT  :
	case AST_DECL_INT : return DTYPE_INLINE(DTYPE_FUNC | DTYPE_INT);
	case AST_DEF_BYTE :
	case AST_DECL_BYTE: return DTYPE_INLINE(DTYPE_FUNC | DTYPE_BYTE);
	default           : return DTYPE_INLINE(DTYPE_NONE);
	}
}

inline const char *
sem_get_type_str (st_type_t type)
{
	return dtype_get_type_str(type.type);
}

bool
sem_type_eq (const parser_t *this, const sym_table_t *st, st_type_t a, st_type_t b)
{
	log("Type equality check: %s -- %s\n", sem_get_type_str(a), sem_get_type_str(b));

	dtype_t da = a.is_position ? par_get_type(this, a.pos)
		: (dtype_t){ .type = a.type, .array_length = a.array_length, .length = 1};
	dtype_t db = b.is_position ? par_get_type(this, b.pos)
		: (dtype_t){ .type = b.type, .array_length = b.array_length, .length = 1};

	if (da.type == DTYPE_LOOP || db.type == DTYPE_LOOP)
		return false;
	if ((da.type & DTYPE_ARRAY) != (da.type & DTYPE_ARRAY))
		return false;
	if ((da.type & DTYPE_FUNC) != (da.type & DTYPE_FUNC))
		return false;
	if (da.type & DTYPE_ARRAY) {
		if (DTYPE_ARR_TYPE(da) != DTYPE_ARR_TYPE(db))
			return false;
		if (da.length != db.length)
			return false;
		if (!a.is_position)
			return db.length == 1 && (
					db.array_length == da.array_length
					|| (da.type & DTYPE_VAR)
					|| (db.type & DTYPE_VAR));
		if (!b.is_position)
			return da.length == 1 && (
					da.array_length == db.array_length
					|| (da.type & DTYPE_VAR)
					|| (db.type & DTYPE_VAR));
		for (uint16_t i=0; i<da.length; ++i) {
			da = par_get_type(this, POS_ADV(a.pos, i));
			db = par_get_type(this, POS_ADV(b.pos, i));
			if (!(da.type & DTYPE_VAR_ARRAY) &&
					!(db.type & DTYPE_VAR_ARRAY) &&
					da.array_length != db.array_length)
				return false;
		}
		return true;
	}
	if (da.type & DTYPE_FUNC) {
		if ((da.type & ~DTYPE_FUNC) != (db.type & ~DTYPE_FUNC))
			return false;
		if (a.args.count != b.args.count)
			return false;
		for (uint16_t i=0; i<a.args.count; ++i)
			if (!sem_type_eq(this, st,
						st_get_arg(st, POS_ADV(a.args.begin, i)),
						st_get_arg(st, POS_ADV(b.args.begin, i))
					))
				return false;
		return true;
	}
	if ((da.type == DTYPE_BYTE && db.type == DTYPE_BOOL) ||
			(da.type == DTYPE_BOOL && db.type == DTYPE_BYTE))
		return true;
	return da.type == db.type;
}
#pragma endregion

// #pragma GCC diagnostic pop
#endif
