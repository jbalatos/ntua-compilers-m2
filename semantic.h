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
extern st_type_t  st_get_symbol(const sym_table_t *this, uint16_t name);
extern st_type_t  st_get_last_def(const sym_table_t *this, enum dtype type);
extern st_type_t  st_get_arg(const sym_table_t *this, st_arg_pos pos);
extern void       st_pop_def(sym_table_t *this);
extern void       st_pop_scope(sym_table_t *this);
extern void       st_push_args(sym_table_t *this, st_type_t *func, const parser_t *par, ast_node_t node);
extern void       st_push_def(sym_table_t *this, st_type_t def);
extern void       st_push_scope(sym_table_t *this, st_type_t def);
extern uint8_t    st_push_symbol(sym_table_t *this, uint16_t name, st_type_t type);
extern bool       st_scope_has_symbol(const sym_table_t *this, uint16_t name);
#pragma endregion

#pragma region SEMANTIC CHECK
extern bool         sem_check(const parser_t *this, ast_node_pos pos);
extern bool        _sem_check(const parser_t *this, sym_table_t *st, ast_node_pos pos);
extern bool         sem_check_decl(const parser_t *this, sym_table_t *st, ast_node_pos pos);
extern bool         sem_check_def(const parser_t *this, sym_table_t *st, ast_node_pos pos);
extern st_type_t    sem_get_node_type(const parser_t *this, ast_node_t node);
extern const char*  sem_get_type_str(st_type_t type);
extern bool         sem_type_eq(st_type_t a, st_type_t b);
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
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"

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
st_push_args (sym_table_t *this, st_type_t *func, const parser_t *par, ast_node_t node)
{
	arr_push(this->args, arg);
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

inline uint8_t
st_push_symbol (sym_table_t *this, uint16_t name, st_type_t type)
{
	if (st_scope_has_symbol(this, name)) return 1;
	hm_put(stack_top(this->scopes), name, type);
	return 0;
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
	dtype_t lhs, rhs;
	dtype_pos arg;
	(void)st;

	log("checking %s at " PAR_FSTR,
			ast_get_type_str(node), PAR_FPOS(this, node));
	switch (node.type) {
// 	/* grouping nodes */
// 	case AST_ARGS:
// 		arg = st->proto;
// 		for (; ast_is_child(it);
// 				it = ast_next_child(it),
// 				arg = POS_ADV(arg, 1)) {
// 			rhs = sem_eval_expr(this, st, it.pos);
// 			throw_if(rhs.type == DTYPE_NONE,
// 					bool,
// 					PAR_FSTR "cannot find variable %.*s in current scope",
// 					PAR_FPOS(this, *it.node),
// 					UNSLICE(par_get_name(this, *it.node)));
// 			throw_if(!sem_converts_to(rhs, par_get_type(this, arg)),
// 					bool,
// 					PAR_FSTR "variable type %s cannot be converted to required %s for argument",
// 					PAR_FPOS(this, *it.node),
// 					dtype_get_type_str(rhs),
// 					dtype_get_type_str(par_get_type(this, arg)));
// 		}
// 	break; case AST_VARS:
// 		for(; ast_is_child(it); it = ast_next_child(it))
// 			throw_if(st_emplace_symbol(st, it.node->name,
// 						.type = sem_get_dtype(this, *it.node),
// 					), bool,
// 					PAR_FSTR "variable name %.*s already used in current scope",
// 					PAR_FPOS(this, *it.node),
// 					UNSLICE(par_get_name(this, *it.node)));
// 	break; case AST_LOCAL_DEF:
// 		CHECK_CHLD();
// 	// /* scope-creating nodes */
// 	break; case AST_BLOCK:
// 	       case AST_INDENT:
// 		log("-- %sblock begin", node.type == AST_INDENT ? "indent " : "");
// 		st_push_scope(st);
// 		CHECK_CHLD();
// 		log("-- %sblock end", node.type == AST_INDENT ? "indent " : "");
// 		st_pop_scope(st);
	break; case AST_DECL_PROC ... AST_DECL_BYTE:
	       case AST_DEF_PROC ... AST_DEF_BYTE:
		throw_if(st_scope_has_symbol(st, node.name), bool,
				PAR_FSTR "function name %.*s is already being used in current scope",
				PAR_FPOS(this, node),
				UNSLICE(par_get_name(this, node)));
		if (AST_DEF_PROC <= node.type && node.type <= AST_DEF_BYTE)
			try(sem_check_def(this, st, pos), PAR_FSTR, PAR_FPOS(this, node));
		else
			try(sem_check_decl(this, st, pos), PAR_FSTR, PAR_FPOS(this, node));
// 	/* statements */
// 	break; case AST_SKIP:
// 	break; case AST_EXIT:
// 		throw_if(st_last_func(st).type & (DTYPE_INT | DTYPE_BYTE),
// 				bool,
// 				PAR_FSTR "exit cannot be called on function body",
// 				PAR_FPOS(this, node));
// 	break; case AST_BREAK: case AST_CONT:
// 		if (!node.name) break;
// 		throw_if(st_get_symbol(st, node.name).type != DTYPE_LOOP,
// 				bool,
// 				PAR_FSTR "name %.*s does not correspond to a loop",
// 				PAR_FPOS(this, node),
// 				UNSLICE(par_get_name(this, node))
// 				);
// 	break; case AST_LOOP:
// 		throw_if(st_emplace_symbol(st, node.name, .type = DTYPE_LOOP),
// 				bool,
// 				PAR_FSTR "loop name %.*s already being used in current scope",
// 				PAR_FPOS(this, node),
// 				UNSLICE(par_get_name(this, node))
// 				);
// 		try(_sem_check(this, st, it.pos), PAR_FSTR, PAR_FPOS(this, node));
// 	break; case AST_ASSIGN:
// 		lhs = sem_eval_expr(this, st, it.pos); it = ast_next_child(it);
// 		rhs = sem_eval_expr(this, st, it.pos);
// 		throw_if(sem_converts_to(rhs, lhs) == DTYPE_NONE, bool,
// 				PAR_FSTR "type mismatch on assignment: %s cannot be converted to %s",
// 				PAR_FPOS(this, node),
// 				sem_get_type_str(lhs), sem_get_type_str(rhs)
// 				);
// 	break; case AST_RETURN:
// 		rhs = sem_eval_expr(this, st, it.pos);
// 		throw_if(!sem_converts_to(rhs, st_last_func(st)), bool,
// 				PAR_FSTR "return type %s cannot be casted to expected type %s",
// 				PAR_FPOS(this, node),
// 				dtype_get_type_str(rhs),
// 				dtype_get_type_str(st_last_func(st)));
// 	break; case AST_PROC_CALL:
// 		lhs = st_get_symbol(st, node.name);
// 		throw_if((lhs.type & DTYPE_FUNC) == 0, bool,
// 				PAR_FSTR "no procedure named %.*s in scope",
// 				PAR_FPOS(this, node),
// 				UNSLICE(par_get_name(this, node))
// 				);
// 		throw_if(lhs.type & (DTYPE_INT | DTYPE_BYTE), bool,
// 				PAR_FSTR "procedure %.*s has return value",
// 				PAR_FPOS(this, node),
// 				UNSLICE(par_get_name(this, node))
// 				);
// 		st->proto = lhs.func_def;
// 		CHECK_CHLD();
// 	break; case AST_COND:
// 		try(_sem_check_cond(this, st, it.pos), PAR_FSTR, PAR_FPOS(this, node));
	break; default:
		log("%s - PENDING...", ast_get_type_str(node));
	}
	return true;
#undef CHECK_CHLD
}

bool
sem_check_decl (const parser_t *this, sym_table_t *st, ast_node_pos pos)
{
	ast_node_t node = par_get_node(this, pos);
	ast_node_it it = ast_get_child(this, pos);
	st_type_t func;

	/* create func type */
	func = sem_get_node_type(this, node);
	func.args.begin = (st_arg_pos){ arr_ulen(st->args) };
	st_push_def(st, func);
	/* arguments */
	for (it = ast_get_child(this, pos);
			POS_DIFF(pos, it.pos) < node.decl_data.body_offset;
			it = ast_next_child(it)) {
		try(_sem_check(this, st, it.pos), PAR_FSTR, PAR_FPOS(this, node));
		st_push_arg(st, &func, sem_get_node_type(this, *it.node));
	}
	st_push_symbol(st, node.name, func);

	log("--- Function decl: %.*s: %s(", UNSLICE(par_get_name(this, node)),
			sem_get_type_str(func));
	for (st_arg_pos it = func.args.begin;
			POS_DIFF(func.args.begin, it) < func.args.count;
			it = POS_ADV(it, 1))
		printf("%s, ", sem_get_type_str(st_get_arg(st, it)));
	printf(")\n");

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
		/* check declaration - definition matching */
		for (it = ast_get_child(this, pos), i = 0;
				POS_DIFF(pos, it.pos) < node.decl_data.body_offset;
				it = ast_next_child(it), ++i)
			throw_if(!sem_type_eq(sem_get_node_type(this, *it.node),
						st_get_arg(st, POS_ADV(func.args.begin, i))),
					bool,
					PAR_FSTR "type mismatch between declaration and definition of function",
					PAR_FPOS(this, *it.node));
	} else {
		/* create func type */
		func = sem_get_node_type(this, node);
		func.args.begin = (st_arg_pos){ arr_ulen(st->args) };
		/* arguments */
		for (it = ast_get_child(this, pos);
				POS_DIFF(pos, it.pos) < node.decl_data.body_offset;
				it = ast_next_child(it)) {
			try(_sem_check(this, st, it.pos), PAR_FSTR, PAR_FPOS(this, node));
			st_push_args(st, &func, this, *it.node);
		}
		log("--- Function def: %.*s: %s(", UNSLICE(par_get_name(this, node)),
				sem_get_type_str(func));
		for (st_arg_pos it = func.args.begin;
				POS_DIFF(func.args.begin, it) < func.args.count;
				it = POS_ADV(it, 1))
			printf("%s, ", sem_get_type_str(st_get_arg(st, it)));
		printf(")\n");
	}
	st_push_def(st, func);
	st_push_symbol(st, node.name, func);
	/* local defs */
	try(_sem_check(this, st, it.pos), PAR_FSTR, PAR_FPOS(this, node));
	it = ast_next_child(it);
	/* body */
	try(_sem_check(this, st, it.pos), PAR_FSTR, PAR_FPOS(this, node));
	st_pop_scope(st);

	return true;
}

inline st_type_t __attribute__((const))
sem_get_node_type (const parser_t *this, ast_node_t node)
{
	switch(node.type) {
	case AST_TRUE ... AST_FALSE:
	case AST_CHAR     :
	case AST_BYTE     : return DTYPE_INLINE(DTYPE_BYTE);
	case AST_INT      : return DTYPE_INLINE(DTYPE_INT);
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
sem_type_eq (Unused st_type_t a, Unused st_type_t b)
{
	/* TODO */
	return true;
}
#pragma endregion

#pragma GCC diagnostic pop
#endif
