#pragma once

#ifdef SEM_IMPLEMENT
	#define DA_IMPLEMENT
	#define STACK_IMPLEMENT
#endif

#include "parser.h"
#include "util.h"
#include "util/dynamic_array.h"
#include "util/stack.h"

#pragma region TYPES

// TODO: add declaration positions
typedef struct { uint16_t key; dtype_t value; } st_cell_t;
typedef st_cell_t  *st_scope_t;
typedef st_scope_t *sym_table_t;

#pragma endregion

#pragma region SYMBOL TABLE DECLARATIONS

#define ST_CLEANUP __attribute__((cleanup(sym_table_destroy)))
extern sym_table_t sym_table_create(void);
extern void        sym_table_destroy(sym_table_t *this);
#define st_emplace_symbol(this, name, decl...) \
	st_put_symbol(this, name, (dtype_t){ decl })
extern dtype_t     st_get_symbol(const sym_table_t *this, uint16_t name);
extern void        st_pop_scope(sym_table_t *this);
extern void        st_push_scope(sym_table_t *this);
extern bool        st_put_symbol(sym_table_t *this, uint16_t name, dtype_t type);
extern bool        st_scope_has(const sym_table_t *this, uint16_t name);
#pragma endregion

#pragma region SEMANTIC CHECK DECLARATIONS
extern bool        sem_check(const parser_t *this, ast_node_pos pos);
extern bool       _sem_check(const parser_t *this, sym_table_t *st, ast_node_pos pos);
extern bool       _sem_check_cond(const parser_t *this, sym_table_t *st, ast_node_pos pos);
extern bool       _sem_check_def(const parser_t *this, sym_table_t *st, ast_node_pos pos);
extern enum dtype  sem_converts_to(dtype_t from, dtype_t to);
extern dtype_t     sem_eval_expr(const parser_t *this, sym_table_t *st, ast_node_pos pos);
extern enum dtype  sem_get_dtype(const parser_t *this, ast_node_t node);
extern const char* sem_get_type_str(dtype_t type);
extern bool        sem_is_number(enum dtype type);
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

#pragma region SYMBOL TABLE IMPLEMENTATIONS
sym_table_t
sym_table_create (void)
{
	sym_table_t ret = {0};
	st_push_scope(&ret);
	// TODO: add default procedures
	return ret;
}

void
sym_table_destroy (sym_table_t *this)
{
	for (; !stack_empty(*this); st_pop_scope(this));
}

dtype_t
st_get_symbol (const sym_table_t *this, uint16_t name)
{
	ssize_t idx = -1;

	for (st_scope_t *it = *this; it; it = stack_next(it))
		if ((idx = hm_geti(*it, name)) != -1)
			return (*it)[idx].value;
	return (dtype_t){0};
}

inline void
st_pop_scope (sym_table_t *this)
{
	stack_pop(*this);
}

inline void
st_push_scope (sym_table_t *this)
{
	stack_push(*this, (st_scope_t){0});
}

inline bool
st_put_symbol (sym_table_t *this, uint16_t name, dtype_t type)
{
	if (st_scope_has(this, name)) return true;
	hm_put(stack_top(*this), name, type);
	return false;
}

inline bool
st_scope_has (const sym_table_t *this, uint16_t name)
{
	return hm_geti(stack_top(*this), name) != -1;
}

#pragma endregion

#pragma region SEMANTIC CHECK IMPLEMENTATIONS
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
	enum dtype type;

	log("checking %s at " PAR_FSTR,
			ast_get_type_str(node), PAR_FPOS(this, node));
	switch (node.type) {
	/* grouping nodes */
	case AST_ARGS:
		for(; ast_is_child(it); it = ast_next_child(it)) {
			dtype_t arg = sem_eval_expr(this, st, it.pos);
			throw_if(arg.type == DTYPE_NONE,
					bool,
					PAR_FSTR "cannot find variable %.*s in current scope",
					PAR_FPOS(this, *it.node),
					UNSLICE(par_get_name(this, *it.node)));
			// TODO: check value on func declaration
		}
	break; case AST_VARS:
		for(; ast_is_child(it); it = ast_next_child(it))
			throw_if(st_emplace_symbol(st, it.node->name,
						sem_get_dtype(this, *it.node),
					), bool,
					PAR_FSTR "variable name %.*s already used in current scope",
					PAR_FPOS(this, *it.node),
					UNSLICE(par_get_name(this, *it.node)));
	break; case AST_LOCAL_DEF:
		CHECK_CHLD();
	/* scope-creating nodes */
	break; case AST_BLOCK_SIMPLE:
		log("-- simple block begin");
		st_push_scope(st);
		try(_sem_check(this, st, it.pos), PAR_FSTR, PAR_FPOS(this, node));
		st_pop_scope(st);
		log("-- simple block end");
	break; case AST_BLOCK:
	       case AST_INDENT:
		log("-- %sblock begin", node.type == AST_INDENT ? "indent " : "");
		st_push_scope(st);
		CHECK_CHLD();
		log("-- %sblock end", node.type == AST_INDENT ? "indent " : "");
		st_pop_scope(st);
	break; case AST_DECL_PROC ... AST_DECL_BYTE:
	       case AST_DEF_PROC ... AST_DEF_BYTE:
		throw_if(st_emplace_symbol(st, node.name, sem_get_dtype(this, node)),
				bool,
				PAR_FSTR "function name %.*s is already being used in current scope",
				PAR_FPOS(this, node),
				UNSLICE(par_get_name(this, node)));
		if (AST_DEF_PROC <= node.type && node.type <= AST_DEF_BYTE)
			try(_sem_check_def(this, st, it.pos), PAR_FSTR, PAR_FPOS(this, node));
	/* statements */
	break; case AST_SKIP:
	break; case AST_EXIT:
		// TODO: check if topmost function is proc
	break; case AST_BREAK: case AST_CONT:
		if (!node.name) break;
		throw_if(st_get_symbol(st, node.name).type != DTYPE_LOOP,
				bool,
				PAR_FSTR "name %.*s does not correspond to a loop",
				PAR_FPOS(this, node),
				UNSLICE(par_get_name(this, node))
				);
	break; case AST_LOOP:
		throw_if(st_emplace_symbol(st, node.name, DTYPE_LOOP),
				bool,
				PAR_FSTR "loop name %.*s already being used in current scope",
				PAR_FPOS(this, node),
				UNSLICE(par_get_name(this, node))
				);
		try(_sem_check(this, st, it.pos), PAR_FSTR, PAR_FPOS(this, node));
	break; case AST_ASSIGN:
		lhs = sem_eval_expr(this, st, it.pos); it = ast_next_child(it);
		rhs = sem_eval_expr(this, st, it.pos);
		throw_if(sem_converts_to(rhs, lhs) == DTYPE_NONE, bool,
				PAR_FSTR "type mismatch on assignment: %s cannot be converted to %s",
				PAR_FPOS(this, node),
				sem_get_type_str(lhs), sem_get_type_str(rhs)
				);
	break; case AST_RETURN:
		try(sem_eval_expr(this, st, it.pos).type == DTYPE_FUNC, PAR_FSTR, PAR_FPOS(this, node));
		// TODO: check if topmost function is of same return type
	break; case AST_PROC_CALL:
		type = st_get_symbol(st, node.name).type;
		throw_if((type & DTYPE_FUNC) == 0, bool,
				PAR_FSTR "no procedure named %.*s in scope",
				PAR_FPOS(this, node),
				UNSLICE(par_get_name(this, node))
				);
		throw_if(type & (DTYPE_INT | DTYPE_BYTE), bool,
				PAR_FSTR "procedure %.*s has return value",
				PAR_FPOS(this, node),
				UNSLICE(par_get_name(this, node))
				);
		CHECK_CHLD();
	break; case AST_COND:
		try(_sem_check_cond(this, st, it.pos), PAR_FSTR, PAR_FPOS(this, node));
	break; default:
		log("%s - PENDING...", ast_get_type_str(node));
	}
	return true;
#undef CHECK_CHLD
}

bool
_sem_check_cond (const parser_t *this, sym_table_t *st, ast_node_pos pos)
{
	ast_node_t node = par_get_node(this, pos);
	ast_node_it it = ast_get_child(this, pos);
	dtype_t tmp;
	bool is_cond;

	tmp = sem_eval_expr(this, st, it.pos);
	try(sem_is_number(tmp.type),
			PAR_FSTR "in condition of if", PAR_FPOS(this, node));
	it = ast_next_child(it);

	try(_sem_check(this, st, it.pos),
			PAR_FSTR "in body of if", PAR_FPOS(this, node));
	it = ast_next_child(it);

	for (is_cond = true; ast_is_child(it); it = ast_next_child(it), is_cond ^= 1)
		if (is_cond && ast_is_child(ast_next_child(it))) {
			tmp = sem_eval_expr(this, st, it.pos);
			try(sem_is_number(tmp.type),
					PAR_FSTR "in condition of else-if",
					PAR_FPOS(this, node));
		} else if (is_cond) {
			try(_sem_check(this, st, it.pos),
					PAR_FSTR "in body of else",
					PAR_FPOS(this, node));
		} else {
			try(_sem_check(this, st, it.pos),
					PAR_FSTR "in body of else-if",
					PAR_FPOS(this, node));
		}

	return true;
}

bool
_sem_check_def (const parser_t *this, sym_table_t *st, ast_node_pos pos)
{
	ast_node_t node = par_get_node(this, pos);
	ast_node_it it = ast_get_child(this, pos);

	st_push_scope(st);
	st_emplace_symbol(st, node.name, sem_get_dtype(this, node)); /* cannot overwrite on new scope */
	/* arguments */
	for (; POS_CMP(POS_ADV(pos, node.def_data.body_offset), it.pos);
			it = ast_next_child(it))
		try(_sem_check(this, st, it.pos), PAR_FSTR, PAR_FPOS(this, node));
	/* local defs */
	try(_sem_check(this, st, it.pos), PAR_FSTR, PAR_FPOS(this, node));
	it = ast_next_child(it);
	/* body */
	try(_sem_check(this, st, it.pos), PAR_FSTR, PAR_FPOS(this, node));

	return true;
}

inline enum dtype
sem_converts_to (dtype_t from, dtype_t to)
{
	if (from.type == DTYPE_BYTE && to.type == DTYPE_INT)
		return DTYPE_INT;
	if (from.type == DTYPE_BYTE && to.type == DTYPE_BYTE)
		return DTYPE_BYTE;
	if (from.type == DTYPE_INT && to.type == DTYPE_INT)
		return DTYPE_INT;
	return DTYPE_NONE;
}

dtype_t
sem_eval_expr (const parser_t *this, sym_table_t *st, ast_node_pos pos)
{
	ast_node_t node = par_get_node(this, pos);
	ast_node_it it = ast_get_child(this, pos);
	dtype_t lhs, rhs;

	switch (node.type) {
	/* literal */
	case AST_NUMBER:
		return (dtype_t){
			(0 <= node.pl_data.num && node.pl_data.num < (1 << 8))
				? DTYPE_BYTE : DTYPE_INT
		};
	case AST_TRUE ... AST_FALSE:
	case AST_CHAR:
		return (dtype_t){ DTYPE_BYTE };
	case AST_STRING:
		return (dtype_t){
			.type = DTYPE_ARRAY | DTYPE_BYTE,
			.dim = strlen(par_get_text(this, node.pl_data.str)),
		};
	/* lvalue */
	case AST_NAME:
		lhs = st_get_symbol(st, node.name);
		throw_if(lhs.type == DTYPE_NONE, dtype_t,
				PAR_FSTR "no name '%.*s' in current scope",
				PAR_FPOS(this, node),
				UNSLICE(par_get_name(this, node)));
		return lhs;
	case AST_ARRAY_AT: // TODO
		return (dtype_t){ DTYPE_INT };
	/* func-call */
	case AST_FUNC_CALL:
		lhs = par_get_type(this, node.name);
		throw_if(lhs.type == DTYPE_NONE, dtype_t,
				PAR_FSTR "function %.*s doesn't exist in current scope",
				PAR_FPOS(this, node),
				UNSLICE(par_get_name(this, node))
				);
		throw_if(lhs.type != (DTYPE_FUNC | DTYPE_INT) &&
			 lhs.type != (DTYPE_FUNC | DTYPE_BYTE), dtype_t,
			 PAR_FSTR "%.*s is procedure, not a function",
			 PAR_FPOS(this, node),
			 UNSLICE(par_get_name(this, node))
			 );
		for(; ast_is_child(it); it = ast_next_child(it)) {
			try(sem_eval_expr(this, st, it.pos),
					PAR_FSTR, PAR_FPOS(this, node));
		}
		// TODO: Add check of types to func signature
		return (dtype_t){ lhs.type ^ DTYPE_FUNC };
	/* unary operators */
	case AST_BIT_NOT:
	case AST_BOOL_NOT:
	case AST_MINUS: /* unary minus */
		if (!ast_is_child(ast_next_child(it))) {
			lhs = try(sem_eval_expr(this, st, it.pos),
					PAR_FSTR, PAR_FPOS(this, node));
			throw_if(!sem_is_number(lhs.type), dtype_t,
					PAR_FSTR "operand in '%s' is not a number",
					PAR_FPOS(this, node),
					ast_get_type_str(node));
			return (dtype_t){ lhs.type };
		}
	/* binary operators */
	case AST_PLUS:
	case AST_MULT ... AST_MOD:
	case AST_BIT_AND ... AST_CMP_GEQ:
	case AST_BOOL_AND ... AST_BOOL_OR:
		lhs = try(sem_eval_expr(this, st, it.pos),
				PAR_FSTR, PAR_FPOS(this, node));
		it = ast_next_child(it);
		rhs = try(sem_eval_expr(this, st, it.pos),
				PAR_FSTR, PAR_FPOS(this, node));

		throw_if(!sem_is_number(lhs.type), dtype_t,
				PAR_FSTR "left operand in '%s' is not a number",
				PAR_FPOS(this, node), ast_get_type_str(node));
		throw_if(!sem_is_number(rhs.type), dtype_t,
				PAR_FSTR "right operand in '%s' is not a number",
				PAR_FPOS(this, node), ast_get_type_str(node));
		/* type conversion */
		if (lhs.type == DTYPE_INT || rhs.type == DTYPE_INT)
			lhs.type = DTYPE_INT;
		return (dtype_t){ lhs.type };
	default:
		return (dtype_t){};
	}
}

inline enum dtype
sem_get_dtype (const parser_t *this, ast_node_t node)
{
	switch (node.type) {
	case AST_INT      : return DTYPE_INT;
	case AST_BYTE     : return DTYPE_BYTE;
	case AST_ARRAY    : return par_get_type(this, node.var_data.array).type;
	case AST_DEF_PROC :
	case AST_DECL_PROC: return DTYPE_VAR;
	case AST_DEF_INT  :
	case AST_DECL_INT : return DTYPE_VAR | DTYPE_INT;
	case AST_DEF_BYTE :
	case AST_DECL_BYTE: return DTYPE_VAR | DTYPE_BYTE;
	default           : return DTYPE_NONE;
	}
}

inline const char*
sem_get_type_str (dtype_t type)
{
	switch ((uint8_t)type.type) {
	case DTYPE_INT                   : return "int";
	case DTYPE_BYTE                  : return "byte";
	case DTYPE_ARRAY     | DTYPE_INT :
	case DTYPE_VAR_ARRAY | DTYPE_INT : return "int[]";
	case DTYPE_ARRAY     | DTYPE_BYTE:
	case DTYPE_VAR_ARRAY | DTYPE_BYTE: return "byte[]";
	case DTYPE_FUNC                  : return "void()";
	case DTYPE_FUNC     | DTYPE_INT  : return "int()";
	case DTYPE_FUNC     | DTYPE_BYTE : return "byte()";
	default: return "void";
	}
}

inline bool
sem_is_number (enum dtype type)
{
	return type == DTYPE_INT || type == DTYPE_BYTE;
}
#pragma endregion

#endif
