#pragma once

#ifdef SCOPE_IMPLEMENT
	#define STACK_IMPLEMENT
	#ifdef SEM_DEBUG
		#define AST_IMPLEMENT
	#endif
#endif

#ifdef SEM_DEBUG
	#include "ast.h"
	#define log(fmt...) printf("\tST DBG:\t" fmt)
#else
	#define log(fmt...)
#endif

#include "util/stack.h"

typedef struct {
	size_t key;
	enum sym_type {
		SYM_ERROR   = 0,
		SYM_INT     = 1,
		SYM_BYTE    = 2,
		SYM_REF     = 4,
		SYM_ARR     = 8,
		SYM_FUNC    = 16,
		SYM_VAR_ARR = 4 | 8,
	} __attribute__((packed)) value;
} scope_node_t;

typedef scope_node_t *scope_t;
typedef scope_t      *symbol_table_t;
#define ST_CLEANUP __attribute__((cleanup(st_destroy)))

typedef enum { ST_ANY_SCOPE, ST_TOP_SCOPE } st_scope_opt;

/** symbol_type utilities */
extern enum sym_type       symbol_type_from (enum ast_type type);
extern enum ast_type       symbol_type_of (scope_node_t node);
extern const char*         symbol_get_type_str (scope_node_t node);
extern symbol_table_t      st_create (void);
extern void                st_destroy (symbol_table_t *this);
extern void                st_add_scope (symbol_table_t *this);
extern void                st_remove_scope (symbol_table_t *this);
extern void                st_add_symbol (symbol_table_t *this, size_t name, enum ast_type type);
extern const scope_node_t* st_get_symbol (const symbol_table_t *this, size_t name, st_scope_opt opt);

#ifdef SCOPE_IMPLEMENT
static uint16_t scope_cnt = 0;

/** symbol_type utilities */
inline enum sym_type
symbol_type_from (enum ast_type type)
{
	switch (type) {
	case AST_INT      : return SYM_INT;
	case AST_BYTE     : return SYM_BYTE;
	case AST_REF_INT  : return SYM_REF | SYM_INT;
	case AST_REF_BYTE : return SYM_REF | SYM_BYTE;
	case AST_ARR_INT  : return SYM_ARR | SYM_INT;
	case AST_ARR_BYTE : return SYM_ARR | SYM_BYTE;
	case AST_DEF_PROC :
	case AST_DECL_PROC: return SYM_FUNC;
	case AST_DEF_INT  :
	case AST_DECL_INT : return SYM_FUNC | SYM_INT;
	case AST_DEF_BYTE :
	case AST_DECL_BYTE: return SYM_FUNC | SYM_BYTE;
	default: return SYM_ERROR;
	};
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch"
inline enum ast_type
symbol_type_of (scope_node_t node)
{
	switch (node.value) {
	case SYM_INT             : return AST_INT;
	case SYM_BYTE            : return AST_BYTE;
	case SYM_REF | SYM_INT   : return AST_REF_INT;
	case SYM_REF | SYM_BYTE  : return AST_REF_BYTE;
	case SYM_ARR | SYM_INT   : return AST_ARR_INT;
	case SYM_ARR | SYM_BYTE  : return AST_ARR_BYTE;
	case SYM_FUNC            : return AST_DECL_PROC;
	case SYM_FUNC | SYM_INT  : return AST_DECL_INT;
	case SYM_FUNC | SYM_BYTE : return AST_DECL_BYTE;
	default: return AST_ERROR;
	};
}
#pragma GCC diagnostic pop

inline const char*
symbol_get_type_str (scope_node_t node)
{
	return ast_symbol_arr[symbol_type_of(node)];
}

/** scope methods */
symbol_table_t
st_create (void)
{
	symbol_table_t ret = {0};
	st_add_scope(&ret);
	// TODO: Add default methods
	return ret;
}

void
st_destroy (symbol_table_t *this)
{
	while (!stack_empty(*this)) st_remove_scope(this);
}

inline void
st_add_scope (symbol_table_t *this)
{
	log("--- New Scope ---\n");
	++scope_cnt;
	stack_push(*this, (scope_t){0});
}

inline void
st_remove_scope (symbol_table_t *this)
{
	log("--- Pop Scope ---\n");
	--scope_cnt;
	hm_free(stack_top(*this));
	stack_pop(*this);
}

inline void
st_add_symbol (symbol_table_t *this, size_t name, enum ast_type type)
{
	log("Adding   symbol {%20lu: %s} at scope %u\n",
			name, ast_symbol_arr[type], scope_cnt);
	hm_put(stack_top(*this), name, symbol_type_from(type));
}

const scope_node_t*
st_get_symbol (const symbol_table_t *this, size_t name, st_scope_opt opt)
{
	scope_node_t *ret;
	const scope_t *u = *this;
	uint16_t lvl = 1;

	for (; u; u = stack_next(u), ++lvl)
		if ((ret = hm_getp(*u, name))) {
			log("Checking symbol  %20lu: found at scope %u: %s\n",
					name, lvl, symbol_get_type_str(*ret));
			return ret;
		} else if (opt == ST_TOP_SCOPE) {
			break;
		}
	log("Checking symbol  %20lu: not found\n", name);
	return NULL;
}

#endif // SCOPE_IMPLEMENT

#undef log
