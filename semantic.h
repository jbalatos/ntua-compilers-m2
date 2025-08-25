#pragma once

#ifdef SEM_IMPLEMENT
	#define PARSER_IMPLEMENT
	#define SCOPE_IMPLEMENT
#endif

#include "parser.h"
#include "scope.h"
#include "util.h"

typedef struct {
	const char *msg;
} sem_error_t;

#define SEM_OK(err) (!(err.msg))

#define throw(expr...) do {                   \
	__auto_type __err__ = expr;           \
	if (!SEM_OK(__err__)) return __err__; \
} while (0)

extern sem_error_t sem_check (const parser_t *this, symbol_table_t *st, ast_node_pos pos);

#ifdef SEM_IMPLEMENT

sem_error_t
sem_check (const parser_t *this, symbol_table_t *st, ast_node_pos pos)
{
	ast_node_t node = parser_get_node(this, pos);

#ifdef SEM_DEBUG
	static uint16_t id = 0;
	printf("BEG sem check %u:\t%s\n", ++id, ast_get_type_str(node));
#endif

	switch (node.type) {
	/* New symbols */
	break;	case AST_DECL_PROC ... AST_DECL_BYTE:
		if (st_get_symbol(st, node.name_data.name, ST_TOP_SCOPE))
			return (sem_error_t){ "Func name used on current scope" };
		st_add_symbol(st, node.name_data.name, node.type);
	break;	case AST_DEF_PROC ... AST_DEF_BYTE:
		ast_node_pos args = parser_get_node(this, node.op_data.lhs)
			.name_data.body;

		throw(sem_check(this, st, node.op_data.lhs));
		st_add_scope(st);
		sem_check(this, st, args);
		throw(sem_check(this, st, node.op_data.rhs));
		st_remove_scope(st);
	break;	case AST_LOCAL_DEF:
		extra_for_each(this, node.extra_data, i, it,
			sem_check(this, st, it);
		);
	break;	case AST_INT ... AST_BYTE:
		case AST_REF_INT ... AST_ARR_BYTE:
		for (; node.type; node = parser_get_node(this, node.var_data.next)) {
			if (st_get_symbol(st, node.var_data.name, ST_TOP_SCOPE))
				return (sem_error_t){ "Var name used on current scope" };
			st_add_symbol(st, node.var_data.name, node.type);
		}
	break;	default:
	}

#ifdef SEM_DEBUG/* {{{ */
	printf("Current scope:\n");
	if (!stack_empty(*st)) for (size_t i=0; i<arr_ucap(stack_top(*st)); ++i) {
		hm_cell_t it = arr_header(stack_top(*st))->hash_table[i];
		if (it.hash == 0) continue;
		printf("| %20lu : %10s |\n",
				stack_top(*st)[it.index].key,
				symbol_get_type_str(stack_top(*st)[it.index]));
	}
	printf("END sem check %u\n", id--);
#endif/* }}} */

	return (sem_error_t){};
}

#endif // SEM_IMPLEMENT
