#pragma once

#include <stdlib.h>

#ifndef ADDR_OF
	#define ADDR_OF(x) ((typeof(x)[1]){(x)})
#endif

typedef struct {
	void *next;
} stack_node;

#define stack_node_header(ptr) ((stack_node*)ptr - 1)

#define stack_empty(s)     ((bool)((s) == NULL))
#define stack_push(s, var) ((void)(                       \
		(s) = stack_push_int((void*)(s),          \
			(void*)ADDR_OF(var), sizeof(var)) \
		))
#define stack_top(s)        (*(s))
#define stack_pop(s)       ((void)({                           \
	stack_node *__tmp__ = stack_node_header(s);            \
	(s) = stack_node_header(s)->next;                      \
	free(__tmp__);                                       }))

#define stack_next(s) ((typeof(s))(stack_node_header(s)->next))
#define stack_free(s) ({ while (!stack_empty(s)) stack_pop(s); })

extern void * stack_push_int(void*, void*, size_t);

#ifdef STACK_IMPLEMENT
void * __attribute__((unused))
stack_push_int (void *s, void *obj, size_t len)
{
	void *ret = malloc(len + sizeof(stack_node)) + sizeof(stack_node);
	stack_node_header(ret)->next = s;
	memcpy(ret, obj, len);
	return ret;
}
#endif //STACK_IMPLEMENT

