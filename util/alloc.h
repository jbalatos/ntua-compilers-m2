#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#if !defined(typeof)
#	define typeof __typeof__
#endif
#if !defined(Unused)
#	define Unused __attribute__((unused))
#endif

#pragma push_macro("ADDR_OF")
#define ADDR_OF(x) ((typeof(x)[1]){(x)})
#pragma push_macro("PRAGMA")
#define PRAGMA(x) _Pragma(#x)

/** slice */
/* {{{ */
#define slice(typevar) struct { \
	typeof(typevar) *ptr;   \
	uint32_t length;        \
}

#define EXPAND_SLICE(sl) { (sl).ptr, (sl).length }
#define set_slice(dst, src) ({                                             \
		__auto_type __tmp__ = (src);                               \
		(dst) = (typeof(dst)){ .ptr = (__tmp__).ptr, .length = (__tmp__).length }; })
#define UNSLICE(sl) (sl).length, (sl).ptr

typedef slice(void) slice_t;
/* }}} */

/* interface */
typedef struct alloc_t alloc_t;
struct alloc_t {
	slice_t (*alloc)   (alloc_t *ap, size_t elem_size, size_t length);
	void    (*dealloc) (alloc_t *ap, size_t elem_size, slice_t slice);
};

#define alloc_create(al, typevar) allocate(al, typevar, 1).ptr
#define alloc_destroy(al, ptr) deallocate(al, ((slice(typeof(*ptr))){ (ptr), 1 }))

#define alloc_create_bytes(al, size) allocate(al, void, size).ptr
#define alloc_destroy_bytes(al, ptr, size) deallocate(al, (slice_t){ (ptr), (size) })

/* UFCS */
/* {{{ */
#define allocate(al, typevar, len) ({                             \
		slice_t __tmp__ = ((alloc_t*)&(al))->alloc(       \
			(alloc_t*)&(al), sizeof(typevar), (len)); \
		(slice(typevar)) EXPAND_SLICE(__tmp__);          })
#define deallocate(al, sl) ((alloc_t*)&(al))->dealloc( \
		(alloc_t*)&(al), sizeof(*(sl).ptr),    \
		(slice_t) EXPAND_SLICE(sl)             \
		)
#define alloc_dup(al, src) ({                                          \
		slice_t __tmp__ = alloc_dup_int(                       \
				(alloc_t*)&(al),                       \
				(slice_t) { (src).ptr, (src).length }, \
				sizeof(*(src).ptr)                     \
				);                                     \
		cast_slice(*src.ptr, __tmp__);                        })
/* }}} */

typedef struct {
	const alloc_t alloc;
	uint8_t *buffer;
	size_t capacity, top;
} alloc_buffer_t;

typedef struct {
	const alloc_t alloc;
	const alloc_t *back_alloc;
	size_t page_size, top /*of last page*/;
	uint8_t *page; /*top of each page has the pointer to the previous*/
} alloc_arena_t;

typedef struct {
	alloc_t alloc;
	size_t min_length;
	struct alloc_freelist_node_t {
		size_t length;
		struct alloc_freelist_node_t *next;
	} *list;
} alloc_freelist_t;

/* helper */
extern slice_t         alloc_dup_int (alloc_t *this, slice_t sl,size_t elem_size);
/* LIBC allocator*/
extern const alloc_t   LIBC;
/* buffer allocator */
extern alloc_buffer_t  alloc_buffer_create (size_t length, uint8_t buffer[length]);
/* arena allocator */
#define ARENA_PG_SIZE  4096ul
#define ALLOC_ARENA_CLEANUP \
	alloc_arena_t __attribute__((cleanup(alloc_arena_destroy)))
extern alloc_arena_t   alloc_arena_create (const alloc_t *alloc, size_t page_size);
extern void            alloc_arena_destroy (const alloc_arena_t* this);
/* freelist allocator */
extern alloc_freelist_t alloc_freelist_create (size_t length, uint8_t buffer[length], size_t min_size);

#ifdef ALLOC_IMPLEMENT
slice_t __attribute__((unused))
alloc_dup_int (alloc_t *this, slice_t sl, size_t elem_size)
{
	if (!sl.ptr) return (slice_t){};
	slice_t ret = this->alloc(this, elem_size, sl.length);
	memcpy(ret.ptr, sl.ptr, elem_size * sl.length);
	return ret;
}
/** LIBC: heap allocator */
/* {{{ */
static slice_t
libc_alloc (Unused alloc_t *ap, size_t elem_size, size_t length)
{
	void *ptr = malloc(elem_size * length);
	if (ptr) return (slice_t) { ptr, length };
	else return (slice_t) {};
}

static void
libc_dealloc (Unused alloc_t *ap, Unused size_t elem_size, slice_t slice)
{
	free(slice.ptr);
}

const alloc_t LIBC = { libc_alloc, libc_dealloc };
/* }}} */

/** Buffer allocator */
/* {{{ */
static slice_t
alloc_buffer_alloc (alloc_t *ap, size_t elem_size, size_t length)
{
	alloc_buffer_t *this = (alloc_buffer_t*)ap;
	size_t pos = this->top;

	if (pos + elem_size * length > this->capacity) {
		return (slice_t){};
	} else {
		this->top += elem_size * length;
		return (slice_t){ this->buffer + pos, length };
	}
}

static void
alloc_buffer_dealloc (alloc_t *ap, size_t elem_size, slice_t slice)
{
	alloc_buffer_t *this = (alloc_buffer_t*)ap;
	/* optimization: if last, pop it */
	if (slice.ptr + elem_size * slice.length == this->buffer + this->top)
		this->top -= elem_size * slice.length;
	// do nothing
}

alloc_buffer_t __attribute__((unused))
alloc_buffer_create (size_t length, uint8_t buffer[length])
{
	return (alloc_buffer_t){
		.alloc = { alloc_buffer_alloc, alloc_buffer_dealloc },
		.buffer = buffer, .capacity = length, .top = 0,
	};
}
/* }}} */

/** Arena allocator */
/* {{{ */
static slice_t
alloc_arena_alloc (alloc_t *ap, size_t elem_size, size_t length)
{
	alloc_arena_t *this = (alloc_arena_t*)ap;
	uint8_t *new_page;
	size_t pos;

	if (elem_size * length > this->page_size - sizeof(uint8_t*))
		return (slice_t){};
	/* create new page */
	if (!this->page || this->top + elem_size * length > this->page_size) {
		new_page = allocate(this->back_alloc, uint8_t, this->page_size).ptr;
		if (!new_page) return (slice_t){};
		*(uint8_t**)new_page = this->page;
		this->top = sizeof(uint8_t*);
		this->page = new_page;
	}
	pos = this->top, this->top += elem_size * length;
	return (slice_t){ this->page + pos, length };
}

static void
alloc_arena_dealloc (Unused alloc_t *ap, Unused size_t elem_size, Unused slice_t slice)
{}

alloc_arena_t __attribute__((unused))
alloc_arena_create (const alloc_t *alloc, size_t page_size)
{
	return (alloc_arena_t) {
		.alloc = { alloc_arena_alloc, alloc_arena_dealloc },
		.back_alloc = alloc, .page_size = page_size,
	};
}

void __attribute__((unused))
alloc_arena_destroy (const alloc_arena_t* this)
{
	uint8_t *page, *next;

	for (page = this->page; (next = *(uint8_t**)page); page=next)
		deallocate(this->back_alloc, (
					(slice_t){ page, this->page_size }));
}
/* }}} */

/** Free list allocator */
/* {{{ */
static slice_t
alloc_freelist_alloc (alloc_t *ap, size_t elem_size, size_t length)
{
	alloc_freelist_t *this = (alloc_freelist_t*)ap;
	struct alloc_freelist_node_t *node = this->list, *prv, *new_node;
	size_t total_size = elem_size * length;

	if (total_size < this->min_length)
		total_size = this->min_length;
	for (; node && node->length < total_size; prv=node, node=node->next);
	if (!node) return (slice_t){};

	/* split node into two */
	if (node->length > total_size + this->min_length) {
		new_node = (void*)node + total_size;
		*new_node = (struct alloc_freelist_node_t){
			.length = node->length - total_size,
			.next = node->next,
		};
		*node = (struct alloc_freelist_node_t){
			.length = total_size,
			.next = new_node,
		};
	}
	if (prv) prv->next = node->next;
	else     this->list = node->next;
	return (slice_t){ node, length };
}

static void
alloc_freelist_dealloc (alloc_t *ap, Unused size_t elem_size, slice_t slice)
{
	alloc_freelist_t *this = (alloc_freelist_t*)ap;
	struct alloc_freelist_node_t *node = slice.ptr;

	*node = (struct alloc_freelist_node_t){
		.length = slice.length, .next = this->list
	};
	if (node->length < this->min_length) node->length = this->min_length;
	this->list = node;
}

alloc_freelist_t __attribute__((unused))
alloc_freelist_create (size_t length, uint8_t buffer[length], size_t min_size)
{
	struct alloc_freelist_node_t *top = (struct alloc_freelist_node_t*)buffer;
	*top = (struct alloc_freelist_node_t){ .length = length };
	if (min_size < sizeof(struct alloc_freelist_node_t))
		min_size = sizeof(struct alloc_freelist_node_t);
	return (alloc_freelist_t){
		.alloc = { alloc_freelist_alloc, alloc_freelist_dealloc },
		.list = top, .min_length = min_size,
	};
}
/* }}} */
#endif // ALLOC_IMPLEMENT

#pragma pop_macro("ADDR_OF")
#pragma pop_macro("PRAGMA")
