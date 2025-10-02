#pragma once

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdint.h>

#ifndef SLICE_CHAR_T_DEFINED
#define SLICE_CHAR_T_DEFINED

typedef struct {
    char *ptr;
    uint32_t length;
} slice_char_t;

#endif


typedef struct string_node string_node;
typedef struct string_list string_list;
struct string_list {
 struct string_node { string_node *next, *prev; slice_char_t str; } *first, *last;
};

typedef struct { slice_char_t sep; } string_opts;

#define ListPushBack(u, n)  ListInsertNP((u).first, (u).last, (u).last, n, next, prev)
#define ListPopBack(u)      ListEraseNP((u).first, (u).last, (u).last, next, prev)

#define StrLit(s) ((slice_char_t){s, sizeof(s)-1})
#define STR_CLEANUP __attribute__((cleanup(str_destroy)))

extern void str_destroy (slice_char_t *this);
extern slice_char_t str_from_number(uint32_t n);
#define str_append(dst, src, ...) str_append_opts(dst, src, (string_opts){ __VA_ARGS__ })
extern slice_char_t str_append_opts(slice_char_t start, slice_char_t end, string_opts opts);

#ifdef STR_IMPLEMENT
void
str_destroy (slice_char_t *this)
{
    if (this->ptr) free(this->ptr);
}


slice_char_t
str_append_opts (slice_char_t start, slice_char_t end, string_opts opts)
{
    slice_char_t ret = { .length = start.length + end.length + opts.sep.length };

    if (!ret.length) return ret;
    ret.ptr = malloc(sizeof(char) * ret.length);
    assert(ret.ptr);
    memcpy(ret.ptr, start.ptr, sizeof(char) * start.length);
    if (opts.sep.length)
        memcpy(ret.ptr + start.length, opts.sep.ptr, sizeof(char) * opts.sep.length);
    memcpy(ret.ptr + start.length + opts.sep.length, end.ptr, sizeof(char) * end.length);
    return ret;
}

slice_char_t
str_from_number (uint32_t n)
{
    char buf[11]; // max 10 digits + null terminator
    snprintf(buf, sizeof(buf), "%u", n);
    return (slice_char_t){ .ptr = strndup(buf, 11), .length = strlen(buf) };
}
#endif

#define ListInsertNP(f, l, p, n, next, prev) do { \
    __typeof__(n) __tmp = (n); \
    if ((f) == 0) { \
        /* empty list */ \
        ((f) = (l) = (__tmp), (__tmp)->next = (__tmp)->prev = 0); \
    } else if ((p) == 0) { \
        /* insert at head */ \
        (__tmp)->next = (f); \
        (__tmp)->prev = 0; \
        (f)->prev = (__tmp); \
        (f) = (__tmp); \
    } else if ((p) == (l)) { \
        /* insert at tail */ \
        (__tmp)->prev = (p); \
        (__tmp)->next = 0; \
        (p)->next = (__tmp); \
        (l) = (__tmp); \
    } else { \
        /* insert in middle */ \
        (__tmp)->prev = (p); \
        (__tmp)->next = (p)->next; \
        (p)->next->prev = (__tmp); \
        (p)->next = (__tmp); \
    } \
} while (0)


#define ListEraseNP(f, l, n, next, prev) do { \
    __typeof__(n) __tmp = (n); \
    if ((f) == (l)) { \
        (f) = (l) = 0; \
    } else if ((__tmp) == (f)) { \
        (f) = (f)->next; \
        if (f) (f)->prev = 0; \
    } else if ((__tmp) == (l)) { \
        (l) = (l)->prev; \
        if (l) (l)->next = 0; \
    } else { \
        (__tmp)->prev->next = (__tmp)->next; \
        (__tmp)->next->prev = (__tmp)->prev; \
    } \
    free(__tmp); \
} while (0)

