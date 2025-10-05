#pragma once

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdint.h>
#include <ctype.h>
#include "dynamic_array.h"

#ifndef SLICE_CHAR_T_DEFINED
#define SLICE_CHAR_T_DEFINED

typedef struct {
    char *ptr;
    uint32_t length;
} slice_char_t;

#endif

#define STR_CLEANUP __attribute__((cleanup(str_destroy)))


typedef struct string_node string_node;
typedef struct string_list string_list;
struct string_list {
 struct string_node { string_node *next, *prev; slice_char_t str; } *first, *last;
};

typedef struct { slice_char_t sep; bool c_str; } string_opts;

#define ListPushBack(u, n)  ListInsertNP((u).first, (u).last, (u).last, n, next, prev)
#define ListPopBack(u)      ListEraseNP((u).first, (u).last, (u).last, next, prev)
#define ListFree(u)         while ((u).last) ListPopBack(u)

#define StrLit(s) ((slice_char_t){s, sizeof(s)-1})
#define STR_CLEANUP __attribute__((cleanup(str_destroy)))

extern void str_destroy (slice_char_t *this);
extern slice_char_t str_from_number(uint32_t n);
#define str_append(dst, src, ...) str_append_opts(dst, src, (string_opts){ __VA_ARGS__ })
extern slice_char_t str_append_opts(slice_char_t start, slice_char_t end, string_opts opts);
#define str_join(l, ...) str_join_opts(l, (string_opts){ __VA_ARGS__ })
extern slice_char_t str_join_opts(string_list l, string_opts opts);

extern int isHexDigit(char c);
extern int hexValue(char c);
extern int isBinaryDigit(char c);
extern char* processEscapeSequences(const char* input, size_t* outLen);

#ifdef STR_IMPLEMENT
void
str_destroy (slice_char_t *this)
{
    if (this->ptr) free(this->ptr);
}

slice_char_t
str_append_opts (slice_char_t start, slice_char_t end, string_opts opts)
{
    size_t start_len = start.ptr[start.length - 1] == '\0' ? start.length - 1 : start.length,
           end_len   = end.ptr[end.length - 1] == '\0' ? end.length - 1 : end.length;
    slice_char_t ret = { .length = start_len + end_len + opts.sep.length + (int)opts.c_str };

    if (!ret.length) return ret;
    ret.ptr = malloc(sizeof(char) * ret.length);
    assert(ret.ptr);
    memcpy(ret.ptr, start.ptr, sizeof(char) * start_len);
    if (opts.sep.length)
        memcpy(ret.ptr + start_len, opts.sep.ptr, sizeof(char) * opts.sep.length);
    memcpy(ret.ptr + start_len + opts.sep.length, end.ptr, sizeof(char) * end_len);
    if (opts.c_str)
        ret.ptr[ret.length - 1] = '\0';
    return ret;
}

slice_char_t
str_join_opts (string_list l, string_opts opts)
{
	size_t total_size = 0;
	for (string_node *u = l.first; u; u = u->next) {
		if (u != l.last) total_size += opts.sep.length;
		total_size += u->str.length;
	}

	slice_char_t ret = {
		.ptr = calloc(sizeof(char), total_size),
		.length = 0,
	};

	for (string_node *u = l.first; u; u = u->next ) {
		memcpy(ret.ptr + ret.length, u->str.ptr, u->str.length);
		ret.length += u->str.length;
		if (u != l.last) {
			memcpy(ret.ptr + ret.length, opts.sep.ptr, opts.sep.length);
			ret.length += opts.sep.length;
		}
	}
	if (opts.c_str) ret.length += 1;
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



int isHexDigit(char c) {
    return (c >= '0' && c <= '9') || 
           (c >= 'a' && c <= 'f') || 
           (c >= 'A' && c <= 'F');
}

int hexValue(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return 0;
}

int isBinaryDigit(char c) {
    return c == '0' || c == '1';
}

char* processEscapeSequences(const char* input, size_t* outLen) {
    size_t inLen = strlen(input);
    char* result = {0}; 
    
    for (size_t i = 0; i < inLen; i++) {
        if (input[i] == '\\' && i + 1 < inLen) {
            i++; 
            
            /* \xHH */
            if (input[i] == 'x' && i + 1 < inLen) {
                i++; 
                int value = 0;
                int digits = 0;
                int backtrack = i - 1;
                
                while (digits < 2 && i < inLen && isHexDigit(input[i])) {
                    value = value * 16 + hexValue(input[i]);
                    i++;
                    digits++;
                }
                
                if (digits > 0) {
                    arr_push(result, (char)value);
                    i--; 
                } else {
                    // No hex digits after \x - treat as literal
                    arr_push(result, 'x');
                    i = backtrack;
                }
            }
            /* Handle binary: \bBBBBBBBB */
            else if (input[i] == 'b' && i + 1 < inLen) {
                i++;  
                int value = 0;
                int digits = 0;
                int backtrack = i - 1;
                
                while (digits < 8 && i < inLen && isBinaryDigit(input[i])) {
                    value = value * 2 + (input[i] - '0');
                    i++;
                    digits++;
                } 
                
                if (digits > 0) {
                    arr_push(result, (char)value);
                    i--;
                } else {
                    arr_push(result, '\b');
                    i = backtrack;
                }
            }
            /* Handle octal: \OOO (1-3 octal digits) */
            else if (input[i] >= '0' && input[i] <= '7') {
                int value = 0;
                int digits = 0;
                
                // Read up to 3 octal digits
                while (digits < 3 && i < inLen && input[i] >= '0' && input[i] <= '7') {
                    value = value * 8 + (input[i] - '0');
                    i++;
                    digits++;
                }
                i--;
                
                arr_push(result, (char)value);
            }
            /* Handle named escape sequences */
            else {
                switch (input[i]) {
                    case 'n':  arr_push(result, '\n'); break;
                    case 't':  arr_push(result, '\t'); break;
                    case 'r':  arr_push(result, '\r'); break;
                    case '\\': arr_push(result, '\\'); break;
                    case '\"': arr_push(result, '\"'); break;
                    case '\'': arr_push(result, '\''); break;
                    case '0':  arr_push(result, '\0'); break;
                    case 'b':  arr_push(result, '\b'); break;
                    case 'f':  arr_push(result, '\f'); break;
                    case 'v':  arr_push(result, '\v'); break;
                    case 'a':  arr_push(result, '\a'); break;
                    case '?':  arr_push(result, '\?'); break;
                    default:
                        arr_push(result, '\\');
                        arr_push(result, input[i]);
                        break;
                }
            }
        } else {
            arr_push(result, input[i]);
        }
    }
    
    arr_push(result, '\0');
    if (outLen) *outLen = arr_len(result);
    return result;
}
