#pragma once

#include <stdlib.h>

#pragma region DEBUGGING
#define dbg(x, fmt) \
	printf("DBG:\t%s:%d\t" #x ":\t" fmt "\n", __FILE__, __LINE__, x)
#define _assert(cond, fmt, ...) ({                                          \
		if (!(cond)) {                                              \
			printf("ASSERT\t%s:%d\t" fmt "\n",                  \
					__FILE__, __LINE__, ##__VA_ARGS__); \
			abort();                                            \
		}                                                          })
#pragma endregion

#pragma region UTILITIES
#define swap(a, b) do {            \
	__auto_type __tmp__ = (a); \
	(a) = (b); (b) = __tmp__;  \
} while (0)

#define SWITCH(val, type, body...) ({                      \
		inline type __fn__(typeof(+val) __param__) \
		{ switch(__param__) {body}; };             \
		__fn__(val);                              })
#pragma endregion

#pragma region SLICE_CHAR_T TO C STRING
typedef struct { char *ptr; char backup; } slice_temp_t;
void slice_temp_cleanup (slice_temp_t *this) { *this->ptr = this->backup; }

#define SLICE_TMP_STR(sl)                                             \
	slice_temp_t __attribute__((cleanup(slice_temp_cleanup)))     \
		__SL__ ## __LINE__ = {                                \
			(sl).ptr + (sl).length, (sl).ptr[(sl).length] \
		};                                                    \
	(sl).ptr[(sl).length] = '\0'
#pragma endregion

#pragma region POSITION
#define POS_DECL(name, bits) typedef struct __attribute__((packed)) { uint32_t pos: bits; } name
#define POS_ADV(p, x) ((typeof(p)){ (p).pos + x })
#define POS_DIFF(a, b) ((b).pos - (a).pos)
#define POS_CMP(a, b) ((int32_t)(a).pos - (int32_t)(b).pos)
#define POS_OK(a) ((a).pos != 0)
#pragma endregion

#pragma region ERROR HANDLING
#define throw(type, fmt...) throw_full(true, type, fmt)

#define throw_if(cond, type, fmt...) if (cond) throw_full(true, type, fmt)

#define throw_full(err, type, fmt, ...) do {                                   \
	fprintf(stderr, "%s:\t" fmt "\n",                                      \
			err ? "\x1b[31mError\x1b[m" : "\x1b[36mat   \x1b[m"    \
			__VA_OPT__(,) __VA_ARGS__);                            \
	return (type){0};                                                      \
} while (0)


#define try(expr, fmt...) ({ /* assumes ZII == ERROR */                        \
		typeof(expr) __val__ = (expr);                                 \
		if (!memcmp(&__val__, (typeof(expr)[1]){0},                    \
					sizeof(typeof(expr))))                 \
			throw_full(false, typeof(expr), fmt);                  \
		__val__;                                                      })
#pragma endregion

