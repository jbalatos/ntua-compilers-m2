#pragma once

#include <stdlib.h>

#define dbg(x, fmt) \
	printf("DBG:\t%s:%d\t" #x ":\t" fmt "\n", __FILE__, __LINE__, x)
#define _assert(cond, fmt, ...) ({                                          \
		if (!(cond)) {                                              \
			printf("ASSERT\t%s:%d\t" fmt "\n",                  \
					__FILE__, __LINE__, ##__VA_ARGS__); \
			abort();                                            \
		}                                                          })

#define swap(a, b) do {            \
	__auto_type __tmp__ = (a); \
	(a) = (b); (b) = __tmp__;  \
} while (0)

#define SWITCH(val, type, body...) ({                      \
		inline type __fn__(typeof(+val) __param__) \
		{ switch(__param__) {body}; };             \
		__fn__(val);                              })

/** cleanup trick to turn slices into c-strings */
typedef struct { char *ptr; char backup; } slice_temp_t;
void slice_temp_cleanup (slice_temp_t *this) { *this->ptr = this->backup; }

#define SLICE_TMP_STR(sl)                                             \
	slice_temp_t __attribute__((cleanup(slice_temp_cleanup)))     \
		__SL__ ## __LINE__ = {                                \
			(sl).ptr + (sl).length, (sl).ptr[(sl).length] \
		};                                                    \
	(sl).ptr[(sl).length] = '\0'

/** position structs */
#define POS_DECL(name, bits) typedef struct __attribute__((packed)) { uint32_t pos: bits; } name
#define POS_ADV(p, x) ((typeof(p)){ (p).pos + x })
#define POS_DIFF(a, b) ((b).pos - (a).pos)
#define POS_CMP(a, b) ((int32_t)(a).pos - (int32_t)(b).pos)
#define POS_OK(a) ((a).pos != 0)

