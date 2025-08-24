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

#define SWITCH(val, type, body...) ({                      \
		inline type __fn__(typeof(+val) __param__) \
		{ switch(__param__) {body}; };             \
		__fn__(val);                              })


#define POS_DECL(name, bits) typedef struct __attribute__((packed)) { uint32_t pos: bits; } name
#define POS_ADV(p, x) ((typeof(p)){ (p).pos + x })
#define POS_DIFF(a, b) ((b).pos - (a).pos)
#define POS_CMP(a, b) ((int32_t)(a).pos - (int32_t)(b).pos)

