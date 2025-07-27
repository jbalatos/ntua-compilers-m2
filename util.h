#pragma once

#define dbg(x, fmt) \
	printf("DBG:\t%s:%d\t" #x ":\t" fmt "\n", __FILE__, __LINE__, x)

