LLVMCONFIG = llvm-config-16

CC = clang
CFLAGS=-Wall -Wextra -Werror -g -std=gnu17 -fblocks  -Wno-unknown-pragmas `$(LLVMCONFIG) --cflags`
LDLIBS=-lBlocksRuntime `$(LLVMCONFIG) --ldflags --system-libs --libs all`
SRC = test.c
DIST = test

test:

include $(SRC:.c=.d)	

%.d: %.c
	@set -e; rm -f $@; \
	$(CC) -M $(CFLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$