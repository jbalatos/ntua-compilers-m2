ifeq ($(shell command -v llvm-config-16 2>/dev/null),)
LLVMCONFIG = llvm-config
else
LLVMCONFIG = llvm-config-16
endif

CC = clang
CFLAGS=-Wall -Wextra -Werror -g -std=gnu17 -fblocks -fPIE -Wno-unknown-pragmas `$(LLVMCONFIG) --cflags`
LDLIBS=-lBlocksRuntime `$(LLVMCONFIG) --ldflags --system-libs --libs all`
SRC = main.c
DIST = main

main:

include $(SRC:.c=.d)	

%.d: %.c
	@set -e; rm -f $@; \
	$(CC) -M $(CFLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

.PHONY: blocks
ifeq ($(shell uname -s), Darwin)
blocks:
	git clone https://github.com/mackyle/blocksruntime
	pushd blocksruntime
	./buildlib-osx
	./checktests
	sudo ./installlib
	popd
else
blocks:
	git clone https://github.com/mackyle/blocksruntime
	pushd blocksruntime
	./buildlib
	./checktests
	sudo ./installlib
	popd
endif

.PHONY: clean
clean:
	rm -f *.o *.d *.d.* tags $(DIST)
