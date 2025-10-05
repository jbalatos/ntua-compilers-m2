ifeq ($(shell command -v llvm-config-16 2>/dev/null),)
LLVMCONFIG = llvm-config
else
LLVMCONFIG = llvm-config-16
endif


CC = clang
CFLAGS=-Wall -Wextra -Werror -std=gnu17 -fblocks -fPIE -Wno-unknown-pragmas `$(LLVMCONFIG) --cflags`
LDLIBS=-lBlocksRuntime -lstdc++ -lpthread `$(LLVMCONFIG) --ldflags --system-libs --libs all`

SRC=./src
INCLUDE=$(SRC)/* $(SRC)/lib.a
DIST=danac

danac: $(SRC)/main.c $(INCLUDE)
	$(CC) $(CFLAGS) $< -o $@ $(LDLIBS)
	

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
	rm -f tags

.PHONY: distclean
distclean: clean
	rm -f $(DIST)
