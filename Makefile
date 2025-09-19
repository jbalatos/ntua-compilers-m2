CC = clang
CFLAGS=-Wall -Werror -g -std=c23 -fblocks  -Wno-unknown-pragmas
LDLIBS=-lBlocksRuntime
SRC = main.c
DIST = main

main:

include $(SRC:.c=.d)

%.d: %.c
	@set -e; rm -f $@; \
	$(CC) -M $(CFLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

.PHONY: clean
clean:
	rm -f *.o *.d *.d.* tags $(DIST)
