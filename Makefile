CFLAGS=-Wall -Werror -g -std=gnu11 -D_XOPEN_SOURCE=700
SRC = main.c
DIST = main

main:

include $(SRC:.c=.d)

%.d: %.c
	@set -e; rm -f $@; \
	$(CC) -M $(CPPFLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

.PHONY: clean
clean:
	rm -f *.o *.d tags $(DIST)
