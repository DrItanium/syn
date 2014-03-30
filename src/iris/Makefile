# iris - simple 8-bit CPU core
# See LICENSE file for copyright and license details.

include config.mk

OBJECTS = $(patsubst %.c,%.o,$(filter-out cmd/sim.c,$(wildcard *.c)))

TEST_OBJECTS = $(patsubst %.c,%.o,$(wildcard cmd/tests/*.c))

all: options iris

options:
	@echo iris build options:
	@echo "CFLAGS   = ${CFLAGS}"
	@echo "LDFLAGS  = ${LDFLAGS}"
	@echo "CC       = ${CC}"

%.o: %.c
	@echo -n Compiling $< into $@...
	@${CC} ${CFLAGS} -c $< -o $@
	@echo done.

iris: ${MAIN} ${OBJECTS}
	@echo -n Building ${BINARY} binary out of $^...
	@${CC} ${LDFLAGS} -o ${BINARY} $^
	@echo done.

test_%: ${OBJECTS} cmd/tests/%.o
	@echo -n Building ${BINARY} binary out of $^...
	@${CC} ${LDFLAGS} -o ${BINARY} $^
	@echo done.

clean:
	@echo -n Cleaning...
	@rm -f ${BINARY} ${MAIN} ${OBJECTS} ${TEST_OBJECTS}
	@echo done.

.SECONDARY: ${TEST_OBJECTS}

.PHONY: all options clean
