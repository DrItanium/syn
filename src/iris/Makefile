# iris - simple 8-bit CPU core
# See LICENSE file for copyright and license details.

include config.mk

# TODO SOURCES = $(wildcard *.c)
SOURCES = iris.c sim.c
OBJECTS = ${SOURCES:.c=.o}
TARGETS = iris

TEST_SOURCES = $(wildcard tests/*.c)
TEST_OBJECTS = ${TEST_SOURCES:tests/%.c=tests/%.o}
TEST_TARGETS = ${TEST_OBJECTS:%.o=%}

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

iris: iris.o sim.o
	@echo -n Building $@ binary out of iris.o sim.o...
	@${CC} ${LDFLAGS} -o $@ iris.o sim.o
	@echo done.

# TODO use some `make` magic to automatically create build targets in the same
# pattern as below for all the TEST_TARGETS

test_decode: iris.o tests/decode.o
	@echo -n Building iris binary out of iris.o tests/decode.o...
	@${CC} ${LDFLAGS} -o iris iris.o tests/decode.o
	@echo done.

test_mnemonic: iris.o mnemonic.o tests/mnemonic.o
	@echo -n Building iris binary out of iris.o mnemonic.o tests/mnemonic.o...
	@${CC} ${LDFLAGS} -o iris iris.o mnemonic.o tests/mnemonic.o
	@echo done.

test_unparse: iris.o mnemonic.o unparse.o tests/unparse.o
	@echo -n Building iris binary out of iris.o mnemonic.o unparse.o tests/unparse.o...
	@${CC} ${LDFLAGS} -o iris iris.o mnemonic.o unparse.o tests/unparse.o
	@echo done.

clean:
	@echo -n Cleaning...
	@rm -f ${OBJECTS} ${TARGETS} ${TEST_OBJECTS} ${TEST_TARGETS}
	@echo done.

.PHONY: all options clean
