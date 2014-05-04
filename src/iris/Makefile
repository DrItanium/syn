# iris - simple 16-bit CPU core
# See LICENSE file for copyright and license details.

include config.mk

OBJECTS = $(patsubst %.c,%.o,$(filter-out cmd/sim.c,$(wildcard *.c)))
RL_OBJECTS = $(patsubst %.c,%.o,$(filter-out cmd/img.c,$(wildcard *.c)))
TEST_OBJECTS = $(patsubst %.c,%.o,$(wildcard cmd/tests/*.c))
DECODE_OBJECTS = $(patsubst %.c,%.o,$(filter-out cmd/decode.c,$(wildcard *.c)))

ALL_BINARIES = ${BINARY} ${RL_BINARY} ${DECODE_BINARY} ${ASM_BINARY}
ALL_OBJECTS = ${OBJECTS} ${RL_OBJECTS} ${TEST_OBJECTS} ${DECODE_OBJECTS}
ALL_MAINS = ${MAIN} ${RL_MAIN} ${DECODE_MAIN}

all: options iris rl decode asm

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

rl: ${RL_MAIN} ${RL_OBJECTS}
	@echo -n Building ${RL_BINARY} binary out of $^...
	@${CC} ${LDFLAGS} -o ${RL_BINARY} $^
	@echo done.

decode: ${DECODE_MAIN} ${DECODE_OBJECTS}
	@echo -n Building ${DECODE_BINARY} binary out of $^...
	@${CC} ${LDFLAGS} -o ${DECODE_BINARY} $^
	@echo done.

asm: 
	@cd cmd/asm && $(MAKE)
	@mv cmd/asm/irisasm .


test_%: ${OBJECTS} cmd/tests/%.o
	@echo -n Building ${BINARY} binary out of $^...
	@${CC} ${LDFLAGS} -o ${BINARY} $^
	@echo done.

clean:
	@echo -n Cleaning...
	@rm -f ${ALL_BINARIES} ${ALL_OBJECTS} ${ALL_MAINS} ${ASM_OBJECTS} ${ALL_FILES}
	@cd cmd/asm && $(MAKE) clean
	@echo done.

.SECONDARY: ${TEST_OBJECTS}

.PHONY: all options clean
