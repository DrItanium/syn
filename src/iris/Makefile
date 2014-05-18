# iris - simple 16-bit CPU core
# See LICENSE file for copyright and license details.

include config.mk

LIBIRIS_OBJECTS = $(patsubst %.c,%.o, $(wildcard src/libiris/*.c))
OBJECTS = $(patsubst %.c,%.o,$(filter-out cmd/sim.c,$(wildcard *.c)))
RL_OBJECTS = $(patsubst %.c,%.o,$(filter-out cmd/img.c,$(wildcard *.c)))
TEST_OBJECTS = $(patsubst %.c,%.o,$(wildcard cmd/tests/*.c))
DECODE_OBJECTS = $(patsubst %.c,%.o,$(filter-out cmd/decode.c,$(wildcard *.c)))
DBG_OBJECTS = $(patsubst %.c,%.o,$(filter-out cmd/dbg.c,$(wildcard *.c)))

ALL_BINARIES = ${BINARY} ${RL_BINARY} ${DECODE_BINARY} ${ASM_BINARY} ${DBG_BINARY}
ALL_OBJECTS = ${OBJECTS} ${RL_OBJECTS} ${TEST_OBJECTS} ${DECODE_OBJECTS} ${DBG_OBJECTS}
ALL_MAINS = ${MAIN} ${RL_MAIN} ${DECODE_MAIN} ${DBG_MAIN}
LIBIRIS_OUT = src/libiris/libiris.a

all: options libiris
# iris rl decode asm dbg

options:
	@echo iris build options:
	@echo "CFLAGS   = ${CFLAGS}"
	@echo "LDFLAGS  = ${LDFLAGS}"
	@echo "CC       = ${CC}"


%.o: %.c
	@echo -n Compiling $< into $@...
	@${CC} ${CFLAGS} -c $< -o $@
	@echo done.
libiris: ${LIBIRIS_OBJECTS}
	@echo -n Building libiris out of $^...
	@${AR} rcs ${LIBIRIS_OUT}  $^
	@echo done
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

dbg: ${DBG_MAIN} ${DBG_OBJECTS}
	@echo -n Building ${DBG_BINARY} binary out of $^...
	@${CC} ${LDFLAGS} -o ${DBG_BINARY} $^
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
	@rm -f ${LIBIRIS_OUT} ${LIBIRIS_OBJECTS}
#	@rm -f ${ALL_BINARIES} ${ALL_OBJECTS} ${ALL_MAINS} ${ASM_OBJECTS} ${ALL_FILES}
#	@cd cmd/asm && $(MAKE) clean
	@echo done.

install: 
	@echo installing executables to ${DESTDIR}${PREFIX}/bin
	@mkdir -p ${DESTDIR}${PREFIX}/bin
	@cp -f ${ALL_BINARIES} ${DESTDIR}${PREFIX}/bin
	@chmod 755 ${DESTDIR}${PREFIX}/bin/${BINARY}
	@chmod 755 ${DESTDIR}${PREFIX}/bin/${RL_BINARY}
	@chmod 755 ${DESTDIR}${PREFIX}/bin/${DECODE_BINARY}
	@chmod 755 ${DESTDIR}${PREFIX}/bin/${ASM_BINARY}
	@chmod 755 ${DESTDIR}${PREFIX}/bin/${DBG_BINARY}

uninstall:
	@echo removing executables from ${DESTDIR}${PREFIX}/bin
	@rm -f ${DESTDIR}${PREFIX}/bin/${BINARY}
	@rm -f ${DESTDIR}${PREFIX}/bin/${RL_BINARY}
	@rm -f ${DESTDIR}${PREFIX}/bin/${DECODE_BINARY}
	@rm -f ${DESTDIR}${PREFIX}/bin/${ASM_BINARY}
	@rm -f ${DESTDIR}${PREFIX}/bin/${DBG_BINARY}

.SECONDARY: ${TEST_OBJECTS}

.PHONY: all options clean install uninstall
