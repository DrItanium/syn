# iris - simple 16-bit CPU core
# See LICENSE file for copyright and license details.

include config.mk

IRIS16_SIM_BINARY = bin/iris16
IRIS16_SIM_MAIN = src/target/iris16/cmd/sim.o
IRIS16_SIM_OBJECTS = ${IRIS16_SIM_MAIN}

IRIS16_LINK_BINARY = bin/iris16link
IRIS16_LINK_MAIN = src/target/iris16/cmd/link.o
IRIS16_LINK_OBJECTS = ${IRIS16_SIM_MAIN}

IRIS16_OBJECTS = $(patsubst %.cc,%.o, $(wildcard src/target/iris16/lib/*.cc))
IRIS16_OUT = src/target/iris16/libiris16.a

IRIS16_ASM_BINARY = bin/iris16asm
IRIS16_ASM_BASE = src/target/iris16/cmd/asm

IRIS16_ASM_FILES = ${IRIS16_ASM_BASE}/lex.yy.c \
				   ${IRIS16_ASM_BASE}/asm.tab.c \
				   ${IRIS16_ASM_BASE}/asm.tab.h

IRIS16_ASM_OBJECTS = ${IRIS16_ASM_BASE}/lex.yy.o \
					 ${IRIS16_ASM_BASE}/asm.tab.o

ALL_IRIS16_OBJECTS = ${IRIS16_OBJECTS} \
					 ${IRIS16_SIM_OBJECTS} \
					 ${IRIS16_LINK_OBJECTS} \
					 ${IRIS16_OUT} \
					 ${IRIS16_ASM_OBJECTS} \
					 ${IRIS16_ASM_FILES}
IRIS16_BINARIES = ${IRIS16_SIM_BINARY} \
				  ${IRIS16_LINK_BINARY} \
				  ${IRIS16_ASM_BINARY}
IRIS16_TOOLS = ${IRIS16_BINARIES}

IRIS16 = ${IRIS16_OUT} \
		 ${IRIS16_TOOLS}

# The object file that defines main()
SIM_BINARY = iris
SIM_MAIN = src/cmd/sim.o
SIM_OBJECTS = src/cmd/sim.o

RL_BINARY = irislink
RL_MAIN = src/cmd/img.o

DECODE_BINARY = irisdecode
DECODE_MAIN = src/cmd/decode.o

DBG_BINARY = irisdbg
DBG_MAIN = src/cmd/dbg.o

ASM_BINARY = irisasm
ASM_BASE = src/cmd/asm

ASM_FILES = ${ASM_BASE}/lex.yy.c ${ASM_BASE}/asm.tab.c ${ASM_BASE}/asm.tab.h
ASM_OBJECTS = ${ASM_BASE}/lex.yy.o ${ASM_BASE}/asm.tab.o

LIBIRIS_OBJECTS = $(patsubst %.cc,%.o, $(wildcard src/libiris/*.cc))
LIBIRIS_OUT = src/libiris/libiris.a


TEST_OBJECTS = $(patsubst %.c,%.o,$(wildcard src/cmd/tests/*.c))

ALL_BINARIES = ${SIM_BINARY} ${RL_BINARY} ${DECODE_BINARY} ${ASM_BINARY}\
			   ${DBG_BINARY} ${IRIS16_BINARIES}
ALL_OBJECTS = ${LIBIRIS_OBJECTS} ${RL_MAIN} ${TEST_OBJECTS} ${DECODE_MAIN} \
			  ${SIM_MAIN} ${DBG_MAIN} ${ASM_FILES} ${ASM_OBJECTS} ${LIBIRIS_OUT} \
			  ${ALL_IRIS16_OBJECTS}

all: options ${IRIS16}

options:
	@echo iris build options:
	@echo "CFLAGS   = ${CFLAGS}"
	@echo "CXXFLAGS = ${CXXFLAGS}"
	@echo "LDFLAGS  = ${LDFLAGS}"
	@echo "CC       = ${CC}"
	@echo "CXX      = ${CXX}"


%.o: %.c
	@echo -n Compiling $< into $@...
	@${CC} ${CFLAGS} -c $< -o $@
	@echo done.

%.o: %.cc
	@echo -n Compiling $< into $@...
	@${CC} ${CXXFLAGS} -c $< -o $@
	@echo done.

${IRIS16_OUT}: ${IRIS16_OBJECTS}
	@echo -n Building ${IRIS16_OUT} out of $^...
	@${AR} rcs ${IRIS16_OUT}  $^
	@echo done

${IRIS16_SIM_BINARY}: ${IRIS16_SIM_MAIN} ${IRIS16_OUT}
	@echo -n Building ${IRIS16_SIM_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o ${IRIS16_SIM_BINARY} $^
	@echo done.

${IRIS16_LINK_BINARY}: ${IRIS16_LINK_MAIN} ${IRIS16_OUT}
	@echo -n Building ${IRIS16_LINK_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o ${IRIS16_LINK_BINARY} $^
	@echo done.

${IRIS16_ASM_BASE}/asm.tab.c ${IRIS16_ASM_BASE}/asm.tab.h: ${IRIS16_ASM_BASE}/asm.y
	@${YACC} -o ${IRIS16_ASM_BASE}/asm.tab.c -d ${IRIS16_ASM_BASE}/asm.y
	@${CXX} ${CXXFLAGS} -c ${IRIS16_ASM_BASE}/asm.tab.c -o ${IRIS16_ASM_BASE}/asm.tab.o

${IRIS16_ASM_BASE}/lex.yy.c: ${IRIS16_ASM_BASE}/asm.l ${IRIS16_ASM_BASE}/asm.tab.h
	@${LEX} -o ${IRIS16_ASM_BASE}/lex.yy.c -l ${IRIS16_ASM_BASE}/asm.l
	@${CXX} ${CXXFLAGS} -D_POSIX_SOURCE -c ${IRIS16_ASM_BASE}/lex.yy.c -o ${IRIS16_ASM_BASE}/lex.yy.o

${IRIS16_ASM_BINARY}: ${IRIS16_ASM_BASE}/lex.yy.c ${IRIS16_ASM_BASE}/asm.tab.c ${IRIS16_ASM_BASE}/asm.tab.h 
	@echo -n Building ${IRIS16_ASM_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o ${IRIS16_ASM_BINARY} ${IRIS16_ASM_BASE}/lex.yy.o ${IRIS16_ASM_BASE}/asm.tab.o ${IRIS16_OUT}
	@echo done.



${ASM_BASE}/asm.tab.c ${ASM_BASE}/asm.tab.h: ${ASM_BASE}/asm.y
	@${YACC} -o ${ASM_BASE}/asm.tab.c -d ${ASM_BASE}/asm.y
	@${CC} ${CFLAGS} -c ${ASM_BASE}/asm.tab.c -o ${ASM_BASE}/asm.tab.o

${ASM_BASE}/lex.yy.c: ${ASM_BASE}/asm.l ${ASM_BASE}/asm.tab.h
	@${LEX} -o ${ASM_BASE}/lex.yy.c -l ${ASM_BASE}/asm.l
	@${CC} ${CFLAGS} -D_POSIX_SOURCE -c ${ASM_BASE}/lex.yy.c -o ${ASM_BASE}/lex.yy.o

${LIBIRIS_OUT}: ${LIBIRIS_OBJECTS}
	@echo -n Building ${LIBIRIS_OUT} out of $^...
	@${AR} rcs ${LIBIRIS_OUT}  $^
	@echo done




iris: ${SIM_MAIN} ${LIBIRIS_OUT}
	@echo -n Building ${SIM_BINARY} binary out of $^...
	${CC} ${LDFLAGS} -o ${SIM_BINARY} $^
	@echo done.

rl: ${RL_MAIN} ${LIBIRIS_OUT}
	@echo -n Building ${RL_BINARY} binary out of $^...
	@${CC} ${LDFLAGS} -o ${RL_BINARY} $^
	@echo done.

decode: ${DECODE_MAIN} ${LIBIRIS_OUT}
	@echo -n Building ${DECODE_BINARY} binary out of $^...
	@${CC} ${LDFLAGS} -o ${DECODE_BINARY} $^
	@echo done.

dbg: ${DBG_MAIN} ${LIBIRIS_OUT} 
	@echo -n Building ${DBG_BINARY} binary out of $^...
	@${CC} ${LDFLAGS} -o ${DBG_BINARY} $^
	@echo done.

asm: ${ASM_BASE}/lex.yy.c ${ASM_BASE}/asm.tab.c ${ASM_BASE}/asm.tab.h src/libiris/util.c 
	@echo -n Building ${ASM_BINARY} binary out of $^...
	@${CC} ${LDFLAGS} -o ${ASM_BINARY} ${ASM_BASE}/lex.yy.o ${ASM_BASE}/asm.tab.o ${LIBIRIS_OUT}
	@echo done.

test_%: src/cmd/tests/%.o ${LIBIRIS_OUT}
	@echo -n Building ${SIM_BINARY} binary out of $^...
	${CC} ${LDFLAGS} -o ${SIM_BINARY} $^
	@echo done.

clean:
	@echo -n Cleaning...
	@rm -f ${ALL_OBJECTS} ${ALL_BINARIES}
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
