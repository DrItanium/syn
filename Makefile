# iris - simple 16-bit CPU core
# See LICENSE file for copyright and license details.

include config.mk

IRIS16_OBJECTS = $(patsubst %.cc,%.o, $(wildcard src/target/iris16/lib/*.cc))
IRIS16_OUT = src/target/iris16/libiris16.a

IRIS16_SIM_BINARY = iris16
IRIS16_SIM_MAIN = src/target/iris16/cmd/sim.o
IRIS16_SIM_OBJECTS = ${IRIS16_SIM_MAIN}

IRIS16_LINK_BINARY = iris16link
IRIS16_LINK_MAIN = src/target/iris16/cmd/link.o
IRIS16_LINK_OBJECTS = ${IRIS16_LINK_MAIN}

IRIS16_ASM_BINARY = iris16asm
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
#TEST_OBJECTS = $(patsubst %.c,%.o,$(wildcard src/cmd/tests/*.c))
# STACK64 stuffs
STACK64_OBJECTS = $(patsubst %.cc,%.o, $(wildcard src/target/stack64/lib/*.cc))
STACK64_OUT = src/target/stack64/libstack64.a

STACK64_SIM_BINARY = stack64
STACK64_SIM_MAIN = src/target/stack64/cmd/sim.o
STACK64_SIM_OBJECTS = ${STACK64_SIM_MAIN}

STACK64_LINK_BINARY = stack64link
STACK64_LINK_MAIN = src/target/stack64/cmd/link.o
STACK64_LINK_OBJECTS = ${STACK64_SIM_MAIN}

STACK64_ASM_BINARY = stack64asm
STACK64_ASM_BASE = src/target/stack64/cmd/asm

STACK64_ASM_FILES = ${STACK64_ASM_BASE}/lex.yy.c \
				   ${STACK64_ASM_BASE}/asm.tab.c \
				   ${STACK64_ASM_BASE}/asm.tab.h

STACK64_ASM_OBJECTS = ${STACK64_ASM_BASE}/lex.yy.o \
					 ${STACK64_ASM_BASE}/asm.tab.o

ALL_STACK64_OBJECTS = ${STACK64_OBJECTS} \
					 ${STACK64_SIM_OBJECTS} \
					 ${STACK64_LINK_OBJECTS} \
					 ${STACK64_OUT} \
					 ${STACK64_ASM_OBJECTS} \
					 ${STACK64_ASM_FILES}

STACK64_BINARIES = ${STACK64_SIM_BINARY} \
				  ${STACK64_LINK_BINARY} \
				  ${STACK64_ASM_BINARY}
STACK64_TOOLS = ${STACK64_BINARIES}

STACK64 = ${STACK64_OUT} \
		 ${STACK64_TOOLS}

ALL_BINARIES = ${IRIS16_BINARIES} \
			   ${STACK64_BINARIES}
ALL_OBJECTS = ${ALL_IRIS16_OBJECTS} \
			  ${ALL_STACK64_OBJECTS}
# NOTE: stack64 isn't complete and hasn't been ported over so it is currently
# broken
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
	@${CXX} ${LDFLAGS} -o bin/${IRIS16_SIM_BINARY} $^
	@echo done.

${IRIS16_LINK_BINARY}: ${IRIS16_LINK_MAIN} ${IRIS16_OUT}
	@echo -n Building ${IRIS16_LINK_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o bin/${IRIS16_LINK_BINARY} $^
	@echo done.

${IRIS16_ASM_BASE}/asm.tab.c ${IRIS16_ASM_BASE}/asm.tab.h: ${IRIS16_ASM_BASE}/asm.y
	@${YACC} -o ${IRIS16_ASM_BASE}/asm.tab.c -d ${IRIS16_ASM_BASE}/asm.y
	@${CXX} ${CXXFLAGS} -c ${IRIS16_ASM_BASE}/asm.tab.c -o ${IRIS16_ASM_BASE}/asm.tab.o

${IRIS16_ASM_BASE}/lex.yy.c: ${IRIS16_ASM_BASE}/asm.l ${IRIS16_ASM_BASE}/asm.tab.h
	@${LEX} -o ${IRIS16_ASM_BASE}/lex.yy.c -l ${IRIS16_ASM_BASE}/asm.l
	@${CXX} ${CXXFLAGS} -D_POSIX_SOURCE -c ${IRIS16_ASM_BASE}/lex.yy.c -o ${IRIS16_ASM_BASE}/lex.yy.o

${IRIS16_ASM_BINARY}: ${IRIS16_ASM_BASE}/lex.yy.c ${IRIS16_ASM_BASE}/asm.tab.c ${IRIS16_ASM_BASE}/asm.tab.h 
	@echo -n Building ${IRIS16_ASM_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o bin/${IRIS16_ASM_BINARY} ${IRIS16_ASM_BASE}/lex.yy.o ${IRIS16_ASM_BASE}/asm.tab.o ${IRIS16_OUT}
	@echo done.

${STACK64_OUT}: ${STACK64_OBJECTS}
	@echo -n Building ${STACK64_OUT} out of $^...
	@${AR} rcs ${STACK64_OUT}  $^
	@echo done

${STACK64_SIM_BINARY}: ${STACK64_SIM_MAIN} ${STACK64_OUT}
	@echo -n Building ${STACK64_SIM_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o bin/${STACK64_SIM_BINARY} $^
	@echo done.

${STACK64_LINK_BINARY}: ${STACK64_LINK_MAIN} ${STACK64_OUT}
	@echo -n Building ${STACK64_LINK_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o bin/${STACK64_LINK_BINARY} $^
	@echo done.

${STACK64_ASM_BASE}/asm.tab.c ${STACK64_ASM_BASE}/asm.tab.h: ${STACK64_ASM_BASE}/asm.y
	@${YACC} -o ${STACK64_ASM_BASE}/asm.tab.c -d ${STACK64_ASM_BASE}/asm.y
	@${CXX} ${CXXFLAGS} -c ${STACK64_ASM_BASE}/asm.tab.c -o ${STACK64_ASM_BASE}/asm.tab.o

${STACK64_ASM_BASE}/lex.yy.c: ${STACK64_ASM_BASE}/asm.l ${STACK64_ASM_BASE}/asm.tab.h
	@${LEX} -o ${STACK64_ASM_BASE}/lex.yy.c -l ${STACK64_ASM_BASE}/asm.l
	@${CXX} ${CXXFLAGS} -D_POSIX_SOURCE -c ${STACK64_ASM_BASE}/lex.yy.c -o ${STACK64_ASM_BASE}/lex.yy.o

${STACK64_ASM_BINARY}: ${STACK64_ASM_BASE}/lex.yy.c ${STACK64_ASM_BASE}/asm.tab.c ${STACK64_ASM_BASE}/asm.tab.h 
	@echo -n Building ${STACK64_ASM_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o bin/${STACK64_ASM_BINARY} ${STACK64_ASM_BASE}/lex.yy.o ${STACK64_ASM_BASE}/asm.tab.o ${STACK64_OUT}
	@echo done.

#test_%: src/cmd/tests/%.o ${LIBIRIS_OUT}
#	@echo -n Building ${SIM_BINARY} binary out of $^...
#	${CC} ${LDFLAGS} -o ${SIM_BINARY} $^
#	@echo done.

clean:
	@echo -n Cleaning...
	@rm -f ${ALL_OBJECTS} 
	@for n in $(ALL_BINARIES); do \
		rm -f bin/$$n; \
	done
	@echo done.

install: ${ALL_BINARIES}
	@echo installing executables to ${DESTDIR}${PREFIX}/bin
	@mkdir -p ${DESTDIR}${PREFIX}/bin
	@for n in $(ALL_BINARIES); do \
		cp bin/$$n ${DESTDIR}${PREFIX}/bin/$$n; \
		chmod 755 ${DESTDIR}${PREFIX}/bin/$$n; \
	done
	
uninstall:
	@echo removing executables from ${DESTDIR}${PREFIX}/bin
	@for n in $(ALL_BINARIES); do \
		rm -f ${DESTDIR}${PREFIX}/bin/$$n ; \
	done

.SECONDARY: ${TEST_OBJECTS}

.PHONY: all options clean install uninstall
