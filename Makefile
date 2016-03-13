# iris - simple 16-bit CPU core
# See LICENSE file for copyright and license details.

include config.mk

IRIS16_OBJECTS = iris16.o \

IRIS16_OUT = libiris16.a

IRIS16_SIM_BINARY = iris16
IRIS16_SIM_MAIN = iris16_sim.o
IRIS16_SIM_OBJECTS = ${IRIS16_SIM_MAIN}

IRIS16_LINK_BINARY = iris16link
IRIS16_LINK_MAIN = iris16_link.o
IRIS16_LINK_OBJECTS = ${IRIS16_LINK_MAIN}


IRIS16_STRGEN_BINARY = iris16strgen
IRIS16_STRGEN_MAIN = iris16_strgen.o
IRIS16_STRGEN_OBJECTS = ${IRIS16_STRGEN_MAIN} 

IRIS16_ASM_BINARY = iris16asm

IRIS16_ASM_FILES = iris16_lex.yy.c \
				   iris16_asm.tab.c \
				   iris16_asm.tab.h

IRIS16_ASM_OBJECTS = iris16_lex.yy.o \
					 iris16_asm.tab.o

ALL_IRIS16_OBJECTS = ${IRIS16_OBJECTS} \
					 ${IRIS16_SIM_OBJECTS} \
					 ${IRIS16_LINK_OBJECTS} \
					 ${IRIS16_OUT} \
					 ${IRIS16_ASM_OBJECTS} \
					 ${IRIS16_ASM_FILES} \
					 ${IRIS16_STRGEN_OBJECTS}

IRIS16_BINARIES = ${IRIS16_SIM_BINARY} \
				  ${IRIS16_LINK_BINARY} \
				  ${IRIS16_ASM_BINARY} \
				  ${IRIS16_STRGEN_BINARY}

IRIS16_TOOLS = ${IRIS16_BINARIES}

IRIS16 = ${IRIS16_OUT} \
		 ${IRIS16_TOOLS}

# The object file that defines main()
#TEST_OBJECTS = $(patsubst %.c,%.o,$(wildcard src/cmd/tests/*.c))

ALL_BINARIES = ${IRIS16_BINARIES}

ALL_OBJECTS = ${ALL_IRIS16_OBJECTS}

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

${IRIS16_STRGEN_BINARY}: ${IRIS16_STRGEN_MAIN} ${IRIS16_OUT}
	@echo -n Building ${IRIS16_STRGEN_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o ${IRIS16_STRGEN_BINARY} $^
	@echo done.


iris16_asm.tab.c iris16_asm.tab.h: iris16_asm.y
	@${YACC} -o iris16_asm.tab.c -d iris16_asm.y
	@${CXX} ${CXXFLAGS} -c iris16_asm.tab.c -o iris16_asm.tab.o

iris16_lex.yy.c: iris16_asm.l iris16_asm.tab.h
	@${LEX} -o iris16_lex.yy.c -l iris16_asm.l
	@${CXX} ${CXXFLAGS} -D_POSIX_SOURCE -c iris16_lex.yy.c -o iris16_lex.yy.o

${IRIS16_ASM_BINARY}: iris16_lex.yy.c iris16_asm.tab.c iris16_asm.tab.h 
	@echo -n Building ${IRIS16_ASM_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o ${IRIS16_ASM_BINARY} iris16_lex.yy.o iris16_asm.tab.o ${IRIS16_OUT}
	@echo done.

#test_%: src/cmd/tests/%.o ${LIBIRIS_OUT}
#	@echo -n Building ${SIM_BINARY} binary out of $^...
#	${CC} ${LDFLAGS} -o ${SIM_BINARY} $^
#	@echo done.

clean:
	@echo -n Cleaning...
	@rm -f ${ALL_OBJECTS} ${ALL_BINARIES}
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


iris16.o: iris16.cc iris16.h iris_base.h Core.h iris16_groups.def \
 iris16_arithmetic.def iris16_misc.def iris16_jump.def \
 iris16_syscalls.def iris16_move.def iris16_compare.def \
 iris16_instruction.def
iris16_link.o: iris16_link.cc iris16.h iris_base.h Core.h \
 iris16_groups.def iris16_arithmetic.def iris16_misc.def iris16_jump.def \
 iris16_syscalls.def iris16_move.def iris16_compare.def \
 iris16_instruction.def
iris16_sim.o: iris16_sim.cc iris16.h iris_base.h Core.h iris16_groups.def \
 iris16_arithmetic.def iris16_misc.def iris16_jump.def \
 iris16_syscalls.def iris16_move.def iris16_compare.def \
 iris16_instruction.def
iris16_strgen.o: iris16_strgen.cc
