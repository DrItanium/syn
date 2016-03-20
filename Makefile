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

IRIS64_OBJECTS = iris64.o \

IRIS64_OUT = libiris64.a

IRIS64_SIM_BINARY = iris64
IRIS64_SIM_MAIN = iris64_sim.o
IRIS64_SIM_OBJECTS = ${IRIS64_SIM_MAIN}

IRIS64_LINK_BINARY = iris64link
IRIS64_LINK_MAIN = iris64_link.o
IRIS64_LINK_OBJECTS = ${IRIS64_LINK_MAIN}


IRIS64_STRGEN_BINARY = iris64strgen
IRIS64_STRGEN_MAIN = iris64_strgen.o
IRIS64_STRGEN_OBJECTS = ${IRIS64_STRGEN_MAIN} 

IRIS64_ASM_BINARY = iris64asm

IRIS64_ASM_FILES = iris64_lex.yy.c \
				   iris64_asm.tab.c \
				   iris64_asm.tab.h

IRIS64_ASM_OBJECTS = iris64_lex.yy.o \
					 iris64_asm.tab.o

ALL_IRIS64_OBJECTS = ${IRIS64_OBJECTS} \
					 ${IRIS64_SIM_OBJECTS} \
					 ${IRIS64_LINK_OBJECTS} \
					 ${IRIS64_OUT} \
					 ${IRIS64_ASM_OBJECTS} \
					 ${IRIS64_ASM_FILES} \
					 ${IRIS64_STRGEN_OBJECTS}

IRIS64_BINARIES = ${IRIS64_SIM_BINARY} \
				  ${IRIS64_LINK_BINARY} \
				  ${IRIS64_ASM_BINARY} \
				  ${IRIS64_STRGEN_BINARY}

IRIS64_TOOLS = ${IRIS64_BINARIES}

IRIS64 = ${IRIS64_OUT} \
		 ${IRIS64_TOOLS}

# The object file that defines main()
#TEST_OBJECTS = $(patsubst %.c,%.o,$(wildcard src/cmd/tests/*.c))

ALL_BINARIES = ${IRIS16_BINARIES} \
			   ${IRIS64_BINARIES}

ALL_OBJECTS = ${ALL_IRIS16_OBJECTS} \
			  ${ALL_IRIS64_OBJECTS}

all: options ${IRIS16} ${IRIS64}

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

# BEGIN IRIS64
#
${IRIS64_OUT}: ${IRIS64_OBJECTS}
	@echo -n Building ${IRIS64_OUT} out of $^...
	@${AR} rcs ${IRIS64_OUT}  $^
	@echo done

${IRIS64_SIM_BINARY}: ${IRIS64_SIM_MAIN} ${IRIS64_OUT}
	@echo -n Building ${IRIS64_SIM_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o ${IRIS64_SIM_BINARY} $^
	@echo done.

${IRIS64_LINK_BINARY}: ${IRIS64_LINK_MAIN} ${IRIS64_OUT}
	@echo -n Building ${IRIS64_LINK_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o ${IRIS64_LINK_BINARY} $^
	@echo done.

${IRIS64_STRGEN_BINARY}: ${IRIS64_STRGEN_MAIN} ${IRIS64_OUT}
	@echo -n Building ${IRIS64_STRGEN_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o ${IRIS64_STRGEN_BINARY} $^
	@echo done.


iris64_asm.tab.c iris64_asm.tab.h: iris64_asm.y
	@${YACC} -o iris64_asm.tab.c -d iris64_asm.y
	@${CXX} ${CXXFLAGS} -c iris64_asm.tab.c -o iris64_asm.tab.o

iris64_lex.yy.c: iris64_asm.l iris64_asm.tab.h
	@${LEX} -o iris64_lex.yy.c -l iris64_asm.l
	@${CXX} ${CXXFLAGS} -D_POSIX_SOURCE -c iris64_lex.yy.c -o iris64_lex.yy.o

${IRIS64_ASM_BINARY}: iris64_lex.yy.c iris64_asm.tab.c iris64_asm.tab.h 
	@echo -n Building ${IRIS64_ASM_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o ${IRIS64_ASM_BINARY} iris64_lex.yy.o iris64_asm.tab.o ${IRIS64_OUT}
	@echo done.

clean:
	@echo -n Cleaning...
	@rm -f ${ALL_OBJECTS} ${ALL_BINARIES}
	@echo done.

install: ${ALL_BINARIES}
	@echo installing executables to ${DESTDIR}${PREFIX}/bin
	@mkdir -p ${DESTDIR}${PREFIX}/bin
	@for n in $(ALL_BINARIES); do \
		cp $$n ${DESTDIR}${PREFIX}/bin/$$n; \
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

iris64.o: iris64.cc iris64.h iris_base.h Core.h iris64_groups.def \
 iris64_arithmetic.def iris64_misc.def iris64_jump.def \
 iris64_syscalls.def iris64_move.def iris64_compare.def \
 iris64_instruction.def
iris64_link.o: iris64_link.cc iris16.h iris_base.h Core.h \
 iris16_groups.def iris16_arithmetic.def iris16_misc.def iris16_jump.def \
 iris16_syscalls.def iris16_move.def iris16_compare.def \
 iris16_instruction.def
iris64_sim.o: iris64_sim.cc iris16.h iris_base.h Core.h iris16_groups.def \
 iris16_arithmetic.def iris16_misc.def iris16_jump.def \
 iris16_syscalls.def iris16_move.def iris16_compare.def \
 iris16_instruction.def
iris64_strgen.o: iris64_strgen.cc
