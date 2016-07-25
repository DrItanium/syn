# iris - simple 16-bit CPU core
# See LICENSE file for copyright and license details.

include config.mk

IRIS16_OBJECTS = iris16.o \

IRIS16_OUT = libiris16.a

IRIS16_SIM_BINARY = iris16_sim
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

IRIS17_OBJECTS = iris17.o \

IRIS17_OUT = libiris17.a

IRIS17_SIM_BINARY = iris17_sim
IRIS17_SIM_MAIN = iris17_sim.o
IRIS17_SIM_OBJECTS = ${IRIS17_SIM_MAIN}

IRIS17_LINK_BINARY = iris17link
IRIS17_LINK_MAIN = iris17_link.o
IRIS17_LINK_OBJECTS = ${IRIS17_LINK_MAIN}


IRIS17_STRGEN_BINARY = iris17strgen
IRIS17_STRGEN_MAIN = iris17_strgen.o
IRIS17_STRGEN_OBJECTS = ${IRIS17_STRGEN_MAIN} 

IRIS17_ASM_BINARY = iris17asm

IRIS17_ASM_FILES = iris17_lex.yy.c \
				   iris17_asm.tab.c \
				   iris17_asm.tab.h

IRIS17_ASM_OBJECTS = iris17_lex.yy.o \
					 iris17_asm.tab.o

ALL_IRIS17_OBJECTS = ${IRIS17_OBJECTS} \
					 ${IRIS17_SIM_OBJECTS} \
					 ${IRIS17_LINK_OBJECTS} \
					 ${IRIS17_OUT} \
					 ${IRIS17_ASM_OBJECTS} \
					 ${IRIS17_ASM_FILES} \
					 ${IRIS17_STRGEN_OBJECTS}

IRIS17_BINARIES = ${IRIS17_SIM_BINARY} \
				  ${IRIS17_LINK_BINARY} \
				  ${IRIS17_ASM_BINARY} \
				  ${IRIS17_STRGEN_BINARY}

IRIS17_TOOLS = ${IRIS17_BINARIES}

IRIS17 = ${IRIS17_OUT} \
		 ${IRIS17_TOOLS}

IRIS32_OBJECTS = iris32.o \

IRIS32_OUT = libiris32.a

IRIS32_SIM_BINARY = iris32_sim
IRIS32_SIM_MAIN = iris32_sim.o
IRIS32_SIM_OBJECTS = ${IRIS32_SIM_MAIN}

IRIS32_STRGEN_BINARY = iris32strgen
IRIS32_STRGEN_MAIN = iris32_strgen.o
IRIS32_STRGEN_OBJECTS = ${IRIS32_STRGEN_MAIN} 

IRIS32_ASM_BINARY = iris32asm

IRIS32_ASM_FILES = iris32_lex.yy.c \
				   iris32_asm.tab.c \
				   iris32_asm.tab.h

IRIS32_ASM_OBJECTS = iris32_lex.yy.o \
					 iris32_asm.tab.o

ALL_IRIS32_OBJECTS = ${IRIS32_OBJECTS} \
					 ${IRIS32_SIM_OBJECTS} \
					 ${IRIS32_OUT} \
					 ${IRIS32_ASM_OBJECTS} \
					 ${IRIS32_ASM_FILES} \
					 ${IRIS32_STRGEN_OBJECTS}

IRIS32_BINARIES = ${IRIS32_SIM_BINARY} \
				  ${IRIS32_ASM_BINARY} \
				  ${IRIS32_STRGEN_BINARY}

IRIS32_TOOLS = ${IRIS32_BINARIES}

IRIS32 = ${IRIS32_OUT} \
		 ${IRIS32_TOOLS}


ARCH_TARGETS = ${IRIS16_OUT} \
			   ${IRIS32_OUT} \
			   ${IRIS17_OUT}

COMMON_THINGS = architecture.o \
				${ARCH_TARGETS}

# The object file that defines main()
#TEST_OBJECTS = $(patsubst %.c,%.o,$(wildcard src/cmd/tests/*.c))
SIM_OBJECTS = iris_sim.o \
			  sim_registration.o \
			  ${COMMON_THINGS}

SIM_BINARY = iris_sim

ASM_OBJECTS = iris_asm.o \
			  asm_interact.o \
			  ${COMMON_THINGS}

ASM_PARSERS = ${IRIS16_ASM_FILES} \
			  ${IRIS32_ASM_FILES} \
			  ${IRIS17_ASM_FILES} 

ASM_SUPPLIMENTARY_BUILD = ${IRIS16_ASM_OBJECTS} \
						  ${IRIS32_ASM_OBJECTS} \
						  ${IRIS17_ASM_OBJECTS} \

ASM_BINARY = iris_asm

STRGEN_BINARY = iris_strgen

STRGEN_OBJECTS = iris_strgen.o \
				 ${COMMON_THINGS}


ALL_BINARIES = ${SIM_BINARY} \
			   ${ASM_BINARY} \
			   ${STRGEN_BINARY}

ALL_OBJECTS = ${COMMON_THINGS} \
			  ${SIM_OBJECTS} \
			  ${ALL_IRIS32_OBJECTS} \
			  ${ALL_IRIS17_OBJECTS} \
			  ${ALL_IRIS16_OBJECTS} \
			  ${ASM_OBJECTS}

ALL_ARCHIVES = ${IRIS16_OUT} \
			   ${IRIS32_OUT} \
			   ${IRIS17_OUT}

all: options ${SIM_BINARY} ${ASM_BINARY} ${STRGEN_BINARY}

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


${SIM_BINARY}: ${SIM_OBJECTS}
	@echo -n Building ${SIM_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o ${SIM_BINARY} $^
	@echo done.

${STRGEN_BINARY}: ${STRGEN_OBJECTS}
	@echo -n Building ${STRGEN_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o ${STRGEN_BINARY} $^
	@echo done.

${ASM_BINARY}: ${ASM_OBJECTS} ${ASM_PARSERS} 
	@echo -n Building ${ASM_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o ${ASM_BINARY} ${ASM_SUPPLIMENTARY_BUILD} ${ASM_OBJECTS}
	@echo done.

# BEGIN IRIS16
#

${IRIS16_OUT}: ${IRIS16_OBJECTS}
	@echo -n Building ${IRIS16_OUT} out of $^...
	@${AR} rcs ${IRIS16_OUT}  $^
	@echo done


iris16_asm.tab.c iris16_asm.tab.h: iris16_asm.y
	@echo -n Constructing Parser from $< ...
	@${YACC} -o iris16_asm.tab.c -d iris16_asm.y
	@echo done
	@echo -n Compiling iris16_asm.tab.c into iris16_asm.tab.o ...
	@${CXX} ${CXXFLAGS} -c iris16_asm.tab.c -o iris16_asm.tab.o
	@echo done

iris16_lex.yy.c: iris16_asm.l iris16_asm.tab.h
	@echo -n Constructing Lexer from $< ...
	@${LEX} -o iris16_lex.yy.c iris16_asm.l
	@echo done
	@echo -n Compiling iris16_lex.yy.c into iris16_lex.yy.o ...
	@${CXX} ${CXXFLAGS} -D_POSIX_SOURCE -c iris16_lex.yy.c -o iris16_lex.yy.o
	@echo done

# BEGIN IRIS32
#
${IRIS32_OUT}: ${IRIS32_OBJECTS}
	@echo -n Building ${IRIS32_OUT} out of $^...
	@${AR} rcs ${IRIS32_OUT}  $^
	@echo done

iris32_asm.tab.c iris32_asm.tab.h: iris32_asm.y
	@echo -n Constructing Parser from $< ...
	@${YACC} -o iris32_asm.tab.c -d iris32_asm.y
	@echo done
	@echo -n Compiling iris32_asm.tab.c into iris32_asm.tab.o ...
	@${CXX} ${CXXFLAGS} -c iris32_asm.tab.c -o iris32_asm.tab.o
	@echo done

iris32_lex.yy.c: iris32_asm.l iris32_asm.tab.h
	@echo -n Constructing Lexer from $< ...
	@${LEX} -o iris32_lex.yy.c iris32_asm.l
	@echo done
	@echo -n Compiling iris32_lex.yy.c into iris32_lex.yy.o ...
	@${CXX} ${CXXFLAGS} -D_POSIX_SOURCE -c iris32_lex.yy.c -o iris32_lex.yy.o
	@echo done

# BEGIN IRIS17
#
#
${IRIS17_OUT}: ${IRIS17_OBJECTS}
	@echo -n Building ${IRIS17_OUT} out of $^...
	@${AR} rcs ${IRIS17_OUT}  $^
	@echo done

iris17_asm.tab.c iris17_asm.tab.h: iris17_asm.y
	@echo -n Constructing Parser from $< ...
	@${YACC} -o iris17_asm.tab.c -d iris17_asm.y
	@echo done
	@echo -n Compiling iris17_asm.tab.c into iris17_asm.tab.o ...
	@${CXX} ${CXXFLAGS} -c iris17_asm.tab.c -o iris17_asm.tab.o
	@echo done

iris17_lex.yy.c: iris17_asm.l iris17_asm.tab.h
	@echo -n Constructing Lexer from $< ...
	@${LEX} -o iris17_lex.yy.c iris17_asm.l
	@echo done
	@echo -n Compiling iris17_lex.yy.c into iris17_lex.yy.o ...
	@${CXX} ${CXXFLAGS} -D_POSIX_SOURCE -c iris17_lex.yy.c -o iris17_lex.yy.o
	@echo done

clean:
	@echo -n Cleaning...
	@rm -f ${ALL_OBJECTS} ${ALL_BINARIES} ${ASM_PARSERS}
	@echo done.

install: ${ALL_BINARIES}
	@echo installing executables to ${DESTDIR}${PREFIX}/bin
	@mkdir -p ${DESTDIR}${PREFIX}/bin
	@for n in $(ALL_BINARIES); do \
		cp $$n ${DESTDIR}${PREFIX}/bin/$$n; \
		chmod 755 ${DESTDIR}${PREFIX}/bin/$$n; \
	done
	@echo installing archives
	@mkdir -p ${DESTDIR}${PREFIX}/lib/iris/
	@for n in $(ALL_ARCHIVES); do \
		cp $$n ${DESTDIR}${PREFIX}/lib/iris/$$n; \
	done
	@echo installing headers
	@mkdir -p ${DESTDIR}${PREFIX}/include/iris/
	@cp -r *.h *.def ${DESTDIR}${PREFIX}/include/iris/
	
uninstall:
	@echo removing executables from ${DESTDIR}${PREFIX}/bin
	@for n in $(ALL_BINARIES); do \
		rm -f ${DESTDIR}${PREFIX}/bin/$$n ; \
	done
	@echo removing archives and headers
	@for n in ${ALL_ARCHIVES}; do \
		rm ${DESTDIR}${PREFIX}/lib/iris/$$n; \
	done
	@rmdir ${DESTDIR}${PREFIX}/lib/iris
	@rm -rf ${DESTDIR}${PREFIX}/include/iris

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

iris32.o: iris32.cc iris32.h iris_base.h Core.h iris32_groups.def \
 iris32_arithmetic.def iris32_misc.def iris32_jump.def \
 iris32_syscalls.def iris32_move.def iris32_compare.def \
 iris32_instruction.def
iris32_sim.o: iris32_sim.cc iris32.h iris_base.h Core.h iris32_groups.def \
 iris32_arithmetic.def iris32_misc.def iris32_jump.def \
 iris32_syscalls.def iris32_move.def iris32_compare.def \
 iris32_instruction.def
iris32_strgen.o: iris32_strgen.cc
