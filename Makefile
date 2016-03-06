# iris - simple 16-bit CPU core
# See LICENSE file for copyright and license details.

include config.mk

IRIS16_OBJECTS = Arithmetic.o \
				 Compare.o \
				 Core.o \
				 DecodedInstruction.o \
				 Encoder.o \
				 Jump.o \
				 Misc.o \
				 Move.o

IRIS16_OUT = libiris16.a

IRIS16_SIM_BINARY = iris16
IRIS16_SIM_MAIN = sim.o
IRIS16_SIM_OBJECTS = ${IRIS16_SIM_MAIN}

IRIS16_LINK_BINARY = iris16link
IRIS16_LINK_MAIN = link.o
IRIS16_LINK_OBJECTS = ${IRIS16_LINK_MAIN}

IRIS16_ASM_BINARY = iris16asm

IRIS16_ASM_FILES = lex.yy.c \
				   asm.tab.c \
				   asm.tab.h

IRIS16_ASM_OBJECTS = lex.yy.o \
					 asm.tab.o

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

asm.tab.c asm.tab.h: asm.y
	@${YACC} -o asm.tab.c -d asm.y
	@${CXX} ${CXXFLAGS} -c asm.tab.c -o asm.tab.o

lex.yy.c: asm.l asm.tab.h
	@${LEX} -o lex.yy.c -l asm.l
	@${CXX} ${CXXFLAGS} -D_POSIX_SOURCE -c lex.yy.c -o lex.yy.o

${IRIS16_ASM_BINARY}: lex.yy.c asm.tab.c asm.tab.h 
	@echo -n Building ${IRIS16_ASM_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o ${IRIS16_ASM_BINARY} lex.yy.o asm.tab.o ${IRIS16_OUT}
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
