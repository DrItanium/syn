include ../../config.mk

ALL_FILES = lex.yy.c asm.tab.c asm.tab.h
all: default

asm.tab.c asm.tab.h: asm.y
	@${YACC} -d asm.y

lex.yy.c: asm.l asm.tab.h
	@${LEX} -l asm.l

default: lex.yy.c asm.tab.c asm.tab.h 
	@echo -n Building ${ASM_BINARY} binary out of $^...
	@${CC} lex.yy.c asm.tab.c ../../util.c -I../../ ${LDFLAGS} -o ${ASM_BINARY}
	@echo done.

clean:
	@rm -f ${ALL_FILES} ${ASM_BINARY} *.o

.PHONY: all clean
