LIBS = -lc 

CC = cc 
LEX = flex
YACC = bison
CFLAGS = -g3 -ansi -std=c99 -Wall -I.
LDFLAGS =  ${LIBS}

BINARY = iris
RL_BINARY = irislink
DECODE_BINARY = irisdecode
ASM_BINARY = irisasm

# The object file that defines main()
MAIN = cmd/sim.o
RL_MAIN = cmd/img.o
DECODE_MAIN = cmd/decode.o
