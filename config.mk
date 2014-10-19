LIBS = -lc -lm

CC = cc 
LEX = flex
YACC = bison
CFLAGS = -g3 -ansi -std=c99 -Wall -Iinclude/
LDFLAGS = ${LIBS} 
PREFIX = /usr/local
