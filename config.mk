LIBS = -lc -lm

CC = cc 
CXX = c++
LEX = flex
YACC = bison
GENFLAGS = -Wall -g3
CFLAGS = -ansi -std=c99 ${GENFLAGS}
CXXFLAGS = -std=c++11 ${GENFLAGS}
LDFLAGS = ${LIBS}
PREFIX = /usr/local
