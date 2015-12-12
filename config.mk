LIBS = -lc -lm

CC = cc 
CXX = c++
LEX = flex
YACC = bison
CFLAGS = -g3 -ansi -std=c99 -Wall -Iinclude/
CXXFLAGS = -std=c++11
LDFLAGS = ${LIBS}
PREFIX = /usr/local
