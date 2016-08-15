LIBS = -lc -lm

CC := asminst -a x86_64_gcc --parse-only -- cc
CXX := asminst -a x86_64_gcc --parse-only -- c++
LEX ?= flex
YACC ?= bison
GENFLAGS = -Wall -g3
CFLAGS = -ansi -std=c99 ${GENFLAGS}
CXXFLAGS = -std=c++11 ${GENFLAGS}
LDFLAGS = ${LIBS}
PREFIX = /usr/local
