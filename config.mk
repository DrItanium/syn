LIBS = -lc -lm

CC := cc
CXX := c++
LEX ?= flex
YACC ?= bison
GENFLAGS = -O2 -Wall -g3
CFLAGS = -ansi -std=c99 ${GENFLAGS}
CXXFLAGS = -std=c++14 ${GENFLAGS}
LDFLAGS = ${LIBS}
PREFIX = /usr/local
