LIBS = -lc -lm -lboost_system -lboost_filesystem -lsfml-system

CC := cc
CXX := c++
LEX ?= flex
YACC ?= bison
GENFLAGS = -Wall -g3 -Imisc/maya/
CFLAGS = -ansi -std=c99 ${GENFLAGS}
CXXFLAGS = -std=c++14 ${GENFLAGS}
LDFLAGS = ${LIBS}
PREFIX = /usr/local
