LIBS = -lc -lm -lboost_system -lboost_filesystem -lpthread

CC := cc
CXX := c++
LEX ?= flex
YACC ?= bison
GENFLAGS = -Wall -g3 -Imisc/maya/ -Imisc/PEGTL/
CFLAGS = -ansi -std=c99 ${GENFLAGS}
CXXFLAGS = -std=c++14 ${GENFLAGS}
LDFLAGS = ${LIBS}
PREFIX = /usr/local
