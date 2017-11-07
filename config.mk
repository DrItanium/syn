LIBS = -lc -lm -lboost_system -lboost_filesystem -lrt -lasound

CC := cc
CXX := c++
GENFLAGS = -Wall -g3 -DENABLE_EXTENDED_MEMORY_BLOCKS=1
CFLAGS = -ansi -std=c99 ${GENFLAGS}
CXXFLAGS = -std=c++14 ${GENFLAGS}
LDFLAGS = ${LIBS}
PREFIX = /usr/local
