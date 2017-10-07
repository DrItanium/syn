LIBS = -lc -lm -lboost_system -lboost_filesystem -lpthread -lrt

CC := cc
CXX := c++
GENFLAGS = -Wall -g3 -Imisc/maya/ -Imisc/PEGTL/include -O2
CFLAGS = -ansi -std=c99 ${GENFLAGS}
CXXFLAGS = -std=c++14 ${GENFLAGS}
LDFLAGS = ${LIBS} -flto
PREFIX = /usr/local
