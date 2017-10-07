LIBS = -lc -lm -lboost_system -lboost_filesystem -lpthread -lrt

CC := clang
CXX := clang++
GENFLAGS = -Wall -g3 -Imisc/maya/ -Imisc/PEGTL/include
CFLAGS = -ansi -std=c99 ${GENFLAGS}
CXXFLAGS = -std=c++14 ${GENFLAGS}
LDFLAGS = ${LIBS}
PREFIX = /usr/local
