LIBS = -lc 

CC = cc 
CFLAGS = -g3 -ansi -std=c99 -Wall -I.
LDFLAGS =  ${LIBS}

BINARY = iris

# The object file that defines main()
MAIN = cmd/sim.o
