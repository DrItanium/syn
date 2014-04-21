LIBS = -lc 

CC = cc 
CFLAGS = -g3 -ansi -std=c99 -Wall -I.
LDFLAGS =  ${LIBS}

BINARY = iris
RL_BINARY = rl

# The object file that defines main()
MAIN = cmd/sim.o

RL_MAIN = cmd/img.o
