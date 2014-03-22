# iris - simple 8-bit CPU core
# See LICENSE file for copyright and license details.

include config.mk

SRC = iris.c sim.c test.c
OBJ = ${SRC:.c=.o}

all: options iris test

options:
	@echo iris build options:
	@echo "CFLAGS   = ${CFLAGS}"
	@echo "LDFLAGS  = ${LDFLAGS}"
	@echo "CC       = ${CC}"

.c.o:
	@echo CC -c $<
	@${CC} -c $< ${CFLAGS}

${OBJ}: config.mk 

iris: iris.o sim.o
	@echo CC -o $@
	@${CC} -o $@ iris.o sim.o ${LDFLAGS}

test: iris.o test.o
	@echo CC -o $@
	@${CC} -o $@ test.o iris.o ${LDFLAGS}

clean:
	@echo cleaning
	@rm -f iris test ${OBJ} 


.PHONY: all options clean
