# iris - simple 8-bit CPU core
# See LICENSE file for copyright and license details.

include config.mk

SRC = iris.c sim.c testdecode.c
OBJ = ${SRC:.c=.o}

all: options iris testdecode

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

testdecode: iris.o testdecode.o
	@echo CC -o $@
	@${CC} -o $@ iris.o testdecode.o ${LDFLAGS}

clean:
	@echo cleaning
	@rm -f iris testdecode ${OBJ} 


.PHONY: all options clean
