# iris - simple 8-bit CPU core
# See LICENSE file for copyright and license details.

include config.mk

SRC = iris.c
OBJ = ${SRC:.c=.o}

all: options iris

options:
	@echo iris build options:
	@echo "CFLAGS   = ${CFLAGS}"
	@echo "LDFLAGS  = ${LDFLAGS}"
	@echo "CC       = ${CC}"

.c.o:
	@echo CC -c $<
	@${CC} -c $< ${CFLAGS}

${OBJ}: config.mk 

iris: iris.o
	@echo CC -o $@
	@${CC} -o $@ iris.o ${LDFLAGS}

clean:
	@echo cleaning
	@rm -f iris stest ${OBJ} iris-${VERSION}.tar.gz


.PHONY: all options clean
