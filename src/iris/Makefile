# iris - simple 8-bit CPU core
# See LICENSE file for copyright and license details.

include config.mk

SRCS = $(wildcard *.c)
OBJS = ${SRCS:.c=.o}

all: options iris

options:
	@echo iris build options:
	@echo "CFLAGS   = ${CFLAGS}"
	@echo "LDFLAGS  = ${LDFLAGS}"
	@echo "CC       = ${CC}"

.c.o:
	@echo CC -c ${SRCS}
	@${CC} -c ${SRCS} ${CFLAGS}

${OBJ}: config.mk 

iris: ${OBJS}
	@echo CC ${OBJS} -o iris
	@${CC} ${OBJS} -o iris ${LDFLAGS}

clean:
	@echo cleaning
	@rm -f iris stest ${OBJS} iris-${VERSION}.tar.gz


.PHONY: all options clean
