########### MAKEFILE FOR MAYA ###########
include config.mk
OBJECTS = $(patsubst %.c,%.o, $(wildcard *.c))
CFLAGS += -DBANNER_STRING=${BANNER_STRING} -DCOMMAND_PROMPT='$(COMMAND_PROMPT)'
CXXFLAGS += -DBANNER_STRING=${BANNER_STRING} -DCOMMAND_PROMPT='${COMMAND_PROMPT}'
ifeq ($(CXXEXTENSIONS), TRUE)
CXX_OBJECTS = $(patsubst %.cc,%.o, $(wildcard *.cc))
endif
ifeq ($(CXXEXTENSIONS), TRUE)
	OBJS = ${OBJECTS} ${CXX_OBJECTS}
else
	OBJS = ${OBJECTS}
endif

.PHONY: clean all

all: archive repl

archive: $(OBJS)
	@echo Bulding maya archive
	@$(AR) rcs libmaya.a $(OBJS)

repl: $(OBJS) cmd/repl/main.o
	@echo Building maya
	@$(LD) $(LDFLAGS) -o maya $(OBJS) cmd/repl/main.o

install:
	@echo Installing binaries to $(PREFIX)/bin
	@mkdir -p $(PREFIX)/bin
	@cp maya $(PREFIX)/bin

deinstall uninstall:
	@echo Uninstalling...
	@rm -f $(PREFIX)/bin/$(OUTPUT)


clean:
	@echo Cleaning
	@rm -f $(OBJS) cmd/repl/main.o maya libmaya.a


.c.o :
	@echo CC $<
	@$(CC) -c $(CFLAGS) -o $@ -D_POSIX_C_SOURCE=200112L \
		-std=c99 -Wall -Wundef -Wpointer-arith -Wshadow -Wcast-qual \
	    -Wcast-align -Winline -Wmissing-declarations -Wredundant-decls \
	    -Wmissing-prototypes -Wnested-externs -Wstrict-prototypes \
	    -Waggregate-return -Wno-implicit -I. $<

.cc.o :
	@echo CXX $<
	@$(CXX) -c $(CXXFLAGS) -o $@ -D_POSIX_C_SOURCE=200112L \
		-std=c++11 -Wall -Wundef -Wpointer-arith -Wcast-qual \
		-Wcast-align -Winline -Wmissing-declarations -Wredundant-decls \
		-Waggregate-return -I. $<

