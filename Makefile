# syn - a series of virtual cpus and other such things as I learn more about
# processor simulation
# See LICENSE file for copyright and license details.

include config.mk

MAYA_OBJECTS = $(patsubst %.c,%.o, $(wildcard *.c))
COMMON_THINGS = ClipsExtensions.o \
				MemoryBlock.o \
				boost.o \
				functional.o \
				AlsaMIDIExtensions.o 

REPL_FINAL_BINARY = syn

REPL_FINAL_OBJECTS = Repl.o \
					 ${COMMON_THINGS} \
					 ${ARCH_OBJECTS} \
					 ${MAYA_OBJECTS}

ALL_BINARIES = ${REPL_FINAL_BINARY}

ALL_OBJECTS = ${COMMON_THINGS} \
			  ${REPL_OBJECTS} \
			  ${REPL_FINAL_OBJECTS} 

TEST_SUITES = test_maya.clp \
			  test_ClipsExtensions.clp


all: options ${ALL_BINARIES}

full: all tests

docs: ${ALL_BINARIES}
	@echo "running doxygen"
	@doxygen

options:
	@echo syn build options:
	@echo "CFLAGS   = ${CFLAGS}"
	@echo "CXXFLAGS = ${CXXFLAGS}"
	@echo "LDFLAGS  = ${LDFLAGS}"
	@echo "CC       = ${CC}"
	@echo "CXX      = ${CXX}"


%.o: %.c
	@echo CC $<
	@${CC} ${CFLAGS} -c $< -o $@

%.o: %.cc
	@echo CXX $<
	@${CXX} ${CXXFLAGS} -c $< -o $@

${REPL_FINAL_BINARY}: ${REPL_FINAL_OBJECTS}
	@echo Building ${REPL_FINAL_BINARY}
	@${CXX} ${LDFLAGS} -o ${REPL_FINAL_BINARY} ${REPL_FINAL_OBJECTS}

clean:
	@echo Cleaning...
	@rm -f ${ALL_OBJECTS} ${ALL_BINARIES}

nuke: clean
	@rm -rf doc/html

tests: ${ALL_BINARIES} ${TEST_SUITES}
	@echo "Running tests..."
	@for n in ${TEST_SUITES}; do \
		./${REPL_FINAL_BINARY} -f2 $$n -f2 cmd/test-case-invoke.clp ; \
	done



.PHONY: all options clean install uninstall docs tests

include deps.make
