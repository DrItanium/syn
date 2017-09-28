# syn - a series of virtual cpus and other such things as I learn more about
# processor simulation
# See LICENSE file for copyright and license details.

include config.mk

ASM_PARSERS_OBJECTS = AssemblerBase.o

COMMON_THINGS = Core.o \
				WrappedIODevice.o \
				IOController.o \
				ClipsExtensions.o \
			 	MultifieldBuilder.o \
				MemoryBlock.o \
				ExecutionUnits.o

REPL_FINAL_BINARY = syn

REPL_FINAL_OBJECTS = Repl.o \
					 RegisteredExternalAddressAssemblers.o \
					 ${COMMON_THINGS} \
					 ${ARCH_OBJECTS} \
					 ${ASM_PARSERS_OBJECTS} \

ALL_BINARIES = ${REPL_FINAL_BINARY}

ALL_OBJECTS = ${COMMON_THINGS} \
			  ${REPL_OBJECTS} \
			  ${DEFINE_CLPS} \
			  ${REPL_FINAL_OBJECTS}

COMMON_CLP_FILES = reset-run-exit.clp
COMMON_GEN_ENCODER_DECODER_FILES= ${COMMON_CLP_FILES} \
								  cmd/deffield.clp \
								  cmd/deffunctions.clp \
								  cortex.clp \
								  Base.h

TEST_SUITES = target/test_maya.clp \
			  target/test_ClipsExtensions.clp


all: options bootstrap ${ALL_BINARIES}

docs: bootstrap ${ALL_BINARIES}
	@echo "running doxygen"
	@doxygen

libmaya.a: 
	@echo "Building libmaya..."
	@cd misc/maya && $(MAKE)
	@echo "Finished building maya"
	@echo "Copying libmaya.a to root..."
	@cp misc/maya/libmaya.a .

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

${REPL_FINAL_BINARY}: ${REPL_FINAL_OBJECTS} libmaya.a libtermbox.a
	@echo Building ${REPL_FINAL_BINARY}
	@${CXX} ${LDFLAGS} -o ${REPL_FINAL_BINARY} ${REPL_FINAL_OBJECTS} libmaya.a libtermbox.a

clean:
	@echo Cleaning...
	@rm -f ${ALL_OBJECTS} ${ALL_BINARIES}

nuke: clean
	@echo "Cleaning maya..."
	@cd misc/maya && $(MAKE) clean
	@rm -rf doc/html libmaya.a
	@cd misc/termbox && ./waf uninstall --targets=termbox_static
	@cd misc/termbox && ./waf distclean

tests: bootstrap ${ALL_BINARIES} ${TEST_SUITES}
	@echo "Running tests..."
	@for n in ${TEST_SUITES}; do \
		./syn_repl -f2 $$n -f2 cmd/test-case-invoke.clp ; \
	done



.PHONY: all options clean install uninstall docs tests bootstrap 

bootstrap: ${DEFINE_OBJECTS} 

misc/termbox:
	@git submodule update --init --recursive

include/termbox.h libtermbox.a: misc/termbox
	@cd misc/termbox && ./waf configure --prefix=../../ --libdir=../../
	@cd misc/termbox && ./waf
	@cd misc/termbox && ./waf install --targets=termbox_static

include deps.make
