# syn - a series of virtual cpus and other such things as I learn more about
# processor simulation
# See LICENSE file for copyright and license details.

include config.mk
ARCH_OBJECTS = IrisCore.o \
			   Cisc0Core.o

ASM_PARSERS_OBJECTS = Cisc0CoreAssembler.o \
					  IrisCoreAssembler.o \
					  AssemblerBase.o

COMMON_THINGS = Core.o \
				WrappedIODevice.o \
				IOController.o \
				ClipsExtensions.o \
				libmaya.a

ASM_OBJECTS = Assembler.o \
			  AssemblerRegistrar.o \
			  RegisteredAssemblers.o \
			  Cisc0CoreAssemblerAssembleOperation.o \
			  Cisc0CoreInstructionEncoder.o \
			  IrisCoreAssemblerAssembleOperation.o \
			  Core.o \
			  ${ASM_PARSERS_OBJECTS}

REPL_BINARY = xsyn_repl

REPL_OBJECTS= ReplBootstrap.o \
			  ${COMMON_THINGS}

REPL_FINAL_BINARY = syn_repl

REPL_FINAL_OBJECTS = Repl.o \
					 RegisteredExternalAddressAssemblers.o \
					 Cisc0CoreAssemblerWrapper.o \
					 Cisc0CoreInstructionEncoder.o \
					 IrisCoreAssemblerStateWrapper.o \
					 ClipsExtensions.o \
					 libmaya.a \
					 ${ASM_PARSERS_OBJECTS} \

ASM_BINARY = syn_asm

ALL_BINARIES = ${ASM_BINARY} \
			   ${REPL_BINARY} \
			   ${BOOTSTRAP_BINARY} \
			   ${REPL_FINAL_BINARY}

DEFINE_OBJECTS = iris_defines.h \
				 cisc0_defines.h \
				 syn_memory_block_defines.h

DEFINE_CLPS = iris_defines.clp \
			  cisc0_defines.clp

ALL_OBJECTS = ${COMMON_THINGS} \
			  ${ASM_OBJECTS} \
			  ${ARCH_OBJECTS} \
			  ${REPL_OBJECTS} \
			  ${DEFINE_OBJECTS} \
			  ${BOOTSTRAP_OBJECTS} \
			  ${DEFINE_CLPS} \
			  ${REPL_FINAL_OBJECTS}



COMMON_CLP_FILES = lib/reset-run-exit.clp
COMMON_GEN_ENCODER_DECODER_FILES= ${COMMON_CLP_FILES} \
								  cmd/deffield.clp \
								  cmd/deffunctions.clp \
								  lib/cortex.clp \
								  Base.h

all: options bootstrap ${ALL_BINARIES}

maya:
	@echo "Buidling maya..."
	@cd misc/maya && $(MAKE)
	@echo "Finished building maya"
	@echo -n "Copying maya to root..."
	@cp misc/maya/maya .
	@cp misc/maya/libmaya.a .
	@echo Done

libmaya.a: maya

options:
	@echo syn build options:
	@echo "CFLAGS   = ${CFLAGS}"
	@echo "CXXFLAGS = ${CXXFLAGS}"
	@echo "LDFLAGS  = ${LDFLAGS}"
	@echo "CC       = ${CC}"
	@echo "CXX      = ${CXX}"


%.o: %.c
	@echo -n Compiling $< into $@...
	@${CC} ${CFLAGS} -c $< -o $@
	@echo done.

%.o: %.cc
	@echo -n Compiling $< into $@...
	@${CC} ${CXXFLAGS} -c $< -o $@
	@echo done.

%.tab.c %.tab.h: %.y
	@echo -n Constructing Parser from $< ...
	@${YACC} -o $*.tab.c -d $<
	@echo done

%.tab.o: %.tab.c
	@echo -n Compiling $< into $@...
	@${CXX} ${CXXFLAGS} -c $< -o $@
	@echo done

%_lex.yy.c: %Assembler.l %Assembler.tab.h
	@echo -n Constructing Lexer from $< ...
	@${LEX} -o $*_lex.yy.c $*Assembler.l
	@echo done

%.yy.o: %.yy.c
	@echo -n Compiling $< into $@ ...
	@${CXX} ${CXXFLAGS} -D_POSIX_SOURCE -c $< -o $@
	@echo done

${ASM_BINARY}: ${ASM_OBJECTS}
	@echo -n Building ${ASM_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o ${ASM_BINARY} ${ASM_OBJECTS}
	@echo done.

${REPL_BINARY}: ${REPL_OBJECTS}
	@echo -n Building ${REPL_BINARY} binary out of $^...
	@${CXX} ${LIBS} -o ${REPL_BINARY} ${REPL_OBJECTS}
	@echo done.

${REPL_FINAL_BINARY}: ${REPL_BINARY} ${REPL_FINAL_OBJECTS}
	@echo -n Building ${REPL_FINAL_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o ${REPL_FINAL_BINARY} ${REPL_FINAL_OBJECTS}
	@echo done.

${BOOTSTRAP_BINARY}: ${BOOTSTRAP_OBJECTS}
	@echo -n Building ${BOOTSTRAP_BINARY} binary out of $^...
	@${CXX} ${LIBS} -o ${BOOTSTRAP_BINARY} ${BOOTSTRAP_OBJECTS}
	@echo done.

clean:
	@echo -n Cleaning...
	@rm -f ${ALL_OBJECTS} ${ALL_BINARIES} maya libmaya.a
	@echo done.
	@echo "Cleaning maya..."
	@cd misc/maya && $(MAKE) clean

install: ${ALL_BINARIES}
	@echo installing executables to ${DESTDIR}${PREFIX}/bin
	@mkdir -p ${DESTDIR}${PREFIX}/bin
	@for n in $(ALL_BINARIES); do \
		cp $$n ${DESTDIR}${PREFIX}/bin/$$n; \
		chmod 755 ${DESTDIR}${PREFIX}/bin/$$n; \
	done

uninstall:
	@echo removing executables from ${DESTDIR}${PREFIX}/bin
	@for n in $(ALL_BINARIES); do \
		rm -f ${DESTDIR}${PREFIX}/bin/$$n ; \
	done

.SECONDARY: ${TEST_OBJECTS}

.PHONY: all options clean install uninstall

bootstrap: ${REPL_BINARY} ${DEFINE_OBJECTS}
# generate the syn_memory_block_defines.h prior to generating ClipsExtensions.h
syn_memory_block_defines.h: maya ${COMMON_CLP_FILES} def/memory-block-ops.clp
	@echo "Generating memory block call operations..."
	@./maya -f2 def/memory-block-ops.clp -f2 lib/reset-run-exit.clp > syn_memory_block_defines.h

iris_defines.h: ${REPL_BINARY} ${COMMON_GEN_ENCODER_DECODER_FILES} def/iris/instruction.clp
	@echo "Generating encoders, decoders and enumerations for iris..."
	@./deffield.sh -f2 def/iris/instruction.clp -f2 lib/reset-run-exit.clp > iris_defines.h
	@./deffunction.sh -f2 def/iris/instruction.clp -f2 lib/reset-run-exit.clp > iris_defines.clp


cisc0_defines.h: ${REPL_BINARY} ${COMMON_GEN_ENCODER_DECODER_FILES} def/cisc0/instruction.clp
	@echo "Generating encoders, decoders and enumerations for cisc0..."
	@./deffield.sh -f2 def/cisc0/instruction.clp -f2 lib/reset-run-exit.clp > cisc0_defines.h
	@./deffunction.sh -f2 def/cisc0/instruction.clp -f2 lib/reset-run-exit.clp > cisc0_defines.clp

include deps.make
