# syn - a series of virtual cpus and other such things as I learn more about
# processor simulation
# See LICENSE file for copyright and license details.

include config.mk
ARCH_OBJECTS = IrisCore.o \
			   Cisc0Core.o \
			   Cisc0CoreModel0.o \
			   Cisc0CoreModel1.o

ASM_PARSERS_OBJECTS = Cisc0CoreAssembler.o \
					  IrisCoreAssemblerStructures.o \
					  AssemblerBase.o

COMMON_THINGS = Core.o \
				WrappedIODevice.o \
				IOController.o \
				ClipsExtensions.o \
			 	MultifieldBuilder.o \
				MemoryBlock.o \
				libmaya.a

REPL_FINAL_BINARY = syn_repl

REPL_FINAL_OBJECTS = Repl.o \
					 RegisteredExternalAddressAssemblers.o \
					 Cisc0CoreAssemblerWrapper.o \
					 Cisc0CoreInstructionEncoder.o \
					 IrisCoreAssemblerStateWrapper.o \
					 Cisc0CoreWrapper.o \
					 IrisCoreWrapper.o \
					 Cisc0CoreDecodedInstruction.o \
					 ${COMMON_THINGS} \
					 ${ARCH_OBJECTS} \
					 ${ASM_PARSERS_OBJECTS} \

ALL_BINARIES = ${REPL_FINAL_BINARY}

DEFINE_OBJECTS = defines_iris.h \
				 defines_cisc0.h

ALL_OBJECTS = ${COMMON_THINGS} \
			  ${ARCH_OBJECTS} \
			  ${REPL_OBJECTS} \
			  ${DEFINE_CLPS} \
			  ${REPL_FINAL_OBJECTS}

COMMON_CLP_FILES = lib/reset-run-exit.clp
COMMON_GEN_ENCODER_DECODER_FILES= ${COMMON_CLP_FILES} \
								  cmd/deffield.clp \
								  cmd/deffunctions.clp \
								  lib/cortex.clp \
								  Base.h

TEST_SUITES = lib/target/iris/test_Base.clp \
			  lib/target/iris/test_Exec.clp \
			  lib/target/cisc0/test_Base.clp \
			  lib/target/test_maya.clp \
			  lib/target/test_ClipsExtensions.clp


all: options bootstrap ${ALL_BINARIES}

docs: bootstrap ${ALL_BINARIES}
	@echo "running doxygen"
	@doxygen

maya:
	@echo "Building maya..."
	@cd misc/maya && $(MAKE)
	@echo "Finished building maya"
	@echo "Copying maya to root..."
	@cp misc/maya/maya .
	@cp misc/maya/libmaya.a .

libmaya.a: maya

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
	@rm -f ${ALL_OBJECTS} ${ALL_BINARIES} maya libmaya.a

nuke: clean
	@echo "Cleaning maya..."
	@cd misc/maya && $(MAKE) clean
	@rm -rf doc/html

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

tests: bootstrap ${ALL_BINARIES} ${TEST_SUITES}
	@echo "Running tests..."
	@for n in ${TEST_SUITES}; do \
		./syn_repl -f2 $$n -f2 cmd/test-case-invoke.clp ; \
	done



.PHONY: all options clean install uninstall docs tests

bootstrap: ${DEFINE_OBJECTS}

define generateFields
	./deffield.sh -f2 $(1) -f2 lib/reset-run-exit.clp > $(2).h
endef

define generateDefines
	echo "Generating encoders, decoders, and enumerations for $(1)..."
	$(call generateFields,def/$(1)/instruction.clp,defines_$(1))
endef

define generateDefinesRule

defines_$(1).h: maya ${COMMON_GEN_ENCODER_DECODER_FILES} def/$(1)/instruction.clp
	@$(call generateDefines,$(1))

endef

$(foreach i,iris cisc0,$(eval $(call generateDefinesRule,$(i))))

include deps.make
