# syn - a series of virtual cpus and other such things as I learn more about
# processor simulation
# See LICENSE file for copyright and license details.

include config.mk
MACHINE_OBJECTS =
ARCH_OBJECTS = iris.o \
			   cisc0.o \
			   molecule.o \
			   ${MACHINE_OBJECTS}

COMMON_THINGS = libmaya.a \
				Core.o

BOOTSTRAP_OBJECTS = molecule_bootstrap.o \
					${ARCH_OBJECTS} \
					${COMMON_THINGS}

BOOTSTRAP_BINARY = molecule_bootstrap

SIM_OBJECTS = syn_sim.o \
			  CoreRegistrar.o \
			  syn_cores.o \
			  ${ARCH_OBJECTS} \
			  ${COMMON_THINGS}

SIM_BINARY = syn_sim

ASM_PARSERS_OBJECTS = iris_lex.yy.o \
					  iris_asm.tab.o \
					  cisc0_lex.yy.o \
					  cisc0_asm.tab.o \

ASM_OBJECTS = syn_asm.o \
			  AssemblerRegistrar.o \
			  syn_assemblers.o \
			  ${COMMON_THINGS} \
				${ARCH_OBJECTS} \
				CoreRegistrar.o \
				syn_cores.o \
			  ${ASM_PARSERS_OBJECTS}

REPL_BINARY = syn_repl

REPL_OBJECTS= syn_repl.o \
			  syn_clips.o \
			  ${COMMON_THINGS}

ASM_PARSERS = iris_lex.yy.c \
			  iris_asm.tab.c \
			  iris_asm.tab.h \
			  cisc0_lex.yy.c \
			  cisc0_asm.tab.c \
			  cisc0_asm.tab.h \

ASM_BINARY = syn_asm

LINK_OBJECTS = syn_link.o \
				${ARCH_OBJECTS} \
				CoreRegistrar.o \
				syn_cores.o \
			  ${COMMON_THINGS}

LINK_BINARY = syn_link

IRIS_ASSEMBLER_OBJECTS = iris_assembler.o \
						 iris.o \
						 Core.o

IRIS_ASSEMBLER_BINARY = iris_assembler

ALL_BINARIES = ${SIM_BINARY} \
			   ${ASM_BINARY} \
			   ${LINK_BINARY} \
			   ${REPL_BINARY} \
			   ${BOOTSTRAP_BINARY} \
			   ${IRIS_ASSEMBLER_BINARY}

DEFINE_OBJECTS = iris_defines.h \
				 cisc0_defines.h \
				 molecule_defines.h \
				 syn_memory_block_defines.h

DEFINE_CLPS = iris_defines.clp \
			  cisc0_defines.clp \
			  molecule_defines.clp

ALL_OBJECTS = ${COMMON_THINGS} \
			  ${SIM_OBJECTS} \
			  ${ASM_OBJECTS} \
			  ${LINK_OBJECTS} \
			  ${ARCH_OBJECTS} \
			  ${REPL_OBJECTS} \
			  ${DEFINE_OBJECTS} \
			  ${BOOTSTRAP_OBJECTS} \
			  ${DEFINE_CLPS} \
			  ${IRIS_ASSEMBLER_OBJECTS} \



COMMON_CLP_FILES = lib/reset-run-exit.clp
COMMON_GEN_ENCODER_DECODER_FILES= ${COMMON_CLP_FILES} \
								  cmd/deffield.clp \
								  cmd/deffunctions.clp \
								  lib/cortex.clp \
								  syn_base.h

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

%_lex.yy.c: %_asm.l %_asm.tab.h
	@echo -n Constructing Lexer from $< ...
	@${LEX} -o $*_lex.yy.c $*_asm.l
	@echo done

%.yy.o: %.yy.c
	@echo -n Compiling $< into $@ ...
	@${CXX} ${CXXFLAGS} -D_POSIX_SOURCE -c $< -o $@
	@echo done

${SIM_BINARY}: ${SIM_OBJECTS}
	@echo -n Building ${SIM_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o ${SIM_BINARY} $^
	@echo done.

${LINK_BINARY}: ${LINK_OBJECTS}
	@echo -n Building ${LINK_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o ${LINK_BINARY} $^
	@echo done.

${ASM_BINARY}: ${ASM_OBJECTS}
	@echo -n Building ${ASM_BINARY} binary out of $^...
	@${CXX} ${LDFLAGS} -o ${ASM_BINARY} ${ASM_OBJECTS}
	@echo done.

${REPL_BINARY}: ${REPL_OBJECTS}
	@echo -n Building ${REPL_BINARY} binary out of $^...
	@${CXX} ${LIBS} -o ${REPL_BINARY} ${REPL_OBJECTS}
	@echo done.

${BOOTSTRAP_BINARY}: ${BOOTSTRAP_OBJECTS}
	@echo -n Building ${BOOTSTRAP_BINARY} binary out of $^...
	@${CXX} ${LIBS} -o ${BOOTSTRAP_BINARY} ${BOOTSTRAP_OBJECTS}
	@echo done.

${IRIS_ASSEMBLER_BINARY}: ${IRIS_ASSEMBLER_OBJECTS}
	@echo -n Building ${IRIS_ASSEMBLER_BINARY} binary out of $^...
	@${CXX} ${LIBS} -o ${IRIS_ASSEMBLER_BINARY} ${IRIS_ASSEMBLER_OBJECTS}
	@echo done.

clean:
	@echo -n Cleaning...
	@rm -f ${ALL_OBJECTS} ${ALL_BINARIES} ${ASM_PARSERS} scheme init.scm maya libmaya.a
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
# generate the syn_memory_block_defines.h prior to generating syn_clips.h
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

molecule_defines.h: ${REPL_BINARY} ${COMMON_GEN_ENCODER_DECODER_FILES} def/molecule/instruction.clp
	@echo "Generating encoders, decoders and enumerations for molecule..."
	@./deffield.sh -f2 def/molecule/instruction.clp -f2 lib/reset-run-exit.clp > molecule_defines.h
	@./deffunction.sh -f2 def/molecule/instruction.clp -f2 lib/reset-run-exit.clp > molecule_defines.clp

phoenix_defines.h: ${REPL_BINARY} ${COMMON_GEN_ENCODER_DECODER_FILES} def/phoenix/instruction.clp
	@echo "Generating encoders, decoders and enumerations for phoenix..."
	@./deffield.sh -f2 def/phoenix/instruction.clp -f2 lib/reset-run-exit.clp > phoenix_defines.h
	@./deffunction.sh -f2 def/phoenix/instruction.clp -f2 lib/reset-run-exit.clp > phoenix_defines.clp

cisc0_asm.tab.o: cisc0_asm.tab.c cisc0.h syn_base.h Problem.h \
 syn_xunits.h IODevice.h Device.h Core.h cisc0_defines.h \
 def/cisc0/instruction.def def/cisc0/misc.def def/cisc0/ops.def \
 AssemblerRegistrar.h cisc0_asm.tab.h
cisc0_lex.yy.o: cisc0_lex.yy.c cisc0.h syn_base.h Problem.h syn_xunits.h \
 IODevice.h Device.h Core.h cisc0_defines.h def/cisc0/instruction.def \
 def/cisc0/misc.def def/cisc0/ops.def cisc0_asm.tab.h syn_asm_base.h
iris_asm.tab.o: iris_asm.tab.c AssemblerRegistrar.h iris.h syn_base.h \
 Problem.h syn_xunits.h IODevice.h Device.h Core.h IOController.h \
 iris_defines.h iris_asm.tab.h
iris_lex.yy.o: iris_lex.yy.c iris.h syn_base.h Problem.h syn_xunits.h \
 IODevice.h Device.h Core.h IOController.h iris_defines.h iris_asm.tab.h \
 syn_asm_base.h


AssemblerRegistrar.o: AssemblerRegistrar.cc AssemblerRegistrar.h iris.h \
 syn_base.h Problem.h syn_xunits.h IODevice.h Device.h Core.h \
 IOController.h iris_defines.h cisc0.h cisc0_defines.h \
 def/cisc0/instruction.def def/cisc0/misc.def def/cisc0/ops.def \
 molecule.h molecule_defines.h
cisc0.o: cisc0.cc cisc0.h syn_base.h Problem.h syn_xunits.h IODevice.h \
 Device.h Core.h cisc0_defines.h def/cisc0/instruction.def \
 def/cisc0/misc.def def/cisc0/ops.def def/cisc0/bitmask4bit.def
Core.o: Core.cc Core.h Device.h
CoreRegistrar.o: CoreRegistrar.cc Problem.h CoreRegistrar.h Core.h \
 Device.h
iris.o: iris.cc iris.h syn_base.h Problem.h syn_xunits.h IODevice.h \
 Device.h Core.h IOController.h iris_defines.h
molecule_bootstrap.o: molecule_bootstrap.cc molecule.h syn_base.h \
 Problem.h syn_xunits.h IODevice.h Device.h Core.h IOController.h \
 molecule_defines.h
molecule.o: molecule.cc molecule.h syn_base.h Problem.h syn_xunits.h \
 IODevice.h Device.h Core.h IOController.h molecule_defines.h
phoenix.o: phoenix.cc molecule.h syn_base.h Problem.h syn_xunits.h \
 IODevice.h Device.h Core.h IOController.h molecule_defines.h
syn_asm.o: syn_asm.cc Problem.h AssemblerRegistrar.h
syn_assemblers.o: syn_assemblers.cc Problem.h RegisterEntry.h \
 AssemblerRegistrar.h iris.h syn_base.h syn_xunits.h IODevice.h Device.h \
 Core.h IOController.h iris_defines.h cisc0.h cisc0_defines.h \
 def/cisc0/instruction.def def/cisc0/misc.def def/cisc0/ops.def \
 molecule.h molecule_defines.h
syn_clips.o: syn_clips.cc syn_clips.h syn_base.h Problem.h \
 misc/maya/clips.h misc/maya/setup.h misc/maya/os_shim.h \
 misc/maya/platform.h misc/maya/envrnmnt.h misc/maya/symbol.h \
 misc/maya/usrsetup.h misc/maya/argacces.h misc/maya/expressn.h \
 misc/maya/exprnops.h misc/maya/exprnpsr.h misc/maya/extnfunc.h \
 misc/maya/evaluatn.h misc/maya/constant.h misc/maya/userdata.h \
 misc/maya/factmngr.h misc/maya/conscomp.h misc/maya/constrct.h \
 misc/maya/moduldef.h misc/maya/modulpsr.h misc/maya/utility.h \
 misc/maya/scanner.h misc/maya/pprint.h misc/maya/symblcmp.h \
 misc/maya/facthsh.h misc/maya/multifld.h misc/maya/pattern.h \
 misc/maya/match.h misc/maya/network.h misc/maya/ruledef.h \
 misc/maya/agenda.h misc/maya/constrnt.h misc/maya/cstrccom.h \
 misc/maya/reorder.h misc/maya/tmpltdef.h misc/maya/factbld.h \
 misc/maya/object.h misc/maya/memalloc.h misc/maya/cstrcpsr.h \
 misc/maya/filecom.h misc/maya/strngfun.h misc/maya/commline.h \
 misc/maya/router.h misc/maya/prntutil.h misc/maya/filertr.h \
 misc/maya/strngrtr.h misc/maya/iofun.h misc/maya/sysdep.h \
 misc/maya/bmathfun.h misc/maya/watch.h misc/maya/modulbsc.h \
 misc/maya/bload.h misc/maya/exprnbin.h misc/maya/symblbin.h \
 misc/maya/bsave.h misc/maya/rulebsc.h misc/maya/engine.h \
 misc/maya/lgcldpnd.h misc/maya/retract.h misc/maya/drive.h \
 misc/maya/incrrset.h misc/maya/rulecom.h misc/maya/crstrtgy.h \
 misc/maya/dffctdef.h misc/maya/dffctbsc.h misc/maya/tmpltbsc.h \
 misc/maya/tmpltfun.h misc/maya/factcom.h misc/maya/factfun.h \
 misc/maya/globldef.h misc/maya/globlbsc.h misc/maya/globlcom.h \
 misc/maya/dffnxfun.h misc/maya/genrccom.h misc/maya/genrcfun.h \
 misc/maya/classcom.h misc/maya/classexm.h misc/maya/classinf.h \
 misc/maya/classini.h misc/maya/classpsr.h misc/maya/defins.h \
 misc/maya/inscom.h misc/maya/insfun.h misc/maya/insfile.h \
 misc/maya/msgcom.h misc/maya/msgpass.h misc/maya/objrtmch.h \
 syn_memory_block_defines.h
syn_cores.o: syn_cores.cc Problem.h RegisterEntry.h CoreRegistrar.h \
 iris.h syn_base.h syn_xunits.h IODevice.h Device.h Core.h IOController.h \
 iris_defines.h cisc0.h cisc0_defines.h def/cisc0/instruction.def \
 def/cisc0/misc.def def/cisc0/ops.def molecule.h molecule_defines.h \
 syn_machines.h LockStepMachine.h
syn_link.o: syn_link.cc Core.h Device.h CoreRegistrar.h Problem.h
syn_repl.o: syn_repl.cc misc/maya/clips.h misc/maya/setup.h \
 misc/maya/os_shim.h misc/maya/platform.h misc/maya/envrnmnt.h \
 misc/maya/symbol.h misc/maya/usrsetup.h misc/maya/argacces.h \
 misc/maya/expressn.h misc/maya/exprnops.h misc/maya/exprnpsr.h \
 misc/maya/extnfunc.h misc/maya/evaluatn.h misc/maya/constant.h \
 misc/maya/userdata.h misc/maya/factmngr.h misc/maya/conscomp.h \
 misc/maya/constrct.h misc/maya/moduldef.h misc/maya/modulpsr.h \
 misc/maya/utility.h misc/maya/scanner.h misc/maya/pprint.h \
 misc/maya/symblcmp.h misc/maya/facthsh.h misc/maya/multifld.h \
 misc/maya/pattern.h misc/maya/match.h misc/maya/network.h \
 misc/maya/ruledef.h misc/maya/agenda.h misc/maya/constrnt.h \
 misc/maya/cstrccom.h misc/maya/reorder.h misc/maya/tmpltdef.h \
 misc/maya/factbld.h misc/maya/object.h misc/maya/memalloc.h \
 misc/maya/cstrcpsr.h misc/maya/filecom.h misc/maya/strngfun.h \
 misc/maya/commline.h misc/maya/router.h misc/maya/prntutil.h \
 misc/maya/filertr.h misc/maya/strngrtr.h misc/maya/iofun.h \
 misc/maya/sysdep.h misc/maya/bmathfun.h misc/maya/watch.h \
 misc/maya/modulbsc.h misc/maya/bload.h misc/maya/exprnbin.h \
 misc/maya/symblbin.h misc/maya/bsave.h misc/maya/rulebsc.h \
 misc/maya/engine.h misc/maya/lgcldpnd.h misc/maya/retract.h \
 misc/maya/drive.h misc/maya/incrrset.h misc/maya/rulecom.h \
 misc/maya/crstrtgy.h misc/maya/dffctdef.h misc/maya/dffctbsc.h \
 misc/maya/tmpltbsc.h misc/maya/tmpltfun.h misc/maya/factcom.h \
 misc/maya/factfun.h misc/maya/globldef.h misc/maya/globlbsc.h \
 misc/maya/globlcom.h misc/maya/dffnxfun.h misc/maya/genrccom.h \
 misc/maya/genrcfun.h misc/maya/classcom.h misc/maya/classexm.h \
 misc/maya/classinf.h misc/maya/classini.h misc/maya/classpsr.h \
 misc/maya/defins.h misc/maya/inscom.h misc/maya/insfun.h \
 misc/maya/insfile.h misc/maya/msgcom.h misc/maya/msgpass.h \
 misc/maya/objrtmch.h syn_clips.h syn_base.h Problem.h
syn_sim.o: syn_sim.cc Problem.h Core.h Device.h CoreRegistrar.h
