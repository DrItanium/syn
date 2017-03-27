# syn - a series of virtual cpus and other such things as I learn more about
# processor simulation
# See LICENSE file for copyright and license details.

include config.mk
MACHINE_OBJECTS =
DEVICES = IrisCoreSecondaryStorageController.o
ARCH_OBJECTS = IrisCore.o \
			   Cisc0Core.o \
			   ${DEVICES} \
			   ${MACHINE_OBJECTS}

ASM_PARSERS_OBJECTS = Cisc0CoreAssembler.o \
					  IrisCoreAssembler.o \

COMMON_THINGS = Core.o \
				WrappedIODevice.o \
				IOController.o \
				ClipsExtensions.o \
				libmaya.a


SIM_OBJECTS = Simulator.o \
			  CoreRegistrar.o \
			  RegisteredCores.o \
			  ${ARCH_OBJECTS} \
			  ${ASM_PARSERS_OBJECTS} \
			  ${COMMON_THINGS}

SIM_BINARY = syn_sim


ASM_OBJECTS = Assembler.o \
			  AssemblerRegistrar.o \
			  RegisteredAssemblers.o \
			  ${COMMON_THINGS} \
			  ${ARCH_OBJECTS} \
			  CoreRegistrar.o \
			  RegisteredCores.o \
			  ${ASM_PARSERS_OBJECTS}

REPL_BINARY = xsyn_repl

REPL_OBJECTS= ReplBootstrap.o \
			  ${COMMON_THINGS}

REPL_FINAL_BINARY = syn_repl

REPL_FINAL_OBJECTS = Repl.o \
					 RegisteredExternalAddressAssemblers.o \
					 ${COMMON_THINGS} \
					 ${ARCH_OBJECTS} \
					 ${ASM_PARSERS_OBJECTS} \

ASM_BINARY = syn_asm

LINK_OBJECTS = Linker.o \
				${ARCH_OBJECTS} \
				CoreRegistrar.o \
				RegisteredCores.o \
				${ASM_PARSERS_OBJECTS} \
			  ${COMMON_THINGS}

LINK_BINARY = syn_link

ALL_BINARIES = ${SIM_BINARY} \
			   ${ASM_BINARY} \
			   ${LINK_BINARY} \
			   ${REPL_BINARY} \
			   ${BOOTSTRAP_BINARY} \
			   ${REPL_FINAL_BINARY}

DEFINE_OBJECTS = iris_defines.h \
				 cisc0_defines.h \
				 syn_memory_block_defines.h

DEFINE_CLPS = iris_defines.clp \
			  cisc0_defines.clp

ALL_OBJECTS = ${COMMON_THINGS} \
			  ${SIM_OBJECTS} \
			  ${ASM_OBJECTS} \
			  ${LINK_OBJECTS} \
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


Assembler.o: Assembler.cc Problem.h AssemblerRegistrar.h
AssemblerBase.o: AssemblerBase.cc
AssemblerRegistrar.o: AssemblerRegistrar.cc Problem.h \
 AssemblerRegistrar.h
Cisc0Core.o: Cisc0Core.cc Cisc0Core.h Base.h Problem.h ExecutionUnits.h \
 IODevice.h Device.h Core.h ClipsExtensions.h misc/maya/clips.h \
 misc/maya/setup.h misc/maya/os_shim.h misc/maya/platform.h \
 misc/maya/envrnmnt.h misc/maya/symbol.h misc/maya/usrsetup.h \
 misc/maya/argacces.h misc/maya/expressn.h misc/maya/exprnops.h \
 misc/maya/exprnpsr.h misc/maya/extnfunc.h misc/maya/evaluatn.h \
 misc/maya/constant.h misc/maya/userdata.h misc/maya/factmngr.h \
 misc/maya/conscomp.h misc/maya/constrct.h misc/maya/moduldef.h \
 misc/maya/modulpsr.h misc/maya/utility.h misc/maya/scanner.h \
 misc/maya/pprint.h misc/maya/symblcmp.h misc/maya/facthsh.h \
 misc/maya/multifld.h misc/maya/pattern.h misc/maya/match.h \
 misc/maya/network.h misc/maya/ruledef.h misc/maya/agenda.h \
 misc/maya/constrnt.h misc/maya/cstrccom.h misc/maya/reorder.h \
 misc/maya/tmpltdef.h misc/maya/factbld.h misc/maya/object.h \
 misc/maya/memalloc.h misc/maya/cstrcpsr.h misc/maya/filecom.h \
 misc/maya/strngfun.h misc/maya/commline.h misc/maya/router.h \
 misc/maya/prntutil.h misc/maya/filertr.h misc/maya/strngrtr.h \
 misc/maya/iofun.h misc/maya/sysdep.h misc/maya/bmathfun.h \
 misc/maya/watch.h misc/maya/modulbsc.h misc/maya/bload.h \
 misc/maya/exprnbin.h misc/maya/symblbin.h misc/maya/bsave.h \
 misc/maya/rulebsc.h misc/maya/engine.h misc/maya/lgcldpnd.h \
 misc/maya/retract.h misc/maya/drive.h misc/maya/incrrset.h \
 misc/maya/rulecom.h misc/maya/crstrtgy.h misc/maya/dffctdef.h \
 misc/maya/dffctbsc.h misc/maya/tmpltbsc.h misc/maya/tmpltfun.h \
 misc/maya/factcom.h misc/maya/factfun.h misc/maya/globldef.h \
 misc/maya/globlbsc.h misc/maya/globlcom.h misc/maya/dffnxfun.h \
 misc/maya/genrccom.h misc/maya/genrcfun.h misc/maya/classcom.h \
 misc/maya/classexm.h misc/maya/classinf.h misc/maya/classini.h \
 misc/maya/classpsr.h misc/maya/defins.h misc/maya/inscom.h \
 misc/maya/insfun.h misc/maya/insfile.h misc/maya/msgcom.h \
 misc/maya/msgpass.h misc/maya/objrtmch.h IOController.h \
 WrappedIODevice.h cisc0_defines.h IrisCoreSecondaryStorageController.h \
 IrisCoreTypes.h Cisc0ClipsExtensions.h
Cisc0CoreAssembler.o: Cisc0CoreAssembler.cc Base.h Problem.h \
 AssemblerBase.h Cisc0Core.h ExecutionUnits.h IODevice.h Device.h Core.h \
 ClipsExtensions.h misc/maya/clips.h misc/maya/setup.h \
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
 misc/maya/objrtmch.h IOController.h WrappedIODevice.h cisc0_defines.h \
 Cisc0ClipsExtensions.h
ClipsExtensions.o: ClipsExtensions.cc ClipsExtensions.h Base.h Problem.h \
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
Core.o: Core.cc Core.h Device.h
CoreRegistrar.o: CoreRegistrar.cc Problem.h CoreRegistrar.h Core.h \
 Device.h
IOController.o: IOController.cc IOController.h Problem.h Device.h \
 IODevice.h Base.h Core.h ClipsExtensions.h misc/maya/clips.h \
 misc/maya/setup.h misc/maya/os_shim.h misc/maya/platform.h \
 misc/maya/envrnmnt.h misc/maya/symbol.h misc/maya/usrsetup.h \
 misc/maya/argacces.h misc/maya/expressn.h misc/maya/exprnops.h \
 misc/maya/exprnpsr.h misc/maya/extnfunc.h misc/maya/evaluatn.h \
 misc/maya/constant.h misc/maya/userdata.h misc/maya/factmngr.h \
 misc/maya/conscomp.h misc/maya/constrct.h misc/maya/moduldef.h \
 misc/maya/modulpsr.h misc/maya/utility.h misc/maya/scanner.h \
 misc/maya/pprint.h misc/maya/symblcmp.h misc/maya/facthsh.h \
 misc/maya/multifld.h misc/maya/pattern.h misc/maya/match.h \
 misc/maya/network.h misc/maya/ruledef.h misc/maya/agenda.h \
 misc/maya/constrnt.h misc/maya/cstrccom.h misc/maya/reorder.h \
 misc/maya/tmpltdef.h misc/maya/factbld.h misc/maya/object.h \
 misc/maya/memalloc.h misc/maya/cstrcpsr.h misc/maya/filecom.h \
 misc/maya/strngfun.h misc/maya/commline.h misc/maya/router.h \
 misc/maya/prntutil.h misc/maya/filertr.h misc/maya/strngrtr.h \
 misc/maya/iofun.h misc/maya/sysdep.h misc/maya/bmathfun.h \
 misc/maya/watch.h misc/maya/modulbsc.h misc/maya/bload.h \
 misc/maya/exprnbin.h misc/maya/symblbin.h misc/maya/bsave.h \
 misc/maya/rulebsc.h misc/maya/engine.h misc/maya/lgcldpnd.h \
 misc/maya/retract.h misc/maya/drive.h misc/maya/incrrset.h \
 misc/maya/rulecom.h misc/maya/crstrtgy.h misc/maya/dffctdef.h \
 misc/maya/dffctbsc.h misc/maya/tmpltbsc.h misc/maya/tmpltfun.h \
 misc/maya/factcom.h misc/maya/factfun.h misc/maya/globldef.h \
 misc/maya/globlbsc.h misc/maya/globlcom.h misc/maya/dffnxfun.h \
 misc/maya/genrccom.h misc/maya/genrcfun.h misc/maya/classcom.h \
 misc/maya/classexm.h misc/maya/classinf.h misc/maya/classini.h \
 misc/maya/classpsr.h misc/maya/defins.h misc/maya/inscom.h \
 misc/maya/insfun.h misc/maya/insfile.h misc/maya/msgcom.h \
 misc/maya/msgpass.h misc/maya/objrtmch.h WrappedIODevice.h
IrisCore.o: IrisCore.cc IrisCore.h Base.h Problem.h ExecutionUnits.h \
 IODevice.h Device.h Core.h ClipsExtensions.h misc/maya/clips.h \
 misc/maya/setup.h misc/maya/os_shim.h misc/maya/platform.h \
 misc/maya/envrnmnt.h misc/maya/symbol.h misc/maya/usrsetup.h \
 misc/maya/argacces.h misc/maya/expressn.h misc/maya/exprnops.h \
 misc/maya/exprnpsr.h misc/maya/extnfunc.h misc/maya/evaluatn.h \
 misc/maya/constant.h misc/maya/userdata.h misc/maya/factmngr.h \
 misc/maya/conscomp.h misc/maya/constrct.h misc/maya/moduldef.h \
 misc/maya/modulpsr.h misc/maya/utility.h misc/maya/scanner.h \
 misc/maya/pprint.h misc/maya/symblcmp.h misc/maya/facthsh.h \
 misc/maya/multifld.h misc/maya/pattern.h misc/maya/match.h \
 misc/maya/network.h misc/maya/ruledef.h misc/maya/agenda.h \
 misc/maya/constrnt.h misc/maya/cstrccom.h misc/maya/reorder.h \
 misc/maya/tmpltdef.h misc/maya/factbld.h misc/maya/object.h \
 misc/maya/memalloc.h misc/maya/cstrcpsr.h misc/maya/filecom.h \
 misc/maya/strngfun.h misc/maya/commline.h misc/maya/router.h \
 misc/maya/prntutil.h misc/maya/filertr.h misc/maya/strngrtr.h \
 misc/maya/iofun.h misc/maya/sysdep.h misc/maya/bmathfun.h \
 misc/maya/watch.h misc/maya/modulbsc.h misc/maya/bload.h \
 misc/maya/exprnbin.h misc/maya/symblbin.h misc/maya/bsave.h \
 misc/maya/rulebsc.h misc/maya/engine.h misc/maya/lgcldpnd.h \
 misc/maya/retract.h misc/maya/drive.h misc/maya/incrrset.h \
 misc/maya/rulecom.h misc/maya/crstrtgy.h misc/maya/dffctdef.h \
 misc/maya/dffctbsc.h misc/maya/tmpltbsc.h misc/maya/tmpltfun.h \
 misc/maya/factcom.h misc/maya/factfun.h misc/maya/globldef.h \
 misc/maya/globlbsc.h misc/maya/globlcom.h misc/maya/dffnxfun.h \
 misc/maya/genrccom.h misc/maya/genrcfun.h misc/maya/classcom.h \
 misc/maya/classexm.h misc/maya/classinf.h misc/maya/classini.h \
 misc/maya/classpsr.h misc/maya/defins.h misc/maya/inscom.h \
 misc/maya/insfun.h misc/maya/insfile.h misc/maya/msgcom.h \
 misc/maya/msgpass.h misc/maya/objrtmch.h IOController.h \
 WrappedIODevice.h IrisCoreTypes.h iris_defines.h \
 IrisCoreSecondaryStorageController.h IrisClipsExtensions.h
IrisCoreAssembler.o: IrisCoreAssembler.cc Base.h Problem.h \
 AssemblerBase.h IrisCore.h ExecutionUnits.h IODevice.h Device.h Core.h \
 ClipsExtensions.h misc/maya/clips.h misc/maya/setup.h \
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
 misc/maya/objrtmch.h IOController.h WrappedIODevice.h IrisCoreTypes.h \
 iris_defines.h IrisClipsExtensions.h
IrisCoreSecondaryStorageController.o: \
 IrisCoreSecondaryStorageController.cc IrisCoreTypes.h \
 IrisCoreSecondaryStorageController.h Base.h Problem.h ExecutionUnits.h \
 IODevice.h Device.h Core.h ClipsExtensions.h misc/maya/clips.h \
 misc/maya/setup.h misc/maya/os_shim.h misc/maya/platform.h \
 misc/maya/envrnmnt.h misc/maya/symbol.h misc/maya/usrsetup.h \
 misc/maya/argacces.h misc/maya/expressn.h misc/maya/exprnops.h \
 misc/maya/exprnpsr.h misc/maya/extnfunc.h misc/maya/evaluatn.h \
 misc/maya/constant.h misc/maya/userdata.h misc/maya/factmngr.h \
 misc/maya/conscomp.h misc/maya/constrct.h misc/maya/moduldef.h \
 misc/maya/modulpsr.h misc/maya/utility.h misc/maya/scanner.h \
 misc/maya/pprint.h misc/maya/symblcmp.h misc/maya/facthsh.h \
 misc/maya/multifld.h misc/maya/pattern.h misc/maya/match.h \
 misc/maya/network.h misc/maya/ruledef.h misc/maya/agenda.h \
 misc/maya/constrnt.h misc/maya/cstrccom.h misc/maya/reorder.h \
 misc/maya/tmpltdef.h misc/maya/factbld.h misc/maya/object.h \
 misc/maya/memalloc.h misc/maya/cstrcpsr.h misc/maya/filecom.h \
 misc/maya/strngfun.h misc/maya/commline.h misc/maya/router.h \
 misc/maya/prntutil.h misc/maya/filertr.h misc/maya/strngrtr.h \
 misc/maya/iofun.h misc/maya/sysdep.h misc/maya/bmathfun.h \
 misc/maya/watch.h misc/maya/modulbsc.h misc/maya/bload.h \
 misc/maya/exprnbin.h misc/maya/symblbin.h misc/maya/bsave.h \
 misc/maya/rulebsc.h misc/maya/engine.h misc/maya/lgcldpnd.h \
 misc/maya/retract.h misc/maya/drive.h misc/maya/incrrset.h \
 misc/maya/rulecom.h misc/maya/crstrtgy.h misc/maya/dffctdef.h \
 misc/maya/dffctbsc.h misc/maya/tmpltbsc.h misc/maya/tmpltfun.h \
 misc/maya/factcom.h misc/maya/factfun.h misc/maya/globldef.h \
 misc/maya/globlbsc.h misc/maya/globlcom.h misc/maya/dffnxfun.h \
 misc/maya/genrccom.h misc/maya/genrcfun.h misc/maya/classcom.h \
 misc/maya/classexm.h misc/maya/classinf.h misc/maya/classini.h \
 misc/maya/classpsr.h misc/maya/defins.h misc/maya/inscom.h \
 misc/maya/insfun.h misc/maya/insfile.h misc/maya/msgcom.h \
 misc/maya/msgpass.h misc/maya/objrtmch.h
Linker.o: Linker.cc Core.h Device.h CoreRegistrar.h Problem.h
RegisteredAssemblers.o: RegisteredAssemblers.cc Problem.h RegisterEntry.h \
 AssemblerRegistrar.h IrisCore.h Base.h ExecutionUnits.h IODevice.h \
 Device.h Core.h ClipsExtensions.h misc/maya/clips.h misc/maya/setup.h \
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
 misc/maya/objrtmch.h IOController.h WrappedIODevice.h IrisCoreTypes.h \
 iris_defines.h Cisc0Core.h cisc0_defines.h
RegisteredCores.o: RegisteredCores.cc Problem.h RegisterEntry.h \
 CoreRegistrar.h IrisCore.h Base.h ExecutionUnits.h IODevice.h Device.h \
 Core.h ClipsExtensions.h misc/maya/clips.h misc/maya/setup.h \
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
 misc/maya/objrtmch.h IOController.h WrappedIODevice.h IrisCoreTypes.h \
 iris_defines.h Cisc0Core.h cisc0_defines.h
RegisteredExternalAddressAssemblers.o: \
 RegisteredExternalAddressAssemblers.cc \
 AssemblerExternalAddressRegistrar.h Cisc0ClipsExtensions.h \
 IrisClipsExtensions.h
Repl.o: Repl.cc misc/maya/clips.h misc/maya/setup.h misc/maya/os_shim.h \
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
 ClipsExtensions.h Base.h Problem.h AssemblerExternalAddressRegistrar.h
ReplBootstrap.o: ReplBootstrap.cc misc/maya/clips.h misc/maya/setup.h \
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
 misc/maya/objrtmch.h ClipsExtensions.h Base.h Problem.h
Simulator.o: Simulator.cc Problem.h Core.h Device.h CoreRegistrar.h
WrappedIODevice.o: WrappedIODevice.cc WrappedIODevice.h Base.h Problem.h \
 Device.h Core.h ClipsExtensions.h misc/maya/clips.h misc/maya/setup.h \
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
 misc/maya/objrtmch.h IODevice.h
