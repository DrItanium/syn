# iris - a series of virtual cpus and other such things as I learn more about
# processor simulation
# See LICENSE file for copyright and license details.

include config.mk

ARCH_OBJECTS = iris17.o \
			   iris16.o \
			   iris18.o \
			   iris19.o

COMMON_THINGS = iris_base.o \
				libmaya.a

SIM_OBJECTS = iris_sim.o \
			  sim_registration.o \
			  ${ARCH_OBJECTS} \
			  ${COMMON_THINGS}

SIM_BINARY = iris_sim

ASM_PARSERS_OBJECTS = iris16_lex.yy.o \
					  iris16_asm.tab.o \
					  iris17_lex.yy.o \
					  iris17_asm.tab.o \
					  iris18_lex.yy.o \
					  iris18_asm.tab.o \
					  iris19_lex.yy.o \
					  iris19_asm.tab.o \

ASM_OBJECTS = iris_asm.o \
			  asm_interact.o \
			  ${COMMON_THINGS} \
				${ARCH_OBJECTS} \
				sim_registration.o \
			  ${ASM_PARSERS_OBJECTS}

REPL_BINARY = iris_repl

REPL_OBJECTS= iris_repl.o \
			  ${COMMON_THINGS}

ASM_PARSERS = iris16_lex.yy.c \
			  iris16_asm.tab.c \
			  iris16_asm.tab.h \
			  iris17_lex.yy.c \
			  iris17_asm.tab.c \
			  iris17_asm.tab.h \
			  iris18_lex.yy.c \
			  iris18_asm.tab.c \
			  iris18_asm.tab.h \
			  iris19_lex.yy.c \
			  iris19_asm.tab.c \
			  iris19_asm.tab.h

ASM_BINARY = iris_asm

LINK_OBJECTS = iris_link.o \
				${ARCH_OBJECTS} \
				sim_registration.o \
			  ${COMMON_THINGS}

LINK_BINARY = iris_link

ALL_BINARIES = ${SIM_BINARY} \
			   ${ASM_BINARY} \
			   ${LINK_BINARY} \
			   ${REPL_BINARY}

DEFINE_OBJECTS = iris19_defines.h

ALL_OBJECTS = ${COMMON_THINGS} \
			  ${SIM_OBJECTS} \
			  ${ASM_OBJECTS} \
			  ${LINK_OBJECTS} \
			  ${ARCH_OBJECTS} \
			  ${REPL_OBJECTS} \
			  ${DEFINE_OBJECTS}

all: options ${REPL_BINARY} ${SIM_BINARY} ${ASM_BINARY} ${LINK_BINARY}

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
	@echo iris build options:
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

iris19_defines.h: iris_repl def/iris19/instruction.clp cmd/deffield.clp lib/cortex.clp lib/reset-run-exit.clp iris_base.h
	@echo "Generating encoders, decoders and enumerations for iris19..."
	@./deffield.sh -f2 def/iris19/instruction.clp -f2 lib/reset-run-exit.clp > iris19_defines.h

asm_interact.o: asm_interact.cc asm_interact.h iris18.h iris_base.h \
 Problem.h Core.h sim_registration.h def/iris18/ops.def \
 def/iris18/arithmetic_ops.def def/iris18/syscalls.def \
 def/iris18/compare.enum def/iris18/logical.enum def/iris18/memory.enum \
 def/iris18/complex.def def/iris18/instruction.def def/iris18/misc.def \
 def/iris18/registers.def def/iris18/logical_generic.sig \
 def/iris18/arithmetic.sig def/iris18/move.sig def/iris18/memory.sig \
 def/iris18/set.sig iris16.h def/iris16/enums.def \
 def/iris16/core_body.def def/iris16/groups.def def/iris16/misc.def \
 def/iris16/instruction.def iris17.h def/iris17/groups.def \
 def/iris17/instruction.def def/iris17/compare.def \
 def/iris17/arithmetic.def def/iris17/move.def def/iris17/jump.def \
 def/iris17/misc.def def/iris17/syscalls.def iris19.h def/iris19/ops.def \
 def/iris19/arithmetic_ops.def def/iris19/compare.enum \
 def/iris19/logical.enum def/iris19/move.def def/iris19/instruction.def
iris16.o: iris16.cc iris16.h iris_base.h Problem.h Core.h \
 def/iris16/enums.def def/iris16/core_body.def def/iris16/groups.def \
 def/iris16/misc.def def/iris16/instruction.def def/iris16/groups.def \
 def/iris16/compare.def def/iris16/arithmetic.def def/iris16/jump.def \
 def/iris16/misc.def def/iris16/move.def
iris17.o: iris17.cc iris17.h iris_base.h Problem.h Core.h \
 def/iris17/groups.def def/iris17/instruction.def def/iris17/compare.def \
 def/iris17/arithmetic.def def/iris17/move.def def/iris17/jump.def \
 def/iris17/misc.def def/iris17/syscalls.def
iris18.o: iris18.cc iris18.h iris_base.h Problem.h Core.h \
 sim_registration.h def/iris18/ops.def def/iris18/arithmetic_ops.def \
 def/iris18/syscalls.def def/iris18/compare.enum def/iris18/logical.enum \
 def/iris18/memory.enum def/iris18/complex.def def/iris18/instruction.def \
 def/iris18/misc.def def/iris18/registers.def \
 def/iris18/logical_generic.sig def/iris18/arithmetic.sig \
 def/iris18/move.sig def/iris18/memory.sig def/iris18/set.sig \
 def/iris18/bitmask4bit.def def/iris18/bitmask8bit.def
iris19.o: iris19.cc iris19.h iris_base.h Problem.h Core.h \
 sim_registration.h def/iris19/ops.def def/iris19/arithmetic_ops.def \
 def/iris19/compare.enum def/iris19/logical.enum def/iris19/move.def \
 def/iris19/instruction.def iris19_defines.h
iris_asm.o: iris_asm.cc Problem.h asm_interact.h
iris_base.o: iris_base.cc iris_base.h Problem.h
iris_link.o: iris_link.cc Core.h sim_registration.h Problem.h
iris_repl.o: iris_repl.cc misc/maya/clips.h misc/maya/setup.h \
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
 misc/maya/objrtmch.h iris_base.h Problem.h Core.h
iris_sim.o: iris_sim.cc Problem.h Core.h sim_registration.h iris18.h \
 iris_base.h def/iris18/ops.def def/iris18/arithmetic_ops.def \
 def/iris18/syscalls.def def/iris18/compare.enum def/iris18/logical.enum \
 def/iris18/memory.enum def/iris18/complex.def def/iris18/instruction.def \
 def/iris18/misc.def def/iris18/registers.def \
 def/iris18/logical_generic.sig def/iris18/arithmetic.sig \
 def/iris18/move.sig def/iris18/memory.sig def/iris18/set.sig iris16.h \
 def/iris16/enums.def def/iris16/core_body.def def/iris16/groups.def \
 def/iris16/misc.def def/iris16/instruction.def
Phoenix.o: Phoenix.cc Phoenix.h Core.h iris17.h iris_base.h Problem.h \
 def/iris17/groups.def def/iris17/instruction.def def/iris17/compare.def \
 def/iris17/arithmetic.def def/iris17/move.def def/iris17/jump.def \
 def/iris17/misc.def def/iris17/syscalls.def
sim_registration.o: sim_registration.cc sim_registration.h Core.h \
 iris18.h iris_base.h Problem.h def/iris18/ops.def \
 def/iris18/arithmetic_ops.def def/iris18/syscalls.def \
 def/iris18/compare.enum def/iris18/logical.enum def/iris18/memory.enum \
 def/iris18/complex.def def/iris18/instruction.def def/iris18/misc.def \
 def/iris18/registers.def def/iris18/logical_generic.sig \
 def/iris18/arithmetic.sig def/iris18/move.sig def/iris18/memory.sig \
 def/iris18/set.sig iris16.h def/iris16/enums.def \
 def/iris16/core_body.def def/iris16/groups.def def/iris16/misc.def \
 def/iris16/instruction.def iris17.h def/iris17/groups.def \
 def/iris17/instruction.def def/iris17/compare.def \
 def/iris17/arithmetic.def def/iris17/move.def def/iris17/jump.def \
 def/iris17/misc.def def/iris17/syscalls.def iris19.h def/iris19/ops.def \
 def/iris19/arithmetic_ops.def def/iris19/compare.enum \
 def/iris19/logical.enum def/iris19/move.def def/iris19/instruction.def \
 iris19_defines.h
Storage.o: Storage.cc Storage.h Core.h iris16.h iris_base.h Problem.h \
 def/iris16/enums.def def/iris16/core_body.def def/iris16/groups.def \
 def/iris16/misc.def def/iris16/instruction.def


