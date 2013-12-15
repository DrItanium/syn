#include <u.h>
#include <libc.h>
#include "interpreter.h"
void main() {
	processor proc;
	setupprocessor(&proc);
	while(cycle(&proc));
	exits(0);
}
instruction retrieveinstruction(processor* proc) {
	return (instruction)proc->memory[proc->gpr[ProgramCounter]];
}
void incrementprogramcounter(processor* proc) {
	add(proc, ProgramCounter, ProgramCounter, TrueRegister);
	modop(proc, ProgramCounter, ProgramCounter, CellCountRegister);
}
int instructionexecutable(processor* proc, instruction inst) {
	return proc->gpr[inst.predicate];
}
int cycle(processor* proc) {
	instruction a, b;
	a = retrieveinstruction(proc);
	/* check to see if we should execute */
	if(instructionexecutable(proc, a)) {
		switch(a.id) {
			case NopInstruction:
				nop(proc);
				break;
			case AddInstruction:
				add(proc, a.destination0, a.source0, a.source1);
				break;
			case SubInstruction:
				sub(proc, a.destination0, a.source0, a.source1);
				break;
			case MulInstruction:
				mul(proc, a.destination0, a.source0, a.source1);
				break;
			case DivInstruction:
				divop(proc, a.destination0, a.source0, a.source1);
				break;
			case RightShiftInstruction:
				rightshift(proc, a.destination0, a.source0, a.source1);
				break;
			case LeftShiftInstruction:
				leftshift(proc, a.destination0, a.source0, a.source1);
				break;
			case BinaryOrInstruction:
				binaryor(proc, a.destination0, a.source0, a.source1);
				break;
			case BinaryAndInstruction:
				binaryand(proc, a.destination0, a.source0, a.source1);
				break;
			case BinaryNotInstruction:
				binarynot(proc, a.destination0, a.source0);
				break;
			case EqualsInstruction:
				equals(proc, a.destination0, a.destination1, a.source0, a.source1);
				break;
			case NotEqualsInstruction:
				notequals(proc, a.destination0, a.destination1, a.source0, a.source1);
				break;
			case GreaterThanInstruction:
				greaterthan(proc, a.destination0, a.destination1, a.source0, a.source1);
				break;
			case LessThanInstruction:
				lessthan(proc, a.destination0, a.destination1, a.source0, a.source1);
				break;
			case LoadInstruction:
				load(proc, a.destination0, a.source0);
				break;
			case StoreInstruction:
				store(proc, a.destination0, a.source0);
				break;
			case BranchInstruction:
				/* we need to grab the next cell */
				incrementprogramcounter(proc);
				b = retrieveinstruction(proc);
				branch(proc, b.value % proc->gpr[CellCountRegister]);
				break;
			case SetInstruction:
				/* this is an interesting case */
				incrementprogramcounter(proc);
				b = retrieveinstruction(proc);
				set(proc, a.destination0, b.value);
				break;
			case ModInstruction:
				modop(proc, a.destination0, a.source0, a.source1);
				break;
			case CallInstruction:
				call(proc, b.source0);
				break;
			case RetInstruction:
				ret(proc);
				break;
			case PlatformCallInstruction:
				platformcall(proc);
				break;
			case TerminateInstruction:
			default:
				return 0;
		}
	} 
	incrementprogramcounter(proc);
	return 1;
}
void add(processor* proc, uchar dest, uchar src0, uchar src1) {
	proc->gpr[dest] = proc->gpr[src0] + proc->gpr[src1];	
}

void sub(processor* proc, uchar dest, uchar src0, uchar src1) {
	proc->gpr[dest] = proc->gpr[src0] - proc->gpr[src1];	
}
void mul(processor* proc, uchar dest, uchar src0, uchar src1) {
	proc->gpr[dest] = proc->gpr[src0] * proc->gpr[src1];	
}
void divop(processor* proc, uchar dest, uchar src0, uchar src1) {
	proc->gpr[dest] = proc->gpr[src0] / proc->gpr[src1];	
}
void rightshift(processor* proc, uchar dest, uchar src0, uchar src1) {
	proc->gpr[dest] = proc->gpr[src0] >> proc->gpr[src1];	
}
void leftshift(processor* proc, uchar dest, uchar src0, uchar src1) {
	proc->gpr[dest] = proc->gpr[src0] << proc->gpr[src1];	
}
void binaryor(processor* proc, uchar dest, uchar src0, uchar src1) {
	proc->gpr[dest] = proc->gpr[src0] | proc->gpr[src1];	
}
void binaryand(processor* proc, uchar dest, uchar src0, uchar src1) {
	proc->gpr[dest] = proc->gpr[src0] & proc->gpr[src1];	
}
void binaryxor(processor* proc, uchar dest, uchar src0, uchar src1) {
	proc->gpr[dest] = proc->gpr[src0] ^ proc->gpr[src1];	
}
void binarynot(processor* proc, uchar dest, uchar src0) {
	proc->gpr[dest] = !(proc->gpr[src0]);	
}
void equals(processor* proc, uchar desttrue, uchar destfalse, uchar src0, uchar src1) {
	uvlong tmp;
	tmp = proc->gpr[src0] == proc->gpr[src1];	
	proc->gpr[desttrue] = tmp;
	proc->gpr[destfalse] = !tmp;
}
void notequals(processor* proc, uchar desttrue, uchar destfalse, uchar src0, uchar src1) {
	equals(proc, destfalse, desttrue, src0, src1);
}
void greaterthan(processor* proc, uchar desttrue, uchar destfalse, uchar src0, uchar src1) {
	uvlong tmp;
	tmp = proc->gpr[src0] > proc->gpr[src1];
	proc->gpr[desttrue] = tmp;
	proc->gpr[destfalse] = !tmp;
}
void lessthan(processor* proc, uchar desttrue, uchar destfalse, uchar src0, uchar src1) {
	uvlong tmp;
	tmp = proc->gpr[src0] < proc->gpr[src1];
	proc->gpr[desttrue] = tmp;
	proc->gpr[destfalse] = !tmp;
}

void load(processor* proc, uchar dest, uchar src) {
	proc->gpr[dest] = proc->memory[proc->gpr[src]];
}
void store(processor* proc, uchar dest, uchar src) {
	proc->memory[proc->gpr[dest]] = proc->gpr[src];
}

void branch(processor* proc, uvlong dest) {
	set(proc, ProgramCounter, dest - 1);
}

void set(processor* proc, uchar dest, uvlong value) {
	proc->gpr[dest] = value;
}
void nop(processor* proc) { }

void modop(processor* proc, uchar dest, uchar src0, uchar src1) {
	proc->gpr[dest] = proc->gpr[src0] % proc->gpr[src1];	
}

void call(processor* proc, uchar dest) {
	add(proc, ReturnRegister, ProgramCounter, FalseRegister);
	branch(proc, proc->gpr[dest]);
}

void ret(processor* proc) {
	add(proc, ProgramCounter, ReturnRegister, FalseRegister);
}
static void installexitcall(processor* proc);
static void installputccall(processor* proc);
static void installgetccall(processor* proc);
static void installprocessorloop(processor* proc);
void setupprocessor(processor* proc) {

	/* Upper registers have a special purpose */
	proc->gpr[FalseRegister] = 0;
	proc->gpr[TrueRegister] = 1;
	proc->gpr[ProcessorIdRegister] = processor_count++;
	proc->gpr[RegisterCountRegister] = 128;
	proc->gpr[CellCountRegister] = 131072;
	proc->gpr[ReturnRegister] = 0;
	/* program-counter */
	proc->gpr[ProgramCounter] = 2048;
	installexitcall(proc);
	installputccall(proc);
	installgetccall(proc);
	installprocessorloop(proc);
}
void installprocessorloop(processor* proc) {
	instruction nop;
	instruction branch0, branch1;
	/* just loop at this point */
	nop.value = 0;
	nop.predicate = TrueRegister;
	nop.id = NopInstruction;
	branch0.value = 0;
	branch0.predicate = TrueRegister;
	branch0.id = BranchInstruction;
	branch1.value = proc->gpr[ProgramCounter];
	proc->memory[proc->gpr[ProgramCounter]] = nop.value;

	proc->memory[proc->gpr[ProgramCounter] + 1] = branch0.value;
	proc->memory[proc->gpr[ProgramCounter] + 2] = branch1.value;
}
void installplatformcallhandler(processor* proc) {
	/* code is as follows
	 *
}
void installexitcall(processor* proc) {
	instruction terminate;
	terminate.value = 0;
	terminate.id = TerminateInstruction;
	terminate.predicate = TrueRegister;
	proc->memory[0] = terminate.value;
}

void installputccall(processor* proc) {
	/* code is as follows @ index 1
	 *
	 * platform 
	 * ret
	 */
	instruction setA,setB, platform, ret;
	setA.value = 0;
	setB.value = 1;
	platform.value = 0;
	ret.value = 0;
	setA.predicate = TrueRegister;
	setA.id = SetInstruction;
	platform.predicate = TrueRegister;
	platform.id = PlatformCallInstruction;
	proc->memory[1] = setA.value;
	proc->memory[2] = setB.value;
	proc->memory[3] = platform.value;
	proc->memory[4] = ret.value;
}
