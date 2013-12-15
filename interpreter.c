#include <u.h>
#include <libc.h>
#include "interpreter.h"
void main() {
	processor proc;
	setupprocessor(&proc);
	while(cycle(&proc));
	exits(0);
}
void setupprocessor(processor* proc) {
	instruction nop;
	instruction branch0, branch1;
	
	/* Upper registers have a special purpose */
	proc->gpr[FalseRegister] = 0;
	proc->gpr[TrueRegister] = 1;
	proc->gpr[ProcessorIdRegister] = processor_count++;
	proc->gpr[RegisterCountRegister] = 128;
	proc->gpr[CellCountRegister] = 131072;
	/* program-counter */
	proc->gpr[ProgramCounter] = 2048;
	/* just loop at this point */
	nop.value = 0;
	nop.bytes[InstructionPredicate] = TrueRegister;
	nop.bytes[InstructionId] = NopInstruction;
	branch0.value = 0;
	branch0.bytes[InstructionPredicate] = TrueRegister;
	branch0.bytes[InstructionId] = BranchInstruction;
	branch1.value = proc->gpr[ProgramCounter];
	proc->memory[proc->gpr[ProgramCounter]] = nop.value;

	proc->memory[proc->gpr[ProgramCounter] + 1] = branch0.value;
	proc->memory[proc->gpr[ProgramCounter] + 2] = branch1.value;
}
instruction retrieveinstruction(processor* proc) {
	return (instruction)proc->memory[proc->gpr[ProgramCounter]];
}
void incrementprogramcounter(processor* proc) {
	proc->gpr[ProgramCounter]++;
}
int instructionexecutable(processor* proc, instruction inst) {
	return proc->gpr[inst.bytes[InstructionPredicate]];
}
int cycle(processor* proc) {
	instruction a, b;
	a = retrieveinstruction(proc);
	/* check to see if we should execute */
	if(instructionexecutable(proc, a)) {
		switch(a.bytes[InstructionId]) {
			case NopInstruction:
				nop(proc);
				break;
			case AddInstruction:
				add(proc, a.bytes[InstructionDestination0], a.bytes[InstructionSource0], a.bytes[InstructionSource1]);
				break;
			case SubInstruction:
				sub(proc, a.bytes[InstructionDestination0], a.bytes[InstructionSource0], a.bytes[InstructionSource1]);
				break;
			case MulInstruction:
				mul(proc, a.bytes[InstructionDestination0], a.bytes[InstructionSource0], a.bytes[InstructionSource1]);
				break;
			case DivInstruction:
				divop(proc, a.bytes[InstructionDestination0], a.bytes[InstructionSource0], a.bytes[InstructionSource1]);
				break;
			case RightShiftInstruction:
				rightshift(proc, a.bytes[InstructionDestination0], a.bytes[InstructionSource0], a.bytes[InstructionSource1]);
				break;
			case LeftShiftInstruction:
				leftshift(proc, a.bytes[InstructionDestination0], a.bytes[InstructionSource0], a.bytes[InstructionSource1]);
				break;
			case BinaryOrInstruction:
				binaryor(proc, a.bytes[InstructionDestination0], a.bytes[InstructionSource0], a.bytes[InstructionSource1]);
				break;
			case BinaryAndInstruction:
				binaryand(proc, a.bytes[InstructionDestination0], a.bytes[InstructionSource0], a.bytes[InstructionSource1]);
				break;
			case BinaryNotInstruction:
				binarynot(proc, a.bytes[InstructionDestination0], a.bytes[InstructionSource0]);
				break;
			case EqualsInstruction:
				equals(proc, a.bytes[InstructionDestination0], a.bytes[InstructionDestination1], a.bytes[InstructionSource0], a.bytes[InstructionSource1]);
				break;
			case NotEqualsInstruction:
				notequals(proc, a.bytes[InstructionDestination0], a.bytes[InstructionDestination1], a.bytes[InstructionSource0], a.bytes[InstructionSource1]);
				break;
			case GreaterThanInstruction:
				greaterthan(proc, a.bytes[InstructionDestination0], a.bytes[InstructionDestination1], a.bytes[InstructionSource0], a.bytes[InstructionSource1]);
				break;
			case LessThanInstruction:
				lessthan(proc, a.bytes[InstructionDestination0], a.bytes[InstructionDestination1], a.bytes[InstructionSource0], a.bytes[InstructionSource1]);
				break;
			case LoadInstruction:
				load(proc, a.bytes[InstructionDestination0], a.bytes[InstructionSource0]);
				break;
			case StoreInstruction:
				store(proc, a.bytes[InstructionDestination0], a.bytes[InstructionSource0]);
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
				set(proc, a.bytes[InstructionDestination0], b.value);
				break;
			case ModInstruction:
				modop(proc, a.bytes[InstructionDestination0], a.bytes[InstructionSource0], a.bytes[InstructionSource1]);
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
	set(proc, ProgramCounter, dest);
}

void set(processor* proc, uchar dest, uvlong value) {
		proc->gpr[dest] = value;
}
void nop(processor* proc) { }

void modop(processor* proc, uchar dest, uchar src0, uchar src1) {
	proc->gpr[dest] = proc->gpr[src0] % proc->gpr[src1];	
}
