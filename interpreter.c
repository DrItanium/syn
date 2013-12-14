#include <u.h>
#include <libc.h>
#include "interpreter.h"
void main() {
	processor proc;
	setupprocessor(&proc);
	exits(0);
}
void setupprocessor(processor* proc) {
	/* Upper registers have a special purpose */
	/* false/zero */
	proc->gpr[FalseRegister] = 0;
	/* true/one */
	proc->gpr[TrueRegister] = 1;
	/* processor-id */
	proc->gpr[ProcessorIdRegister] = processor_count++;
	/* register count */
	proc->gpr[RegisterCountRegister] = 128;
	/* cell count [64-bits * total count] */
	proc->gpr[CellCountRegister] = 131072;
	/* program-counter */
	proc->gpr[ProgramCounter] = 2048;

}
instruction retrieveinstruction(processor* proc) {
	return (instruction)proc->memory[proc->programcounter];
}
void incrementprogramcounter(processor* proc) {
	proc->gpr[ProgramCounter]++;
}
int instructionexecutable(processor* proc, instruction inst) {
	return proc->gpr[inst.bytes[0]];
}
void cycle(processor* proc) {
	instruction a, b;
	a = retrieveinstruction(proc);
	/* check to see if we should execute */
	if(instructionexecutable(proc, a)) {
		switch(inst.bytes[1]) {
			case 0:
				nop(proc);
				break;
			case 1:
				add(proc, a.bytes[2], a.bytes[4], a.bytes[5]);
				break;
			case 2:
				sub(proc, a.bytes[2], a.bytes[4], a.bytes[5]);
				break;
			case 3:
				mul(proc, a.bytes[2], a.bytes[4], a.bytes[5]);
				break;
			case 4:
				divop(proc, a.bytes[2], a.bytes[4], a.bytes[5]);
				break;
			case 5:
				rightshift(proc, a.bytes[2], a.bytes[4], a.bytes[5]);
				break;
			case 6:
				leftshift(proc, a.bytes[2], a.bytes[4], a.bytes[5]);
				break;
			case 7:
				binaryor(proc, a.bytes[2], a.bytes[4], a.bytes[5]);
				break;
			case 8:
				binaryand(proc, a.bytes[2], a.bytes[4], a.bytes[5]);
				break;
			case 9:
				binarynot(proc, a.bytes[2], a.bytes[4]);
				break;
			case 10:
				equals(proc, a.bytes[2], a.bytes[3], a.bytes[4], a.bytes[5]);
				break;
			case 11:
				notequals(proc, a.bytes[2], a.bytes[3], a.bytes[4], a.bytes[5]);
				break;
			case 12:
				greaterthan(proc, a.bytes[2], a.bytes[3], a.bytes[4], a.bytes[5]);
				break;
			case 13:
				lessthan(proc, a.bytes[2], a.bytes[3], a.bytes[4], a.bytes[5]);
				break;
			case 14:
				load(proc, a.bytes[2], a.bytes[4]);
				break;
			case 15:
				store(proc, a.bytes[2], a.bytes[4]);
				break;
			case 16:
				/* we need to grab the next cell */
				incrementprogramcounter(proc);
				b = retrieveinstruction(proc);
				branch(proc, b.value);
				break;
			case 17:
				/* this is an interesting case */
				incrementprogramcounter(proc);
				b = retrieveinstruction(proc);
				set(proc, a.bytes[2], b.value);
		}
	} 
	incrementprogramcounter(proc);
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
	proc->programcounter = dest;
}

void set(processor* proc, uchar dest, uvlong value) {
		proc->gpr[dest] = value;
}
