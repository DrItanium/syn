#include <u.h>
#include <libc.h>
#include "interpreter.h"
void main() {
   processor proc;
	
	exits(0);
}

void add(processor* proc, uchar predicate, uchar dest, uchar src0, uchar src1) {
	if(proc->gpr[predicate]) {
		proc->gpr[dest] = proc->gpr[src0] + proc->gpr[src1];	
	}
}

void sub(processor* proc, uchar predicate, uchar dest, uchar src0, uchar src1) {
	if(proc->gpr[predicate]) {
		proc->gpr[dest] = proc->gpr[src0] - proc->gpr[src1];	
	}

}
void mul(processor* proc, uchar predicate, uchar dest, uchar src0, uchar src1) {
	if(proc->gpr[predicate]) {
		proc->gpr[dest] = proc->gpr[src0] * proc->gpr[src1];	
	}
}
void div(processor* proc, uchar predicate, uchar dest, uchar src0, uchar src1) {
	if(proc->gpr[predicate]) {
		proc->gpr[dest] = proc->gpr[src0] / proc->gpr[src1];	
	}
}
void rightshift(processor* proc, uchar predicate, uchar dest, uchar src0, uchar src1) {
	if(proc->gpr[predicate]) {
		proc->gpr[dest] = proc->gpr[src0] >> proc->gpr[src1];	
	}
}
void leftshift(processor* proc, uchar predicate, uchar dest, uchar src0, uchar src1) {
	if(proc->gpr[predicate]) {
		proc->gpr[dest] = proc->gpr[src0] << proc->gpr[src1];	
	}
}
void binaryor(processor* proc, uchar predicate, uchar dest, uchar src0, uchar src1) {
	if(proc->gpr[predicate]) {
		proc->gpr[dest] = proc->gpr[src0] | proc->gpr[src1];	
	}
}
void binaryand(processor* proc, uchar predicate, uchar dest, uchar src0, uchar src1) {
	if(proc->gpr[predicate]) {
		proc->gpr[dest] = proc->gpr[src0] & proc->gpr[src1];	
	}
}
void binaryxor(processor* proc, uchar predicate, uchar dest, uchar src0, uchar src1) {
	if(proc->gpr[predicate]) {
		proc->gpr[dest] = proc->gpr[src0] ^ proc->gpr[src1];	
	}
	
}
void binarynot(processor* proc, uchar predicate, uchar dest, uchar src0);
void equals(processor* proc, uchar predicate, uchar desttrue, uchar destfalse, uchar src0, uchar src1);
void notequals(processor* proc, uchar predicate, uchar desttrue, uchar destfalse, uchar src0, uchar src1);
void greaterthan(processor* proc, uchar predicate, uchar desttrue, uchar destfalse, uchar src0, uchar src1);
void lessthan(processor* proc, uchar predicate, uchar desttrue, uchar destfalse, uchar src0, uchar src1);

void load(processor* proc, uchar predicate, uchar dest, uchar src);
void store(processor* proc, uchar predicate, uchar dest, uchar src);

void branch(processor* proc, uchar predicate, uvlong dest);
