#define FalseRegister 127
#define TrueRegister 126
#define ProcessorIdRegister 125
#define RegisterCountRegister 124
#define CellCountRegister 123
#define ProgramCounter 122
uvlong processor_count = 0;
typedef struct processor {
	uvlong gpr[128];
	uvlong memory[131072];
} processor;
typedef union instruction {
	uvlong value;
	uchar bytes[8];
	/* an instruction is made up of eight parts 
	 * 0) predicate ind
	 * 1) instruction id
	 * 2) destination (true)
	 * 3) destination (false)
	 * 4) src0 ind
	 * 5) src1 ind
	 * 6) src2 ind
	 * 7) src3 ind
	 */
} instruction;

/* translation table
 * 0) nop
 * 1) add
 * 2) sub
 * 3) mul
 * 4) div
 * 5) rightshift
 * 6) leftshift
 * 7) binaryor
 * 8) binaryand
 * 9) binarynot
 * 10) equals
 * 11) notequals
 * 12) greaterthan
 * 13) lessthan
 * 14) load
 * 15) store
 * 16) branch
 * 17) set
 * 18) terminate
 */


void nop(processor* proc);
void add(processor* proc, uchar dest, uchar src0, uchar src1);
void sub(processor* proc, uchar dest, uchar src0, uchar src1);
void mul(processor* proc, uchar dest, uchar src0, uchar src1);
void divop(processor* proc, uchar dest, uchar src0, uchar src1);
void rightshift(processor* proc, uchar dest, uchar src0, uchar src1);
void leftshift(processor* proc, uchar dest, uchar src0, uchar src1);
void binaryor(processor* proc, uchar dest, uchar src0, uchar src1);
void binaryand(processor* proc, uchar dest, uchar src0, uchar src1);
void binaryxor(processor* proc, uchar dest, uchar src0, uchar src1);
void binarynot(processor* proc, uchar dest, uchar src0);
void equals(processor* proc, uchar desttrue, uchar destfalse, uchar src0, uchar src1);
void notequals(processor* proc, uchar desttrue, uchar destfalse, uchar src0, uchar src1);
void greaterthan(processor* proc, uchar desttrue, uchar destfalse, uchar src0, uchar src1);
void lessthan(processor* proc, uchar desttrue, uchar destfalse, uchar src0, uchar src1);

void load(processor* proc, uchar dest, uchar src);
void store(processor* proc, uchar dest, uchar src);

void branch(processor* proc, uvlong dest);
void set(processor* proc, uchar dest, uvlong value);

/* helper routines */
void incrementprogramcounter(processor* proc);
instruction retrieveinstruction(processor* proc);
int cycle(processor* proc);
int instructionexecutable(processor* proc, instruction inst);

/* bios routines */
void setupprocessor(processor* proc);
