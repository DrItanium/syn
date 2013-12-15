
/* default processor type */
uvlong processor_count = 0;
typedef struct processor {
	uvlong gpr[128];
	uvlong memory[131072];
} processor;

typedef union instruction {
	uvlong value;
	uchar bytes[8];
} instruction;

/* processor register indicies */
enum {
	FalseRegister = 127,
	TrueRegister = 126,
	ProcessorIdRegister = 125,
	RegisterCountRegister = 124,
	CellCountRegister = 123,
	ProgramCounter = 122,
};

/* instruction components */
enum {
	InstructionPredicate = 0,
	InstructionId = 1,
	InstructionDestination0 = 2,
	InstructionDestination1 = 3,
	InstructionSource0 = 4,
	InstructionSource1 = 5,
	InstructionByte6 = 6,
	InstructionByte7 = 7,
};

/* instruction translation table */
enum {
	NopInstruction = 0,
	AddInstruction = 1,
	SubInstruction = 2,
	MulInstruction = 3,
	DivInstruction = 4,
	RightShiftInstruction = 5,
	LeftShiftInstruction = 6,
	BinaryOrInstruction = 7,
	BinaryAndInstruction = 8,
	BinaryNotInstruction = 9,
	EqualsInstruction = 10,
	NotEqualsInstruction = 11,
	GreaterThanInstruction = 12,
	LessThanInstruction = 13,
	LoadInstruction = 14,
	StoreInstruction = 15,
	BranchInstruction = 16,
	SetInstruction = 17,
	ModInstruction = 18,
	TerminateInstruction = 255,
};

void nop(processor* proc);
void add(processor* proc, uchar dest, uchar src0, uchar src1);
void sub(processor* proc, uchar dest, uchar src0, uchar src1);
void mul(processor* proc, uchar dest, uchar src0, uchar src1);
void divop(processor* proc, uchar dest, uchar src0, uchar src1);
void modop(processor* proc, uchar dest, uchar src0, uchar src1);
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
