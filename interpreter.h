enum {
	RegisterCount = 128,
	MemorySize = 131072,
};
/* default processor type */
uvlong processor_count = 0;
typedef struct processor {
	uvlong gpr[RegisterCount];
	uvlong memory[MemorySize];
} processor;

typedef union instruction {
	uvlong value;
	struct {
		uchar predicate;
		uchar id;
		uchar destination0;
		uchar destination1;
		uchar source0;
		uchar source1;
		uchar byte6;
		uchar byte7;
	};
} instruction;

/* processor register indicies */
enum {
	FalseRegister = 127,
	TrueRegister = 126,
	ProcessorIdRegister = 125,
	RegisterCountRegister = 124,
	CellCountRegister = 123,
	ProgramCounter = 122,
	ReturnRegister = 121,
	/* used to jump into the native cpu through a lookup table */
	PlatformFunctionCallIndex = 120,
	PlatformOutputRegister0 = 119,
	PlatformOutputRegister1 = 118,
	PlatformScratch0 = 117,
	PlatformScratch1 = 116,
	PlatformScratch2 = 115,
	PlatformScratch3 = 114,
	PlatformScratch4 = 113,
	PlatformScratch5 = 112,
	PlatformScratch6 = 111,
	PlatformScratch7 = 110,
	PlatformInputRegister0 = 109,
	PlatformInputRegister1 = 108,
	PlatformInputRegister2 = 107,
	PlatformInputRegister3 = 106,
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
	CallInstruction = 19,
	RetInstruction = 20,
	PlatformCallInstruction = 254,
	TerminateInstruction = 255,
};

/* platform calls */
enum {
	platformexit = 0,
	platformputc = 1,
	platformgetc = 2,
};



/* execution processor set */
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
void call(processor* proc, uchar dest);
void ret(processor* proc);

/* helper routines */
void incrementprogramcounter(processor* proc);
instruction retrieveinstruction(processor* proc);
int cycle(processor* proc);
int instructionexecutable(processor* proc, instruction inst);

/* platform routines */
void setupprocessor(processor* proc);
void platformcall(processor* proc);
