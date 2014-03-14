typedef unsigned char byte;
enum {
   RegisterCount = 8, 
   MemorySize = 65536, /* 8-bit cells */
   MajorOperationGroupCount = 8,
};

typedef struct processor {
   byte gpr[RegisterCount];
   byte memory[MemorySize];
   ushort pc : 16;
   byte predicateregister : 1;
} processor;

typedef union datum {
   ushort value : 16;
   struct {
      byte group : 3;
      ushort rest : 13;
   };
} datum;

typedef union instruction {
   ushort value : 13;
   struct {
      byte op : 4;
      byte dest : 3;
      byte source0 : 3;
      byte source1 : 3;
   } arithmetic;
   struct {
      byte op : 2; 
      byte reg0 : 3;
      union {
         byte immediate : 8;
         byte reg1 : 3;
         struct {
            byte reg1 : 3;
            byte reg2 : 3;
            byte accessmode : 1;
         } addressmode;
      };
   } move;
   struct {
      byte distance : 1;
      byte conditional : 2;
      byte immediatemode : 1;
      union {
         byte immediate : 8;
         union {
            byte reg1 : 3;
         } shortform;
         struct {
            byte reg0 : 3;
            byte reg1 : 3;
         } longtype;
      };
   } jump;
   struct {
      byte op : 3;
      byte reg0 : 3;
      byte reg1 : 3;
      byte combinebits : 4; /* nil, and, or, xor */
   } compare;
} instruction;


/* Instructions Groups */
enum {
   InstructionGroupArithmetic = 0,
   InstructionGroupMove,
   InstructionGroupJump,
   InstructionGroupCompare,
};
enum {
   ArithmeticOpAdd = 0,
   ArithmeticOpSub,
   ArithmeticOpMul,
   ArithmeticOpDiv,
   ArithmeticOpRem,
   ArithmeticOpShiftLeft,
   ArithmeticOpShiftRight,
   ArithmeticOpBinaryAnd,
   ArithmeticOpBinaryOr,
   ArithmeticOpBinaryNot,
   ArithmeticOpBinaryXor,
};
enum {
   MoveOpRegToReg = 0,
   MoveOpImmediateToReg,
   MoveOpRegToAddress,
};
enum {
   JumpDistanceShort = 0,
   JumpDistanceLong = 1,
};
enum {
   JumpOpUnconditional = 0,
   JumpOpIfTrue,
   JumpOpIfFalse,
};
enum {
   CompareOpEq = 0,
   CompareOpNeq,
   CompareOpLessThan,
   CompareOpGreaterThan,
   CompareOpLessThanOrEqualTo,
   CompareOpGreaterThanOrEqualTo,
};

enum {
   CombineBitsOpNil = 0,
   CombineBitsOpAnd,
   CombineBitsOpOr,
   CombineBitsOpXor,
};

enum {
   AccessModeMoveOpLoad = 0,
   AccessModeMoveOpStore,
};
void arithmetic(processor* proc, instruction inst);
void move(processor* proc, instruction inst);
void jump(processor* proc, instruction inst);
void compare(processor* proc, instruction inst);
void putregister(processor* proc, byte index, byte value);
byte getregister(processor* proc, byte index);
void decode(processor* proc, ushort value);
