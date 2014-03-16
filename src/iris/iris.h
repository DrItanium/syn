/* UGH!!!!! */
#ifndef _IRIS_H
#define _IRIS_H
typedef unsigned char byte;
typedef unsigned char uchar;
typedef signed char schar;
typedef unsigned short ushort;

enum {
   RegisterCount = 8, 
   MemorySize = 65536, /* 8-bit cells */
   MajorOperationGroupCount = 8,
};

typedef struct core {
   byte gpr[RegisterCount];
   byte memory[MemorySize];
   ushort pc : 16;
   byte predicateregister : 1;
} core;

typedef union datum {
   ushort value : 16;
   byte contents[2];
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
      byte signedmode : 1;
      union {
         byte immediate : 8;
         union {
            byte reg1 : 3;
         } shortform;
         struct {
            byte reg0 : 3;
            byte reg1 : 3;
         } longtype;
         struct {
            byte reg0 : 3;
            byte reg1issigned : 1;
            byte reg1 : 3;
         } ifthenelsetype;
      };
   } jump;
   struct {
      byte op : 3;
      byte reg0 : 3;
      byte reg1 : 3;
      byte combinebits : 3; /* nil, and, or, xor */
   } compare;
   struct {
      byte reg0 : 3;
      byte reg1 : 3;
      byte operation : 7;
   } systemcall;
} instruction;


/* Instructions Groups */
enum {
   InstructionGroupArithmetic = 0,
   InstructionGroupMove,
   InstructionGroupJump,
   InstructionGroupCompare,
   InstructionGroupSystem,
   InstructionGroupCompact,
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
   JumpOpIfThenElse,
};
enum {
   JumpOpIfThenElse_TrueFalse = 0,
   JumpOpIfThenElse_FalseTrue,
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

/* error codes */
enum {
   ErrorInvalidCombineBits = 1,
   ErrorInvalidCompareOperation = 2,
   ErrorInvalidIfThenElseInstructionType = 3,
   ErrorInvalidJumpConditionalType = 4,
   ErrorInvalidMoveOperationConditionalType = 5,
   ErrorInvalidArithmeticOperation = 6,
   ErrorGetRegisterOutOfRange = 7,
   ErrorPutRegisterOutOfRange = 8,
   ErrorInvalidInstructionGroupProvided = 9,
};
void arithmetic(core* proc, instruction inst);
void move(core* proc, instruction inst);
void jump(core* proc, instruction inst);
void compare(core* proc, instruction inst);
void putregister(core* proc, byte index, byte value);
byte getregister(core* proc, byte index);
void decode(core* proc, ushort value);
void error(char* message, int code);
void irissystem(core* proc, instruction inst);

#endif 
