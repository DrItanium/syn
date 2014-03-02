typedef unsigned char byte;
enum {
   RegisterCount = 4,
#ifndef MemorySize 
   MemorySize = 256, /* 8-bit cells */
#endif
};

typedef struct processor {
   byte gpr[RegisterCount];
   byte memory[MemorySize];
   byte predicateregister : 1;
} processor;

typedef union instruction {
   ushort wholedata : 16;
   struct {
      byte opgroup : 3;
      union {
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
               byte immediate;
               byte reg1 : 3;
               struct {
                  byte reg1 : 3;
                  byte tagbits : 5;
               } chainmode;
            };
         } move;
         struct {
            byte op : 2;
            byte immediateform : 1;
            union {
               byte immediate;
               byte reg1 : 3;
            };
         } jump;
         struct {
            byte op : 3;
            byte reg0 : 3;
            byte reg1 : 3;
            byte chaintags : 4;
         } compare;
      };
   };
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
};
enum {
   MoveOpRegToReg = 0,
   MoveOpImmediateToReg,
   MoveOpRegToImmediate,
   MoveOpChainMode,
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
   CompareOpChainMode,
};
void arithmetic(processor* proc, instruction inst);
void move(processor* proc, instruction inst);
void jump(processor* proc, instruction inst);
void compare(processor* proc, instruction inst);
