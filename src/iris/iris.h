/* UGH!!!!! */
#ifndef _IRIS_H
#define _IRIS_H
typedef unsigned char byte;
typedef unsigned char uchar;
typedef signed char schar;
typedef unsigned short ushort;
typedef unsigned short datum;

enum {
   RegisterCount = 8, 
   MemorySize = 65536, /* 8-bit cells */
   MajorOperationGroupCount = 8,
};

typedef struct core {
   byte gpr[RegisterCount];
   ushort code[MemorySize];
   byte data[MemorySize];
   ushort pc : 16;
   byte predicateregister : 1;
} core;

/* macros */
#define get_group(instruction) ((byte)(instruction & 0x7))

/* arithmetic */
/* C structure version 
 * DO NOT UNCOMMENT
 * struct arithmetic {
 *    byte op : 4;
 *    byte dest : 3;
 *    byte source0 : 3;
 *    byte source1 : 3;
 * };
 */
#define get_arithmetic_op(instruction) ((byte)((instruction & 0x78) >> 3))
#define get_arithmetic_dest(instruction) ((byte)((instruction & 0x380) >> 7))
#define get_arithmetic_source0(instruction) ((byte)((instruction & 0x1C00) >> 10))
#define get_arithmetic_source1(instruction) ((byte)((instruction & 0xE000) >> 13))

/* move */
/* C structure version
 * DO NOT UNCOMMENT
 * struct move {
 *    byte op : 2; 
 *    byte reg0 : 3;
 *    union {
 *       byte immediate : 8;
 *       byte reg1 : 3;
 *       struct {
 *          byte reg1 : 3;
 *          byte reg2 : 3;
 *          byte accessmode : 1;
 *       } addressmode;
 *    };
 * };
 */
#define get_move_op(instruction) ((byte)((instruction & 0x18) >> 3))
#define get_move_reg0(instruction) ((byte)((instruction & 0xE0) >> 5))
#define get_move_immediate(instruction) ((byte)((instruction) >> 8))
#define get_move_reg1(instruction) ((byte)((instruction & 0x700) >> 8))
#define get_move_reg2(instruction) ((byte)((instruction & 0x3800) >> 11))
#define get_move_accessmode(instruction) ((byte)((instruction & 0x4000) >> 14))

/* jump */
/* C structure version
 * DO NOT UNCOMMENT
 * struct jump {
 *    byte distance : 1;
 *    byte conditional : 2;
 *    byte immediatemode : 1;
 *    byte signedmode : 1;
 *    union {
 *       byte immediate : 8;
 *       union {
 *          byte reg0 : 3;
 *       } shortform;
 *       struct {
 *          byte reg0 : 3;
 *          byte reg1 : 3;
 *       } longtype;
 *       struct {
 *          byte reg0 : 3;
 *          byte reg1 : 3;
 *          byte reg1issigned : 1;
 *       } ifthenelsetype;
 *    };
 * };
 */
#define get_jump_distance(instruction) ((byte)((instruction & 0x8) >> 3))
#define get_jump_conditional(instruction) ((byte)((instruction & 0x30) >> 4))
#define get_jump_immediatemode(instruction) ((byte)((instruction & 0x40) >> 6))
#define get_jump_signedmode(instruction) ((byte)((instruction & 0x80) >> 7))
#define get_jump_immediate(instruction) ((byte)((instruction) >> 8))
#define get_jump_reg0(instruction) ((byte)((instruction & 0x700) >> 8))
#define get_jump_reg1(instruction) ((byte)((instruction & 0x3800) >> 11))
#define get_jump_reg1issigned(instruction) ((byte)((instruction & 0x4000) >> 14))
/* compare */
/* C structure version 
 * DO NOT UNCOMMENT
 * struct {
 *    byte op : 3;
 *    byte reg0 : 3;
 *    byte reg1 : 3;
 *    byte combinebits : 3; 
 * } compare;
 */
#define get_compare_op(instruction) ((byte)((instruction & 0x38) >> 3))
#define get_compare_reg0(instruction) ((byte)((instruction & 0x1C0) >> 6))
#define get_compare_reg1(instruction) ((byte)((instruction & 0xE00) >> 9))
#define get_compare_combinebits(instruction) ((byte)((instruction & 0x7000) >> 12))
/* systemcall */
/* C structure version
 * DO NOT UNCOMMENT
 * struct {
 *    byte operation : 7;
 *    byte reg0 : 3;
 *    byte reg1 : 3;
 * } systemcall;
 */
#define get_system_operation(instruction) ((byte)((instruction & 0x3F8) >> 3))
#define get_system_reg0(instruction) ((byte)((instruction & 0x1C00) >> 10))
#define get_system_reg1(instruction) ((byte)((instruction) >> 13))


/* Instructions Groups */
enum {
   InstructionGroupArithmetic = 0,
   InstructionGroupMove,
   InstructionGroupJump,
   InstructionGroupCompare,
   InstructionGroupSystem,
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
void arithmetic(core* proc, datum inst);
void move(core* proc, datum inst);
void jump(core* proc, datum inst);
void compare(core* proc, datum inst);
void put_register(core* proc, byte index, byte value);
byte get_register(core* proc, byte index);
void decode(core* proc, ushort value);
void error(char* message, int code);
void iris_system(core* proc, datum inst);
/* mnemonics */
const char* arithmetic_mnemonic(ushort insn);
const char* move_mnemonic(ushort insn);
const char* jump_mnemonic(ushort insn);
const char* compare_mnemonic(ushort insn);
/* unparse */
void unparse(char* unparsed, ushort insn);
void unparse_register(char* unparsed, byte index);
void unparse_arithmetic(char* unparsed, ushort insn);
void unparse_move(char* unparsed, ushort insn);
void unparse_jump(char* unparsed, ushort insn);
void unparse_normal_jump(char* unparsed, ushort insn);
void unparse_if_then_else(char* unparsed, ushort insn);
void unparse_compare(char* unparsed, ushort insn);
void unparse_bitstring(char* bits, ushort insn);
#endif 
