/* UGH!!!!! */
#ifndef _IRIS_H
#define _IRIS_H
typedef unsigned char byte;
typedef unsigned char uchar;
typedef signed char schar;
typedef unsigned short ushort;
typedef unsigned short datum;
typedef unsigned int uint;
/* four bytes and now super flexible */
typedef union instruction {
   /* TODO: see if we should just use int32_t. A c99 feature */
   uint full;
   byte bytes[4];
   datum words[2];
} instruction;

enum {
   RegisterCount = 256, 
   /* used in cases where we don't have enough encoding space */
   ImpliedRegisterCount = 256,
   MemorySize = 65536, /* 8-bit cells */
   MajorOperationGroupCount = 8,
};

typedef struct core {
   datum gpr[RegisterCount];
   byte impliedregisters[ImpliedRegisterCount];
   instruction code[MemorySize];
   datum data[MemorySize];
   ushort pc : 16;
   byte advancepc;
   byte terminateexecution;
} core;


/* Instructions Groups */
enum {
   InstructionGroupArithmetic = 0,
   InstructionGroupMove,
   InstructionGroupJump,
   InstructionGroupCompare,
   InstructionGroupMisc,
};

/* arithmetic */
/* C structure version 
 * DO NOT UNCOMMENT
 * struct arithmetic {
 *    byte op : 5;
 *    byte dest : 8;
 *    byte source0 : 8;
 *    byte source1 : 8;
 * };
 */

/* ops */
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

/* move */
/* C structure version
 * DO NOT UNCOMMENT
 * struct move {
 *    byte op : 5;  //extend the op bits
 *    byte reg0 : 8;
 *    union {
 *       ushort immediate : 16;
 *       struct {
 *         byte reg1 : 8;
 *         byte reg2 : 8;
 *       };
 *    };
 * };
 */

enum {
   MoveOpMove = 0, /* move r? r? */
   MoveOpSwap, /* swap r? r? */
   MoveOpSwapRegAddr, /* swap.reg.addr r? r? */
   MoveOpSwapAddrAddr, /* swap.addr.addr r? r? */
   MoveOpSwapRegMem, /* swap.reg.mem r? $imm */
   MoveOpSwapAddrMem, /* swap.addr.mem r? $imm */
   MoveOpSet, /* set r? $imm */
   MoveOpLoad, /* load r? r? */
   MoveOpLoadMem, /* load.mem r? $imm */
   MoveOpStore, /* store r? r? */
   MoveOpStoreAddr, /* store.addr r? r? */
   MoveOpStoreMem, /* store.mem r? $imm */
   MoveOpStoreImm, /* store.imm r? $imm */
};

/* jump */
/* C structure version
 * DO NOT UNCOMMENT
 * struct jump {
 *    byte op : 5;
 *    modes {
 *       unconditional {
 *          immediate {
 *          ushort immediate : 16;  
 *          };
 *          immediate_link {
 *               byte linkregister : 8;
 *               ushort immediate : 16;
 *          };
 *          register {
 *             byte target : 8;
 *             // upper 16 bits unused
 *          };
 *          link_register {
 *             byte linkregister : 8;
 *             byte target : 8;
 *          }
 *       };
 *       conditional {
 *          immediate {
 *             byte predicate : 8;
 *             ushort value : 16;
 *          };
 *          immediate_link {
 *          // predicate is set in implied registers
 *             byte link_register : 8;
 *             ushort immediate : 16;
 *          };
 *          register {
 *             byte predicate : 8;
 *             byte target : 8;
 *          };
 *          register_link {
 *             byte predicate : 8;
 *             byte target : 8;
 *             byte linkregister : 8;
 *          };
 *       };
 *       ifthenelse {
 *          normal_pred_true {
 *               byte predicate : 8;
 *               byte ontrue : 8;
 *               byte onfalse : 8;
 *          };
 *          normal_pred_false {
 *               byte predicate : 8;
 *               byte ontrue : 8;
 *               byte onfalse : 8;
 *          };
 *          link_pred_true {
 *             // predicate is in set of implied registers
 *             byte linkregister : 8;
 *             byte ontrue : 8;
 *             byte onfalse : 8;
 *          };
 *          link_pred_false {
 *             // predicate is in set of implied registers
 *             byte linkregister : 8;
 *             byte ontrue : 8;
 *             byte onfalse : 8;
 *          }
 *       };
 *    };
 * };
 */
enum {
   JumpOpUnconditionalImmediate = 0,
   JumpOpUnconditionalImmediateLink,
   JumpOpUnconditionalRegister,
   JumpOpUnconditionalRegisterLink,
   JumpOpConditionalImmediate,
   JumpOpConditionalImmediateLink,
   JumpOpConditionalRegister,
   JumpOpConditionalRegisterLink,
   JumpOpIfThenElseNormalPredTrue,
   JumpOpIfThenElseNormalPredFalse,
   JumpOpIfThenElseLinkPredTrue,
   JumpOpIfThenElseLinkPredFalse,
};

/* compare */
/* C structure version 
 * DO NOT UNCOMMENT
 * struct {
 *    byte op : 5;
 *    byte dest : 8;
 *    byte reg0 : 8;
 *    byte reg1 : 8;
 * } compare;
 */
enum {
   CompareOpEq = 0,
   CompareOpEqAnd,
   CompareOpEqOr,
   CompareOpEqXor,
   CompareOpNeq,
   CompareOpNeqAnd,
   CompareOpNeqOr,
   CompareOpNeqXor,
   CompareOpLessThan,
   CompareOpLessThanAnd,
   CompareOpLessThanOr,
   CompareOpLessThanXor,
   CompareOpGreaterThan,
   CompareOpGreaterThanAnd,
   CompareOpGreaterThanOr,
   CompareOpGreaterThanXor,
   CompareOpLessThanOrEqualTo,
   CompareOpLessThanOrEqualToAnd,
   CompareOpLessThanOrEqualToOr,
   CompareOpLessThanOrEqualToXor,
   CompareOpGreaterThanOrEqualTo,
   CompareOpGreaterThanOrEqualToAnd,
   CompareOpGreaterThanOrEqualToOr,
   CompareOpGreaterThanOrEqualToXor,
};

/* misc */
/* C structure version
 * DO NOT UNCOMMENT
 * struct {
 *    byte op : 5;
 *    systemcall {
 *       byte operation : 8;
 *       byte reg0 : 8;
 *       byte reg1 : 8;
 *    };
 *    setimplicitregister_immediate {
 *       byte index : 8;
 *       byte reg0 : 8;
 *    };
 *    getimplicitregister_immediate {
 *       byte index : 8;
 *       byte reg0 : 8;
 *    };
 *    setimplicitregister_register {
 *       byte index : 8;
 *       byte reg0 : 8;
 *    };
 *    getimplicitregister_register {
 *       byte index : 8;
 *       byte reg0 : 8;
 *    };
 * } misc;
 */
enum {
   MiscOpSystemCall = 0,
   MiscOpSetImplicitRegisterImmediate,
   MiscOpSetImplicitRegisterIndirect,
   MiscOpGetImplicitRegisterImmediate,
   MiscOpGetImplicitRegisterIndirect,
};
/* implicit registers */
enum {
   ImplicitRegisterPredicate = 0,
};


/* error codes */
enum {
   ErrorNone = 0,
   ErrorInvalidCombineBits,
   ErrorInvalidCompareOperation ,
   ErrorInvalidIfThenElseInstructionType,
   ErrorInvalidJumpConditionalType,
   ErrorInvalidMoveOperationConditionalType,
   ErrorInvalidArithmeticOperation,
   ErrorGetRegisterOutOfRange,
   ErrorPutRegisterOutOfRange,
   ErrorInvalidInstructionGroupProvided,
   ErrorInvalidSystemCommand,
};

/* system commands */
enum {
   SystemCommandTerminate = 0, /* Send a halt "signal" */
   SystemCommandGetC, 
   SystemCommandPutC,
   SystemCommandPanic,
};


void arithmetic(core* proc, uint inst);
void move(core* proc, uint inst);
void jump(core* proc, uint inst);
void compare(core* proc, uint inst);
void put_register(core* proc, byte index, byte value);
byte get_register(core* proc, byte index);
void decode(core* proc, uint value);
void error(char* message, int code);
void iris_system(core* proc, uint inst);
/* mnemonics */
const char* arithmetic_mnemonic(uint insn);
const char* move_mnemonic(uint insn);
const char* jump_mnemonic(uint insn);
const char* compare_mnemonic(uint insn);
const char* misc_mnemonic(uint insn);
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
/* encode */
byte decode_group(instruction* inst);
void encode_group(instruction* inst, byte group);
byte decode_op(instruction* inst);
void encode_op(instruction* inst, byte op);
byte decode_register(instruction* inst, byte index);
void encode_register(instruction* inst, byte index, byte value);
datum decode_immediate(instruction* inst, byte index);
void encode_immediate(instruction* inst, byte index, datum value);
#endif 
