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
   ushort pc;
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
#define get_arithmetic_op(inst) (decode_op(inst))
#define get_arithmetic_dest(inst) (decode_register(inst, 1))
#define get_arithmetic_source0(inst) (decode_register(inst, 2))
#define get_arithmetic_source1(inst) (decode_register(inst, 3))

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
#define get_move_op(inst) (decode_op(inst))
#define get_move_immediate(inst) (decode_immediate(inst, 1))
#define get_move_reg0(inst) (decode_register(inst, 1))
#define get_move_reg1(inst) (decode_register(inst, 2))

/* jump */
/* C structure version
 * DO NOT UNCOMMENT
 * struct jump {
 *    byte op : 5;
 *    modes {
 *       unconditional {
 *          immediate {
 *          byte unused : 8; //for alignment purposes
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
 *       conditional_true {
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
 *       conditional_false {
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
   JumpOpConditionalTrueImmediate,
   JumpOpConditionalTrueImmediateLink,
   JumpOpConditionalTrueRegister,
   JumpOpConditionalTrueRegisterLink,
   JumpOpConditionalFalseImmediate,
   JumpOpConditionalFalseImmediateLink,
   JumpOpConditionalFalseRegister,
   JumpOpConditionalFalseRegisterLink,
   JumpOpIfThenElseNormalPredTrue,
   JumpOpIfThenElseNormalPredFalse,
   JumpOpIfThenElseLinkPredTrue,
   JumpOpIfThenElseLinkPredFalse,
};
#define get_jump_op(inst) (decode_op(inst))
#define get_jump_immediate(inst) (decode_immediate(inst, 1))
#define get_jump_reg0(inst) (decode_register(inst, 1))
#define get_jump_reg1(inst) (decode_register(inst, 2))
#define get_jump_reg2(inst) (decode_register(inst, 3))

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
#define get_compare_op(inst) (decode_op(inst))
#define get_compare_reg0(inst) (decode_register(inst, 1))
#define get_compare_reg1(inst) (decode_register(inst, 2))
#define get_compare_reg2(inst) (decode_register(inst, 3))

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
#define get_misc_op(inst) (decode_op(inst))
#define get_misc_index(inst) (decode_register(inst, 1))
#define get_misc_reg0(inst) (decode_register(inst, 2))
#define get_misc_reg1(inst) (decode_register(inst, 3))


/* error codes */
enum {
   ErrorNone = 0,
   ErrorGetRegisterOutOfRange,
   ErrorPutRegisterOutOfRange,
   ErrorInvalidInstructionGroupProvided,
   ErrorInvalidArithmeticOperation,
   ErrorInvalidMoveOperation,
   ErrorInvalidJumpOperation,
   ErrorInvalidCompareOperation,
   ErrorInvalidMiscOperation,
   ErrorInvalidSystemCommand,
};

/* system commands */
enum {
   SystemCommandTerminate = 0, /* Send a halt "signal" */
   SystemCommandGetC, 
   SystemCommandPutC,
   SystemCommandPanic,
};


void arithmetic(core* proc, instruction* inst);
void move(core* proc, instruction* inst);
void jump(core* proc, instruction* inst);
void compare(core* proc, instruction* inst);
void put_register(core* proc, byte index, datum value);
datum get_register(core* proc, byte index);
void decode(core* proc, instruction* value);
void error(char* message, int code);
void misc(core* proc, instruction* inst);
/* mnemonics */
const char* arithmetic_mnemonic(instruction* insn);
const char* move_mnemonic(instruction* insn);
const char* jump_mnemonic(instruction* insn);
const char* compare_mnemonic(instruction* insn);
const char* misc_mnemonic(instruction* insn);
/* unparse */
void unparse(char* unparsed, instruction* insn);
void unparse_register(char* unparsed, byte index);
void unparse_arithmetic(char* unparsed, instruction* insn);
void unparse_move(char* unparsed, instruction* insn);
void unparse_jump(char* unparsed, instruction* insn);
void unparse_normal_jump(char* unparsed, instruction* insn);
void unparse_if_then_else(char* unparsed, instruction* insn);
void unparse_compare(char* unparsed, instruction* insn);
void unparse_bitstring(char* bits, instruction* insn);
void unparse_misc(char* unparsed, instruction* insn);
/* encode */
byte decode_group(instruction* inst);
#define get_group(inst) (decode_group(inst))
void encode_group(instruction* inst, byte group);
byte decode_op(instruction* inst);
void encode_op(instruction* inst, byte op);
byte decode_register(instruction* inst, byte index);
void encode_register(instruction* inst, byte index, byte value);
datum decode_immediate(instruction* inst, byte index);
void encode_immediate(instruction* inst, byte index, datum value);
#endif 
