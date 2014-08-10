/* UGH!!!!! */
#ifndef _IRIS_H
#define _IRIS_H
typedef unsigned char byte;
typedef uint16_t word;
typedef uint32_t dword;
/* four bytes and now super flexible */
typedef union instruction {
   /* TODO: see if we should just use int32_t. A c99 feature */
   dword full;
   byte bytes[4];
   word words[2];
} instruction;

enum {
   RegisterCount = 256, 
   MemorySize = 65536, 
   MajorOperationGroupCount = 8,
   PredicateRegisterIndex = 255,
   StackPointerRegisterIndex = 254,
};

typedef struct iris_core {
   word gpr[RegisterCount];
   instruction code[MemorySize];
   word data[MemorySize];
   word stack[MemorySize];
   word pc;
   byte advancepc;
   byte terminateexecution;
} iris_core;


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
   ArithmeticOpAddImmediate,
   ArithmeticOpSubImmediate,
   ArithmeticOpMulImmediate,
   ArithmeticOpDivImmediate,
   ArithmeticOpRemImmediate,
   ArithmeticOpShiftLeftImmediate,
   ArithmeticOpShiftRightImmediate,
};
#define get_arithmetic_op(inst) (iris_decode_op(inst))
#define get_arithmetic_dest(inst) (iris_decode_register(inst, 1))
#define get_arithmetic_source0(inst) (iris_decode_register(inst, 2))
#define get_arithmetic_source1(inst) (iris_decode_register(inst, 3))

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
   MoveOpStoreMem, /* memcopy r? $imm */
   MoveOpStoreImm, /* memset r? $imm */
   /* uses an indirect register for the stack pointer */
   MoveOpPush, /* push r? */
   MoveOpPushImmediate, /* push.imm $imm */
   MoveOpPop, /* pop r? */
};
#define get_move_op(inst) (iris_decode_op(inst))
#define get_move_immediate(inst) (iris_decode_immediate(inst, 1))
#define get_move_reg0(inst) (iris_decode_register(inst, 1))
#define get_move_reg1(inst) (iris_decode_register(inst, 2))

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
#define get_jump_op(inst) (iris_decode_op(inst))
#define get_jump_immediate(inst) (iris_decode_immediate(inst, 1))
#define get_jump_reg0(inst) (iris_decode_register(inst, 1))
#define get_jump_reg1(inst) (iris_decode_register(inst, 2))
#define get_jump_reg2(inst) (iris_decode_register(inst, 3))

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
#define get_compare_op(inst) (iris_decode_op(inst))
#define get_compare_reg0(inst) (iris_decode_register(inst, 1))
#define get_compare_reg1(inst) (iris_decode_register(inst, 2))
#define get_compare_reg2(inst) (iris_decode_register(inst, 3))

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
 *    // Maintain <- flow order
 *    getimplicitregister_immediate {
 *       byte reg0 : 8;
 *       byte index : 8;
 *    };
 *    setimplicitregister_register {
 *       byte index : 8;
 *       byte reg0 : 8;
 *    };
 *    getimplicitregister_register {
 *       byte reg0 : 8;
 *       byte index : 8;
 *    };
 * } misc;
 */
enum {
   MiscOpSystemCall = 0,
};
/* implicit registers */

#define get_misc_op(inst) (iris_decode_op(inst))
#define get_misc_index(inst) (iris_decode_register(inst, 1))
#define get_misc_reg0(inst) (iris_decode_register(inst, 2))
#define get_misc_reg1(inst) (iris_decode_register(inst, 3))


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
   /* repl related */
   ErrorUnableToAllocateCore,
   ErrorTriedToSetAdvancePcToIllegalValue,
   ErrorTriedToSetShouldTerminateToIllegalValue,
};

/* system commands */
enum {
   SystemCommandTerminate = 0, /* Send a halt "signal" */
   SystemCommandGetC, 
   SystemCommandPutC,
   SystemCommandPanic,
};

void iris_rom_init(iris_core* proc);
void iris_arithmetic(iris_core* proc, instruction* inst);
void iris_move(iris_core* proc, instruction* inst);
void iris_jump(iris_core* proc, instruction* inst);
void iris_compare(iris_core* proc, instruction* inst);
void iris_put_register(iris_core* proc, byte index, word value);
word iris_get_register(iris_core* proc, byte index);
void iris_dispatch(iris_core* proc, instruction* value);
void iris_error(char* message, int code);
void iris_misc(iris_core* proc, instruction* inst);
/* mnemonics */
const char* iris_arithmetic_mnemonic(instruction* insn);
const char* iris_move_mnemonic(instruction* insn);
const char* iris_jump_mnemonic(instruction* insn);
const char* iris_compare_mnemonic(instruction* insn);
const char* iris_misc_mnemonic(instruction* insn);
/* unparse */
void iris_unparse(char* unparsed, instruction* insn);
void iris_unparse_register(char* unparsed, byte index);
void iris_unparse_arithmetic(char* unparsed, instruction* insn);
void iris_unparse_move(char* unparsed, instruction* insn);
void iris_unparse_jump(char* unparsed, instruction* insn);
void iris_unparse_normal_jump(char* unparsed, instruction* insn);
void iris_unparse_if_then_else(char* unparsed, instruction* insn);
void iris_unparse_compare(char* unparsed, instruction* insn);
void iris_unparse_bitstring(char* bits, instruction* insn);
void iris_unparse_misc(char* unparsed, instruction* insn);

/* encode */
byte iris_decode_group(instruction* inst);
void iris_encode_group(instruction* inst, byte group);
byte iris_decode_op(instruction* inst);
void iris_encode_op(instruction* inst, byte op);
byte iris_decode_register(instruction* inst, byte index);
void iris_encode_register(instruction* inst, byte index, byte value);
word iris_decode_immediate(instruction* inst, byte index);
void iris_encode_immediate(instruction* inst, byte index, word value);

#define get_group(inst) (iris_decode_group(inst))
#define get_op(inst) (iris_decode_op(inst))
#define get_reg0(inst) (iris_decode_register(inst, 1))
#define get_reg1(inst) (iris_decode_register(inst, 2))
#define get_reg2(inst) (iris_decode_register(inst, 3))
#define get_immediate(inst) (iris_decode_immediate(inst, 1))


/* libelectron interaction */
#define IRIS_CORE_DATA USER_ENVIRONMENT_DATA + 0
#define GetIrisCoreData(theEnv) ((iris_core*) GetEnvironmentData(theEnv, IRIS_CORE_DATA))
extern void iris_declarations(void* theEnv);
void iris_shutdown(core*);

#endif 
