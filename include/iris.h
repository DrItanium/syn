/* UGH!!!!! */
#ifndef _IRIS_H
#define _IRIS_H
#include <stdint.h>
#include <stdbool.h>
typedef unsigned char byte;
typedef uint32_t hword;
typedef uint64_t word;
/* four bytes and now super flexible */
typedef hword instruction;

enum {
   RegisterCount = 256, 
   MemorySize = 65536, 
   MajorOperationGroupCount = 8,
   PredicateRegisterIndex = 255,
   StackPointerRegisterIndex = 254,
};

typedef struct {
	hword memorysize;
	hword codestart, codesize;
	hword datastart, datasize;
} iris_memory_map;
typedef struct iris_core {
   word gpr[RegisterCount];
   byte* memory;
   instruction* code;
   byte* data;
   hword pc;
   bool advancepc,
		terminateexecution;
   hword datasize,
		 codesize,
		 memorysize;
} iris_core;


/* Instructions Groups */
enum {
   InstructionGroupArithmetic = 0,
   InstructionGroupMove,
   InstructionGroupJump,
   InstructionGroupCompare,
   InstructionGroupLoadStore,
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
#define get_arithmetic_immediate(inst) (get_arithmetic_source1(inst))

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
// base operation (bit 0 and 1)
// Move Form (destination is always register)
// 	bits 2-3 are width bits
// 		00: 64-bit chunks (1)
// 		01: 32-bit chunks (2)
// 		10: 16-bit chunks (3)
// 		11: 8-bit chunks (4)
//
// 	register slot 3 is used in conjunction with other bit patterns to perform
// 	more complex things like byte slicing, etc
//
// 		This does lead to some interesting equivalents (0xFF = 0x0F = 0x3 = 0x1)
// 		With the values being 8-bit, 16-bit, 32-bit, and 64-bit chunks
// 		respectively
//
// 		It also means that a mask set to zero is the equivalent to a nop 
//
// Swap Form (destination and sources are always registers)
// 	bits 2-3 are width bits
// 		00: 64-bit chunks (1 bit mask)
// 		01: 32-bit chunks (2 bit mask)
// 		10: 16-bit chunks (4 bit mask)
// 		11: 8-bit chunks (8 bit mask)
//
// 		This does lead to some interesting equivalents (0xFF = 0x0F = 0x3 = 0x1)
// 		With the values being 8-bit, 16-bit, 32-bit, and 64-bit chunks
// 		respectively
//
// 		It also means that a mask set to zero is the equivalent to a nop 
//
// 	register slot 3 is used in conjunction with other bit patterns to perform
// 	more complex things like byte slicing, etc
//
// Set Form 
// bits 2-3 are position bits (or where to place the 16-bit value)
// 	00: 0-15
// 	01: 16-31
// 	10: 32-47
// 	11: 48-63
// 
// Slice Form (slice out bits from an 8 bit segment)
// bits 2,3,4 are position bits (in combination with the bit mask in register slot 3)
// 	000: 0-7
// 	001: 8-15
// 	010: 16-23
// 	011: 24-31
// 	100: 32-39
// 	101: 40-47
// 	110: 48-55
// 	111: 56-63
enum {
	MoveOpMove = 0,
	MoveOpSwap,
	MoveOpSet,
	MoveOpSlice,
};
enum {
	MoveOpSet_Bits_0_15 = 0,
	MoveOpSet_Bits_16_31,
	MoveOpSet_Bits_32_47,
	MoveOpSet_Bits_48_63,
};
enum {
	MoveOp_Form_64bit_chunks = 0,
	MoveOp_Form_32bit_chunks,
	MoveOp_Form_16bit_chunks,
	MoveOp_Form_8bit_chunks, 
};
enum {
	SliceOp_0to7 = 0,
	SliceOp_8to15,
	SliceOp_16to23,
	SliceOp_24to31,
	SliceOp_32to39,
	SliceOp_40to47,
	SliceOp_48to55,
	SliceOp_56to63,
};
//enum {
//   MoveOpMove = 0, /* move r? r? */
//   MoveOpSwap, /* swap r? r? */
//   MoveOpSet, /* set r? $imm */ // 16-bit immediate
//};
#define get_move_op(inst) (iris_decode_op(inst) & 0x3)
#define get_move_position(inst) ((iris_decode_op(inst) >> 2))
#define get_move_immediate(inst) (iris_decode_immediate(inst))
#define get_move_reg0(inst) (iris_decode_register(inst, 1))
#define get_move_reg1(inst) (iris_decode_register(inst, 2))
#define get_move_mask(inst) (iris_decode_register(inst, 3))

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
// jump base form (bits 0,1 of the op)
// immediate? (bit 2)
// link? (bit 3)
// true/false (bit 4)
enum {
	JumpOpUnconditional = 0,
	JumpOpConditional,
	JumpOpIfThenElse,
	JumpOpUnused,
};
#define get_jump_form(inst) (iris_decode_op(inst) & 0x3)
#define get_jump_immediate_flag(inst) ((iris_decode_op(inst) >> 2) & 0x1)
#define get_jump_link_flag(inst) ((iris_decode_op(inst) >> 3) & 0x1)
#define get_jump_true_false_flag(inst) ((iris_decode_op(inst) >> 4) & 0x1)
//enum {
//   JumpOpUnconditionalImmediate = 0,
//   JumpOpUnconditionalImmediateLink,
//   JumpOpUnconditionalRegister,
//   JumpOpUnconditionalRegisterLink,
//   JumpOpConditionalTrueImmediate,
//   JumpOpConditionalTrueImmediateLink,
//   JumpOpConditionalTrueRegister,
//   JumpOpConditionalTrueRegisterLink,
//   JumpOpConditionalFalseImmediate,
//   JumpOpConditionalFalseImmediateLink,
//   JumpOpConditionalFalseRegister,
//   JumpOpConditionalFalseRegisterLink,
//   JumpOpIfThenElseNormalPredTrue,
//   JumpOpIfThenElseNormalPredFalse,
//   JumpOpIfThenElseLinkPredTrue,
//   JumpOpIfThenElseLinkPredFalse,
//};
#define get_jump_immediate(inst) (iris_decode_immediate(inst))
#define get_jump_reg0(inst) (iris_decode_register(inst, 1))
#define get_jump_reg1(inst) (iris_decode_register(inst, 2))
#define get_jump_reg2(inst) (iris_decode_register(inst, 3))

/* compare */
/* C structure version 
 * DO NOT UNCOMMENT
 * struct {
 * 	  byte compare : 3;
 *    byte combine : 2;
 *    byte dest : 8;
 *    byte reg0 : 8;
 *    byte reg1 : 8;
 * } compare;
 */
enum {
	CombineOpSet = 0,
	CombineOpAnd,
	CombineOpOr,
	CombineOpXor,
};
enum {
	CompareOpEq = 0,
	CompareOpNeq,
	CompareOpLessThan,
	CompareOpGreaterThan,
	CompareOpLessThanOrEqualTo,
	CompareOpGreaterThanOrEqualTo,
};
#define get_compare_op(inst) (iris_decode_op(inst) & 0x7)
#define get_combine_op(inst) ((iris_decode_op(inst) >> 2) & 0x3)
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
   ErrorInvalidCombineOperation,
   ErrorInvalidSystemCommand,
   ErrorInvalidLoadStoreOperation,
   ErrorIllogicalSwapOperation,
   ErrorIllegalSetBits,
   ErrorIllegalMoveBits,
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

// Load Store group
// class (1 bit: position 0)
// merge (1 bit: position 1)
// register slot 3 is used as a bit mask to describe byte layout

enum {
	LoadStoreOp_Load = 0, /* load <merge> width r? r? mask */
	LoadStoreOp_Store, /* store <merge> width r? r? mask */
};

#define get_load_store_op(inst) (iris_decode_op(inst) & 0x1)
#define get_load_store_merge_flag(inst) ((iris_decode_op(inst) & 0x2) >> 1)
#define get_load_store_reg0(inst) (iris_decode_register(inst, 1))
#define get_load_store_reg1(inst) (iris_decode_register(inst, 2))
#define get_load_store_mask(inst) (iris_decode_register(inst, 3))

//   MoveOpLoadMem, /* load.mem r? $imm */
//   MoveOpStore, /* store r? r? */
//   MoveOpStoreAddr, /* store.addr r? r? */
//   MoveOpStoreMem, /* memcopy r? $imm */
//   MoveOpStoreImm, /* memset r? $imm */
//   MoveOpStore32, /* store32 r? r? */
//   MoveOpStore16, /* store16 r? r? */
//   MoveOpStore8, /* store8 r? r? */
//   MoveOpLoad32, /* load32 r? r? */
//   MoveOpLoad16, /* load16 r? r? */
//   MoveOpLoad8, /* load8 r? r? */
//   /* uses an indirect register for the stack pointer */
//   MoveOpPush, /* push r? */
//   MoveOpPushImmediate, /* push.imm $imm */
//   MoveOpPop, /* pop r? */
//};

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
void iris_load_store(iris_core* proc, instruction* inst);
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
word iris_decode_immediate(instruction* inst);
void iris_encode_immediate(instruction* inst, byte index, word value);

#define get_group(inst) (iris_decode_group(inst))
#define get_op(inst) (iris_decode_op(inst))
#define get_reg0(inst) (iris_decode_register(inst, 1))
#define get_reg1(inst) (iris_decode_register(inst, 2))
#define get_reg2(inst) (iris_decode_register(inst, 3))
#define get_immediate(inst) (iris_decode_immediate(inst))

void iris_shutdown(iris_core*);
void iris_new_core(iris_core* proc, iris_memory_map* memmap);

#endif 
