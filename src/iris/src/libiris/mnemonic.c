#include <string.h>
#include "iris.h"

const char* iris_arithmetic_mnemonic(instruction* insn) {
   switch(get_arithmetic_op(insn)) {
      case ArithmeticOpAdd:                     return "add";
      case ArithmeticOpSub:                     return "sub";
      case ArithmeticOpMul:                     return "mul";
      case ArithmeticOpDiv:                     return "div";
      case ArithmeticOpRem:                     return "rem";
      case ArithmeticOpShiftLeft:               return "shl";
      case ArithmeticOpShiftRight:              return "shr";
      case ArithmeticOpBinaryAnd:               return "and";
      case ArithmeticOpBinaryOr:                return "or";
      case ArithmeticOpBinaryNot:               return "not";
      case ArithmeticOpBinaryXor:               return "xor";
      case ArithmeticOpAddImmediate:            return "addi";
      case ArithmeticOpSubImmediate:            return "subi";
      case ArithmeticOpMulImmediate:            return "muli";
      case ArithmeticOpDivImmediate:            return "divi";
      case ArithmeticOpRemImmediate:            return "remi";
      case ArithmeticOpShiftLeftImmediate:      return "shli";
      case ArithmeticOpShiftRightImmediate:     return "shri";
      default:                                  return "UNASSIGNED_ARITHMETIC";
   }
}

const char* iris_move_mnemonic(instruction* insn) {
   switch(get_move_op(insn)) {
      case MoveOpMove:           return "move"; /* move r? r? */
      case MoveOpSwap:           return "swap"; /* swap r? r? */
      case MoveOpSwapRegAddr:    return "swap.reg.addr"; /* swap.reg.addr r? r? */
      case MoveOpSwapAddrAddr:   return "swap.addr.addr"; /* swap.addr.addr r? r? */
      case MoveOpSwapRegMem:     return "swap.reg.mem"; /* swap.reg.mem r? $imm */
      case MoveOpSwapAddrMem:    return "swap.addr.mem"; /* swap.addr.mem r? $imm */
      case MoveOpSet:            return "set"; /* set r? $imm */
      case MoveOpLoad:           return "load"; /* load r? r? */
      case MoveOpLoadMem:        return "load.mem"; /* load.mem r? $imm */
      case MoveOpStore:          return "store"; /* store r? r? */
      case MoveOpStoreAddr:      return "store.addr"; /* store.addr r? r? */
      case MoveOpStoreMem:       return "memcopy"; /* memcopy r? $imm */
      case MoveOpStoreImm:       return "memset"; /* memset r? $imm */
      case MoveOpPush:           return "push"; /* push r? */
      case MoveOpPushImmediate:  return "push.imm";
      case MoveOpPop:            return "pop";
      default:                   return "UNKNOWN_MOVE";
   }
}

const char* iris_jump_mnemonic(instruction* insn) {
   switch(get_jump_op(insn)) {
      case JumpOpUnconditionalImmediate:           return "goto";
      case JumpOpUnconditionalImmediateLink:       return "goto.link";
      case JumpOpUnconditionalRegister:            return "jump";
      case JumpOpUnconditionalRegisterLink:        return "jump.link";
      case JumpOpConditionalTrueImmediate:             return "goto.if1";
      case JumpOpConditionalTrueImmediateLink:         return "goto.if1.link";
      case JumpOpConditionalTrueRegister:              return "jump.if1";
      case JumpOpConditionalTrueRegisterLink:          return "jump.if1.link";
      case JumpOpConditionalFalseImmediate:             return "goto.if0";
      case JumpOpConditionalFalseImmediateLink:         return "goto.if0.link";
      case JumpOpConditionalFalseRegister:              return "jump.if0";
      case JumpOpConditionalFalseRegisterLink:          return "jump.if0.link";
      case JumpOpIfThenElseNormalPredTrue:         return "if1";
      case JumpOpIfThenElseNormalPredFalse:        return "if0";
      case JumpOpIfThenElseLinkPredTrue:           return "if1.link";
      case JumpOpIfThenElseLinkPredFalse:          return "if0.link";
      default:                                     return "UNKNOWN_JUMP";
   }
}

const char* iris_compare_mnemonic(instruction* insn) {
   switch(get_compare_op(insn)) {
      case CompareOpEq:                         return "eq";
      case CompareOpEqAnd:                      return "and.eq";
      case CompareOpEqOr:                       return "or.eq";
      case CompareOpEqXor:                      return "xor.eq";
      case CompareOpNeq:                        return "ne";
      case CompareOpNeqAnd:                     return "and.ne";
      case CompareOpNeqOr:                      return "or.ne";
      case CompareOpNeqXor:                     return "xor.ne";
      case CompareOpLessThan:                   return "lt";
      case CompareOpLessThanAnd:                return "and.lt";
      case CompareOpLessThanOr:                 return "or.lt";
      case CompareOpLessThanXor:                return "xor.lt";
      case CompareOpGreaterThan:                return "gt";
      case CompareOpGreaterThanAnd:             return "and.gt";
      case CompareOpGreaterThanOr:              return "or.gt";
      case CompareOpGreaterThanXor:             return "xor.gt";
      case CompareOpLessThanOrEqualTo:          return "le";
      case CompareOpLessThanOrEqualToAnd:       return "and.le";
      case CompareOpLessThanOrEqualToOr:        return "or.le";
      case CompareOpLessThanOrEqualToXor:       return "xor.le";
      case CompareOpGreaterThanOrEqualTo:       return "ge";
      case CompareOpGreaterThanOrEqualToAnd:    return "and.ge";
      case CompareOpGreaterThanOrEqualToOr:     return "or.ge";
      case CompareOpGreaterThanOrEqualToXor:    return "xor.ge";
      default:                                  return "UNKNOWN_COMPARE";
   }
}

const char* iris_misc_mnemonic(instruction* insn) {
   switch(get_misc_op(insn)) {
      case MiscOpSystemCall:                       return "system";
      case MiscOpSetImplicitRegisterImmediate:     return "implicit.register.set";
      case MiscOpSetImplicitRegisterIndirect:      return "implicit.register.indirect.set";
      case MiscOpGetImplicitRegisterImmediate:     return "implicit.register.get";
      case MiscOpGetImplicitRegisterIndirect:      return "implicit.register.indirect.get";
      default:                                     return "UNKNOWN_MISC";
   }

}

/* vim: set expandtab tabstop=3 shiftwidth=3: */
