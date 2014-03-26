#include <string.h>
#include "iris.h"

const char* arithmetic_mnemonic(ushort insn) {
   switch(get_arithmetic_op(insn)) {
      case ArithmeticOpAdd:        return "add";
      case ArithmeticOpSub:        return "sub";
      case ArithmeticOpMul:        return "mul";
      case ArithmeticOpDiv:        return "div";
      case ArithmeticOpRem:        return "rem";
      case ArithmeticOpShiftLeft:  return "shl";
      case ArithmeticOpShiftRight: return "shr";
      case ArithmeticOpBinaryAnd:  return "and";
      case ArithmeticOpBinaryOr:   return "or";
      case ArithmeticOpBinaryNot:  return "not";
      case ArithmeticOpBinaryXor:  return "xor";
      default:                     return "UNASSIGNED_ARITHMETIC";
   }
}

const char* move_mnemonic(ushort insn) {
   switch(get_move_op(insn)) {
      case MoveOpRegToReg:       return "load";
      case MoveOpImmediateToReg: return "load";
      case MoveOpRegToAddress:
         switch(get_move_accessmode(insn)) {
            case AccessModeMoveOpLoad:  return "load";
            case AccessModeMoveOpStore: return "store";
         }
      default:                   return "UNKNOWN_MOVE";
   }
}

const char* jump_mnemonic(ushort insn) {
   switch(get_jump_conditional(insn)) {
      case JumpOpUnconditional: return "goto";
      case JumpOpIfTrue:        return "if1";
      case JumpOpIfFalse:       return "if0";
      case JumpOpIfThenElse:
         switch(get_jump_immediatemode(insn)) {
            case JumpOpIfThenElse_TrueFalse: return "if1";
            case JumpOpIfThenElse_FalseTrue: return "if0";
         }
      default:                  return "UNKNOWN_JUMP";
   }
}

const char* compare_mnemonic(ushort insn) {
   switch(get_compare_combinebits(insn)) {
      case CombineBitsOpNil:
         switch(get_compare_op(insn)) {
            case CompareOpEq:                   return "eq";
            case CompareOpNeq:                  return "ne";
            case CompareOpLessThan:             return "lt";
            case CompareOpGreaterThan:          return "gt";
            case CompareOpLessThanOrEqualTo:    return "le";
            case CompareOpGreaterThanOrEqualTo: return "ge";
            default:                            return "UNKNOWN_COMPARE";
         }
      case CombineBitsOpAnd:
         switch(get_compare_op(insn)) {
            case CompareOpEq:                   return "and.eq";
            case CompareOpNeq:                  return "and.ne";
            case CompareOpLessThan:             return "and.lt";
            case CompareOpGreaterThan:          return "and.gt";
            case CompareOpLessThanOrEqualTo:    return "and.le";
            case CompareOpGreaterThanOrEqualTo: return "and.ge";
            default:                            return "UNKNOWN_COMPARE";
         }
      case CombineBitsOpOr:
         switch(get_compare_op(insn)) {
            case CompareOpEq:                   return "or.eq";
            case CompareOpNeq:                  return "or.ne";
            case CompareOpLessThan:             return "or.lt";
            case CompareOpGreaterThan:          return "or.gt";
            case CompareOpLessThanOrEqualTo:    return "or.le";
            case CompareOpGreaterThanOrEqualTo: return "or.ge";
            default:                            return "UNKNOWN_COMPARE";
         }
      case CombineBitsOpXor:
         switch(get_compare_op(insn)) {
            case CompareOpEq:                   return "xor.eq";
            case CompareOpNeq:                  return "xor.ne";
            case CompareOpLessThan:             return "xor.lt";
            case CompareOpGreaterThan:          return "xor.gt";
            case CompareOpLessThanOrEqualTo:    return "xor.le";
            case CompareOpGreaterThanOrEqualTo: return "xor.ge";
            default:                            return "UNKNOWN_COMPARE";
         }
      default:                                  return "UNKNOWN_COMPARE";
   }
}

/* vim: set expandtab tabstop=3 shiftwidth=3: */
