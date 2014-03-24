#include <string.h>
#include "iris.h"
#include "mnemonic.h"

const char* arithmetic_mnemonic(ushort insn) {
   switch(getarithmeticop(insn)) {
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
   switch(getmoveop(insn)) {
      case MoveOpRegToReg:       return "move";
      case MoveOpImmediateToReg: return "move";
      case MoveOpRegToAddress:
         switch(getmoveaccessmode(insn)) {
            case AccessModeMoveOpLoad:  return "load";
            case AccessModeMoveOpStore: return "store";
         }
      default:                   return "UNKNOWN_MOVE";
   }
}

const char* jump_mnemonic(ushort insn) {
   switch(getjumpconditional(insn)) {
      case JumpOpUnconditional: return "goto";
      case JumpOpIfTrue:        return "if1";
      case JumpOpIfFalse:       return "if0";
      case JumpOpIfThenElse:
         switch(getjumpimmediatemode(insn)) {
            case JumpOpIfThenElse_TrueFalse: return "if1";
            case JumpOpIfThenElse_FalseTrue: return "if0";
         }
      default:                  return "UNKNOWN_JUMP";
   }
}

const char* compare_mnemonic(ushort insn) {
   switch(getcomparecombinebits(insn)) {
      case CombineBitsOpNil:
         switch(getcompareop(insn)) {
            case CompareOpEq:                   return "eq";
            case CompareOpNeq:                  return "ne";
            case CompareOpLessThan:             return "lt";
            case CompareOpGreaterThan:          return "gt";
            case CompareOpLessThanOrEqualTo:    return "le";
            case CompareOpGreaterThanOrEqualTo: return "ge";
            default:                            return "UNKNOWN_COMPARE";
         }
      case CombineBitsOpAnd:
         switch(getcompareop(insn)) {
            case CompareOpEq:                   return "and_eq";
            case CompareOpNeq:                  return "and_ne";
            case CompareOpLessThan:             return "and_lt";
            case CompareOpGreaterThan:          return "and_gt";
            case CompareOpLessThanOrEqualTo:    return "and_le";
            case CompareOpGreaterThanOrEqualTo: return "and_ge";
            default:                            return "UNKNOWN_COMPARE";
         }
      case CombineBitsOpOr:
         switch(getcompareop(insn)) {
            case CompareOpEq:                   return "or_eq";
            case CompareOpNeq:                  return "or_ne";
            case CompareOpLessThan:             return "or_lt";
            case CompareOpGreaterThan:          return "or_gt";
            case CompareOpLessThanOrEqualTo:    return "or_le";
            case CompareOpGreaterThanOrEqualTo: return "or_ge";
            default:                            return "UNKNOWN_COMPARE";
         }
      case CombineBitsOpXor:
         switch(getcompareop(insn)) {
            case CompareOpEq:                   return "xor_eq";
            case CompareOpNeq:                  return "xor_ne";
            case CompareOpLessThan:             return "xor_lt";
            case CompareOpGreaterThan:          return "xor_gt";
            case CompareOpLessThanOrEqualTo:    return "xor_le";
            case CompareOpGreaterThanOrEqualTo: return "xor_ge";
            default:                            return "UNKNOWN_COMPARE";
         }
      default:                                  return "UNKNOWN_COMPARE";
   }
}

/* vim: set expandtab tabstop=3 shiftwidth=3: */
