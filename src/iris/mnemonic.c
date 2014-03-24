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
      case JumpOpIfTrue:        return "iftrue";
      case JumpOpIfFalse:       return "iffalse";
      case JumpOpIfThenElse:    return "if";
      default:                  return "UNKNOWN_JUMP";
   }
}

const char* compare_mnemonic(ushort insn) {
   switch(getcomparecombinebits(insn)) {
      case CombineBitsOpNil:
         switch(getcompareop(insn)) {
            case CompareOpEq:                   return "eq"; break;
            case CompareOpNeq:                  return "ne"; break;
            case CompareOpLessThan:             return "lt"; break;
            case CompareOpGreaterThan:          return "gt"; break;
            case CompareOpLessThanOrEqualTo:    return "le"; break;
            case CompareOpGreaterThanOrEqualTo: return "ge"; break;
            default:                            return "UNKNOWN_COMPARE";
         }
      case CombineBitsOpAnd:
         switch(getcompareop(insn)) {
            case CompareOpEq:                   return "and_eq"; break;
            case CompareOpNeq:                  return "and_ne"; break;
            case CompareOpLessThan:             return "and_lt"; break;
            case CompareOpGreaterThan:          return "and_gt"; break;
            case CompareOpLessThanOrEqualTo:    return "and_le"; break;
            case CompareOpGreaterThanOrEqualTo: return "and_ge"; break;
            default:                            return "UNKNOWN_COMPARE";
         }
      case CombineBitsOpOr:
         switch(getcompareop(insn)) {
            case CompareOpEq:                   return "or_eq"; break;
            case CompareOpNeq:                  return "or_ne"; break;
            case CompareOpLessThan:             return "or_lt"; break;
            case CompareOpGreaterThan:          return "or_gt"; break;
            case CompareOpLessThanOrEqualTo:    return "or_le"; break;
            case CompareOpGreaterThanOrEqualTo: return "or_ge"; break;
            default:                            return "UNKNOWN_COMPARE";
         }
      case CombineBitsOpXor:
         switch(getcompareop(insn)) {
            case CompareOpEq:                   return "xor_eq"; break;
            case CompareOpNeq:                  return "xor_ne"; break;
            case CompareOpLessThan:             return "xor_lt"; break;
            case CompareOpGreaterThan:          return "xor_gt"; break;
            case CompareOpLessThanOrEqualTo:    return "xor_le"; break;
            case CompareOpGreaterThanOrEqualTo: return "xor_ge"; break;
            default:                            return "UNKNOWN_COMPARE";
         }
      default:                                  return "UNKNOWN_COMPARE";
   }
}

/* vim: set expandtab tabstop=3 shiftwidth=3: */
