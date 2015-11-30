#include <string.h>
#include <stdint.h>
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
      case MoveOpMove:           return "move"; /* move <idx> r? r? <mask> */
      case MoveOpSwap:           return "swap"; /* swap <idx> r? r? <mask> */
      case MoveOpSet:            return "set"; /* set <pos> r? $imm */
      case MoveOpSlice:          return "slice"; /* slice <idx> r? r? <mask> */
      default:                   return "UNKNOWN_MOVE";
   }
}

const char* iris_jump_mnemonic(instruction* insn) {
   switch(get_jump_form(insn)) {
      case JumpOpUnconditional: 
         {
            if (get_jump_link_flag(insn)) {
               if (get_jump_immediate_flag(insn)) {
                  return "goto.link";
               } else {
                  return "jump.link";
               }
            } else {
              if (get_jump_immediate_flag(insn)) {
                  return "goto";
              } else {
                 return "jump";
              }
            }
            break;
         }
      case JumpOpConditional: 
         {
            if (get_jump_true_false_flag(insn)) { 
               /* true form */
               if (get_jump_link_flag(insn)) {
                  /* link form */
                  if (get_jump_immediate_flag(insn)) {
                     /* immediate form */
                     return "goto.if1.link";
                  } else {
                     /* register form */
                     return "jump.if1.link";
                  }
               } else {
                  /* non link form */
                  if (get_jump_immediate_flag(insn)) {
                     /* immediate form */
                     return "goto.if1";
                  } else {
                     /* register form */
                     return "jump.if1";
                  }
               }
            } else {
               /* false form */
               if (get_jump_link_flag(insn)) {
                  /* link */
                  if (get_jump_immediate_flag(insn)) {
                     /* immediate */
                     return "goto.if0.link";
                  } else {
                     /* register */
                     return "jump.if0.link";
                  }
               } else {
                  /* non-link */
                  if (get_jump_immediate_flag(insn)) {
                     /* immediate */
                     return "goto.if0";
                  } else {
                     /* register */
                     return "jump.if0";
                  }
               }
            }
            break;
         }
      case JumpOpIfThenElse:
         {
            if (get_jump_true_false_flag(insn)) {
               /* true */
               if (get_jump_link_flag(insn)) {
                  return "if1.link";
               } else {
                  return "if1";
               }
            } else {
               /* false */
               if (get_jump_link_flag(insn)) {
                  return "if0.link";
               } else {
                  return "if0";
               }
            }
            break;
         }
      default:
         return "INVALID JUMP OPERATION";
   }
}

const char* iris_compare_mnemonic(instruction* insn) {
   switch(get_compare_op(insn)) {
#define X(class, __, name) \
   case class: \
               {\
                  switch(get_combine_op(insn)) { \
                     case CombineOpSet: \
                                        return name ; \
                     case CombineOpAnd: \
                                        return "and." name ; \
                     case CombineOpOr: \
                                       return "or." name ; \
                     case CombineOpXor: \
                                        return "xor." name ; \
                     default: \
                              iris_error("invalid combine operation", ErrorInvalidCombineOperation); \
                  } \
                  return "UNKNOWN_COMBINE"; \
               }
#include "moveops.def" 
#undef X
      default:                                  return "UNKNOWN_COMPARE";
   }
}

const char* iris_misc_mnemonic(instruction* insn) {
   switch(get_misc_op(insn)) {
      case MiscOpSystemCall:                       return "system";
      default:                                     return "UNKNOWN_MISC";
   }

}

/* vim: set expandtab tabstop=3 shiftwidth=3: */
