#include <stdlib.h>
#include <stdio.h>
#include "unparse.h"

void unparse(char* unparsed, ushort value) {
   datum d;
   instruction i;
   d.value = value;
   i.value = d.rest;
   switch(d.group) {
      case InstructionGroupArithmetic:
         unparse_arithmetic(unparsed, i);
         break;
         // case InstructionGroupMove:
         //    unparse_move(unparsed, i);
         //    break;
         // case InstructionGroupJump:
         //    unparse_jump(unparsed, i);
         //    break;
         // case InstructionGroupCompare:
         //    unparse_compare(unparsed, i);
         //    break;
      default:
         error("invalid instruction group provided",
               ErrorInvalidInstructionGroupProvided);
   }
}

void unparse_register(char* unparsed, byte index) {
   if(index < RegisterCount) {
      sprintf(unparsed, "r%d", index);
   } else {
      error("attempted to unparse an out-of-range register",
            ErrorRegisterOutOfRange);
   }
}

void unparse_arithmetic(char* unparsed, instruction i) {
   char dest[3];
   char source0[3];
   char source1[3];

   unparse_register(dest, i.arithmetic.dest);
   unparse_register(source0, i.arithmetic.source0);
   unparse_register(source1, i.arithmetic.source1);

   switch(i.arithmetic.op) {
      case ArithmeticOpAdd:
         sprintf(unparsed, "add %s <- %s %s", dest, source0, source1);
         break;
      case ArithmeticOpSub:
         sprintf(unparsed, "sub %s <- %s %s", dest, source0, source1);
         break;
      case ArithmeticOpMul:
         sprintf(unparsed, "mul %s <- %s %s", dest, source0, source1);
         break;
      case ArithmeticOpDiv:
         sprintf(unparsed, "div %s <- %s %s", dest, source0, source1);
         break;
      case ArithmeticOpRem:
         sprintf(unparsed, "rem %s <- %s %s", dest, source0, source1);
         break;
      case ArithmeticOpShiftLeft:
         sprintf(unparsed, "shl %s <- %s %s", dest, source0, source1);
         break;
      case ArithmeticOpShiftRight:
         sprintf(unparsed, "shr %s <- %s %s", dest, source0, source1);
         break;
      case ArithmeticOpBinaryAnd:
         sprintf(unparsed, "and %s <- %s %s", dest, source0, source1);
         break;
      case ArithmeticOpBinaryOr:
         sprintf(unparsed, "or %s <- %s %s", dest, source0, source1);
         break;
      case ArithmeticOpBinaryNot:
         sprintf(unparsed, "not %s <- %s", dest, source0);
         break;
      case ArithmeticOpBinaryXor:
         sprintf(unparsed, "xor %s <- %s %s", dest, source0, source1);
         break;
      default:
         error("invalid arithmetic operation",
               ErrorInvalidArithmeticOperation);
   }
}
