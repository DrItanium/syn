#include <stdlib.h>
#include <stdio.h>
#include "mnemonic.h"
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
      case InstructionGroupMove:
         unparse_move(unparsed, i);
         break;
      case InstructionGroupJump:
         unparse_jump(unparsed, i);
         break;
      case InstructionGroupCompare:
         unparse_compare(unparsed, i);
         break;
      default:
         sprintf(unparsed, "UNASSIGNED INSTRUCTION GROUP");
   }
}

void unparse_register(char* unparsed, byte index) {
   if(index < RegisterCount) {
      sprintf(unparsed, "r%d", index);
   } else {
      sprintf(unparsed, "INVALID_REGISTER");
   }
}

void unparse_arithmetic(char* unparsed, instruction i) {
   char* insn;
   char dest[3];
   char source0[3];
   char source1[3];

   insn = arithmetic_mnemonic(i);
   unparse_register(dest, i.arithmetic.dest);
   unparse_register(source0, i.arithmetic.source0);
   unparse_register(source1, i.arithmetic.source1);

   switch(i.arithmetic.op) {
      case ArithmeticOpBinaryNot:
         sprintf(unparsed, "%s %s <- %s", insn, dest, source0);
         break;
      case ArithmeticOpAdd:
      case ArithmeticOpSub:
      case ArithmeticOpMul:
      case ArithmeticOpDiv:
      case ArithmeticOpRem:
      case ArithmeticOpShiftLeft:
      case ArithmeticOpShiftRight:
      case ArithmeticOpBinaryAnd:
      case ArithmeticOpBinaryOr:
      case ArithmeticOpBinaryXor:
         sprintf(unparsed, "%s %s <- %s %s", insn, dest, source0, source1);
         break;
      default:
         sprintf(unparsed, "INVALID ARITHMETIC");
   }
}

void unparse_move(char* unparsed, instruction i) {
   char* insn;
   char reg0[3];
   char reg1[3];
   char reg2[3];

   insn = move_mnemonic(i);

   switch(i.move.op) {
      case MoveOpRegToReg:
         unparse_register(reg0, i.move.reg0);
         unparse_register(reg1, i.move.reg1);
         sprintf(unparsed, "%s %s <- %s", insn, reg0, reg1);
         break;
      case MoveOpImmediateToReg:
         unparse_register(reg0, i.move.reg0);
         sprintf(unparsed, "%s %s <- %d", insn, reg0, i.move.immediate);
         break;
      case MoveOpRegToAddress:
         unparse_register(reg0, i.move.reg0);
         unparse_register(reg1, i.move.addressmode.reg1);
         unparse_register(reg2, i.move.addressmode.reg2);
         if(i.move.addressmode.accessmode == AccessModeMoveOpLoad) {
            sprintf(unparsed, "%s %s <- %s %s", insn, reg0, reg1, reg2);
         } else {
            sprintf(unparsed, "%s %s -> %s %s", insn, reg0, reg1, reg2);
         }
         break;
      default:
         sprintf(unparsed, "INVALID MOVE");
   }
}

void unparse_jump(char* unparsed, instruction i) {
   sprintf(unparsed, "%s TODO", jump_mnemonic(i));
}

void unparse_compare(char* unparsed, instruction i) {
   sprintf(unparsed, "%s TODO", compare_mnemonic(i));
}

void unparse_bitstring(char* unparsed, ushort value) {
   unparsed[16] = '\0';
   int bit;
   for (bit = 15; bit >= 0; bit -= 1) {
      unparsed[bit] = (value & 1) + '0';
      value >>= 1;
   }
}
