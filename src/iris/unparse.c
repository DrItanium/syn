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
      default:
         sprintf(unparsed, "%s %s <- %s %s", insn, dest, source0, source1);
         break;
   }
}

void unparse_move(char* unparsed, instruction i) {
   sprintf(unparsed, "%s TODO", move_mnemonic(i));
}

void unparse_jump(char* unparsed, instruction i) {
   sprintf(unparsed, "%s TODO", jump_mnemonic(i));
}

void unparse_compare(char* unparsed, instruction i) {
   sprintf(unparsed, "%s TODO", compare_mnemonic(i));
}
