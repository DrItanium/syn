#include <stdlib.h>
#include <stdio.h>
#include "mnemonic.h"
#include "unparse.h"

void unparse(char* unparsed, ushort insn) {
   switch(getgroup(insn)) {
      case InstructionGroupArithmetic:
         unparse_arithmetic(unparsed, insn);
         break;
      case InstructionGroupMove:
         unparse_move(unparsed, insn);
         break;
      case InstructionGroupJump:
         unparse_jump(unparsed, insn);
         break;
      case InstructionGroupCompare:
         unparse_compare(unparsed, insn);
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

void unparse_arithmetic(char* unparsed, ushort insn) {
   const char* op;
   char dest[3];
   char source0[3];
   char source1[3];

   op = arithmetic_mnemonic(insn);
   unparse_register(dest, getarithmeticdest(insn));
   unparse_register(source0, getarithmeticsource0(insn));
   unparse_register(source1, getarithmeticsource1(insn));

   switch(getarithmeticop(insn)) {
      case ArithmeticOpBinaryNot:
         sprintf(unparsed, "%s %s <- %s", op, dest, source0);
         break;
      default:
         sprintf(unparsed, "%s %s <- %s %s", op, dest, source0, source1);
   }
}

void unparse_move(char* unparsed, ushort insn) {
   const char* op;
   char reg0[3];
   char reg1[3];
   char reg2[3];

   op = move_mnemonic(insn);

   switch(getmoveop(insn)) {
      case MoveOpRegToReg:
         unparse_register(reg0, getmovereg0(insn));
         unparse_register(reg1, getmovereg1(insn));
         sprintf(unparsed, "%s %s <- %s", op, reg0, reg1);
         break;
      case MoveOpImmediateToReg:
         unparse_register(reg0, getmovereg0(insn));
         sprintf(unparsed, "%s %s <- %d", op, reg0, getmoveimmediate(insn));
         break;
      case MoveOpRegToAddress:
         unparse_register(reg0, getmovereg0(insn));
         unparse_register(reg1, getmovereg1(insn));
         unparse_register(reg2, getmovereg2(insn));
         if(getmoveaccessmode(insn) == AccessModeMoveOpLoad) {
            sprintf(unparsed, "%s %s <- %s %s", op, reg0, reg1, reg2);
         } else {
            sprintf(unparsed, "%s %s -> %s %s", op, reg0, reg1, reg2);
         }
         break;
      default:
         sprintf(unparsed, "INVALID MOVE");
   }
}

void unparse_jump(char* unparsed, ushort insn) {
   sprintf(unparsed, "%s TODO", jump_mnemonic(insn));
}

void unparse_compare(char* unparsed, ushort insn) {
   const char* op;
   char reg0[3];
   char reg1[3];

   op = compare_mnemonic(insn);
   unparse_register(reg0, getcomparereg0(insn));
   unparse_register(reg1, getcomparereg1(insn));

   sprintf(unparsed, "%s %s %s", op, reg0, reg1);
}

void unparse_bitstring(char* unparsed, ushort insn) {
   unparsed[16] = '\0';
   int bit;
   for (bit = 15; bit >= 0; bit -= 1) {
      unparsed[bit] = (insn & 1) + '0';
      insn >>= 1;
   }
}

/* vim: set expandtab tabstop=3 shiftwidth=3: */
