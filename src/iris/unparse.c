#include <stdlib.h>
#include <stdio.h>
#include "iris.h"

void unparse(char* unparsed, ushort insn) {
   switch(get_group(insn)) {
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
   unparse_register(dest, get_arithmetic_dest(insn));
   unparse_register(source0, get_arithmetic_source0(insn));
   unparse_register(source1, get_arithmetic_source1(insn));

   switch(get_arithmetic_op(insn)) {
      case ArithmeticOpBinaryNot:
         sprintf(unparsed, "%s %s %s", op, dest, source0);
         break;
      default:
         sprintf(unparsed, "%s %s %s %s", op, dest, source0, source1);
   }
}

void unparse_move(char* unparsed, ushort insn) {
   const char* op;
   char reg0[3];
   char reg1[3];
   char reg2[3];

   op = move_mnemonic(insn);

   switch(get_move_op(insn)) {
      case MoveOpRegToReg:
         unparse_register(reg0, get_move_reg0(insn));
         unparse_register(reg1, get_move_reg1(insn));
         sprintf(unparsed, "%s %s %s", op, reg0, reg1);
         break;
      case MoveOpImmediateToReg:
         unparse_register(reg0, get_move_reg0(insn));
         sprintf(unparsed, "%s %s %d", op, reg0, get_move_immediate(insn));
         break;
      case MoveOpRegToAddress:
         unparse_register(reg0, get_move_reg0(insn));
         unparse_register(reg1, get_move_reg1(insn));
         unparse_register(reg2, get_move_reg2(insn));
         sprintf(unparsed, "%s %s %s:%s", op, reg0, reg1, reg2);
         break;
      default:
         sprintf(unparsed, "INVALID MOVE");
   }
}

void unparse_jump(char* unparsed, ushort insn) {
   byte is_normal;
   is_normal = get_jump_conditional(insn) != JumpOpIfThenElse;
   if(is_normal == 1) {
      unparse_normal_jump(unparsed, insn);
   } else {
      unparse_if_then_else(unparsed, insn);
   }
}

void unparse_normal_jump(char* unparsed, ushort insn) {
   const char* op;
   char reg0[3];
   char reg1[3];
   byte is_relative;
   byte is_immediate;
   byte is_signed;

   op = jump_mnemonic(insn);
   is_relative = get_jump_distance(insn) == JumpDistanceShort;
   is_immediate = get_jump_immediatemode(insn) == 1;
   is_signed = get_jump_signedmode(insn) == 1;

   if(is_relative && is_immediate && is_signed == 1) {
      sprintf(unparsed, "%s %d", op, (schar) get_jump_immediate(insn));
   } else if(is_relative && is_immediate && !is_signed == 1) {
      sprintf(unparsed, "%s %d", op, get_jump_immediate(insn));
   } else if(is_relative && !is_immediate && is_signed == 1) {
      unparse_register(reg1, get_jump_reg1(insn));
      sprintf(unparsed, "%s $%s", op, reg1);
   } else if(is_relative && !is_immediate && !is_signed == 1) {
      unparse_register(reg1, get_jump_reg1(insn));
      sprintf(unparsed, "%s %s", op, reg1);
   } else if(!is_relative) {
      unparse_register(reg0, get_jump_reg0(insn));
      unparse_register(reg1, get_jump_reg1(insn));
      sprintf(unparsed, "%s %s:%s", op, reg0, reg1);
   } else {
      sprintf(unparsed, "INVALID JUMP");
   }
}

void unparse_if_then_else(char* unparsed, ushort insn) {
   const char* op;
   char reg0[3];
   char reg1[3];
   byte reg0_is_signed;
   byte reg1_is_signed;

   op = jump_mnemonic(insn);
   unparse_register(reg0, get_jump_reg0(insn));
   unparse_register(reg1, get_jump_reg1(insn));
   reg0_is_signed = get_jump_signedmode(insn) == 1;
   reg1_is_signed = get_jump_reg1issigned(insn) == 1;

   if(reg0_is_signed && reg1_is_signed == 1) {
      sprintf(unparsed, "%s $%s $%s", op, reg0, reg1);
   } else if(reg0_is_signed && !reg1_is_signed == 1) {
      sprintf(unparsed, "%s $%s %s", op, reg0, reg1);
   } else if(!reg0_is_signed && reg1_is_signed == 1) {
      sprintf(unparsed, "%s %s $%s", op, reg0, reg1);
   } else if(!reg0_is_signed && !reg1_is_signed == 1) {
      sprintf(unparsed, "%s %s %s", op, reg0, reg1);
   } else {
      sprintf(unparsed, "INVALID JUMP");
   }
}

void unparse_compare(char* unparsed, ushort insn) {
   const char* op;
   char reg0[3];
   char reg1[3];

   op = compare_mnemonic(insn);
   unparse_register(reg0, get_compare_reg0(insn));
   unparse_register(reg1, get_compare_reg1(insn));

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
