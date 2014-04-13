#include <stdlib.h>
#include <stdio.h>
#include "iris.h"
#define REGISTER_STRING_LENGTH 5
void unparse(char* unparsed, instruction* insn) {
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
      case InstructionGroupMisc:
         unparse_misc(unparsed, insn);
         break;
      default:
         sprintf(unparsed, "UNASSIGNED INSTRUCTION GROUP");
         break;
   }
}

void unparse_register(char* unparsed, byte index) {
   if(index < RegisterCount) {
      sprintf(unparsed, "r%d", index);
   } else {
      sprintf(unparsed, "INVALID_REGISTER");
   }
}

void unparse_arithmetic(char* unparsed, instruction* insn) {
   const char* op;
   char dest[REGISTER_STRING_LENGTH];
   char source0[REGISTER_STRING_LENGTH];
   char source1[REGISTER_STRING_LENGTH];

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
         break;
   }
}

void unparse_move(char* unparsed, instruction* insn) {
   const char* op;
   char reg0[REGISTER_STRING_LENGTH];
   char reg1[REGISTER_STRING_LENGTH];

   op = move_mnemonic(insn);
   switch(get_move_op(insn)) {
      case MoveOpMove: /* move r? r? */
      case MoveOpSwap: /* swap r? r? */
      case MoveOpSwapRegAddr: /* swap.reg.addr r? r? */
      case MoveOpSwapAddrAddr: /* swap.addr.addr r? r? */
         unparse_register(reg0, get_move_reg0(insn));
         unparse_register(reg1, get_move_reg1(insn));
         sprintf(unparsed, "%s %s %s", op, reg0, reg1);
         break;
      case MoveOpSwapRegMem: /* swap.reg.mem r? $imm */
      case MoveOpSwapAddrMem: /* swap.addr.mem r? $imm */
      case MoveOpSet: /* set r? $imm */
         unparse_register(reg0, get_move_reg0(insn));
         sprintf(unparsed, "%s %s $%d", op, reg0, get_move_immediate(insn));
         break;
      case MoveOpLoad: /* load r? r? */
         unparse_register(reg0, get_move_reg0(insn));
         unparse_register(reg1, get_move_reg1(insn));
         sprintf(unparsed, "%s %s %s", op, reg0, reg1);
         break;
      case MoveOpLoadMem: /* load.mem r? $imm */
         unparse_register(reg0, get_move_reg0(insn));
         sprintf(unparsed, "%s %s $%d", op, reg0, get_move_immediate(insn));
         break;
      case MoveOpStore: /* store r? r? */
      case MoveOpStoreAddr: /* store.addr r? r? */
         unparse_register(reg0, get_move_reg0(insn));
         unparse_register(reg1, get_move_reg1(insn));
         sprintf(unparsed, "%s %s %s", op, reg0, reg1);
         break;
      case MoveOpStoreMem: /* memcopy r? $imm */
      case MoveOpStoreImm: /* memset r? $imm */
         unparse_register(reg0, get_move_reg0(insn));
         sprintf(unparsed, "%s %s $%d", op, reg0, get_move_immediate(insn));
         break;
      default:
         sprintf(unparsed, "%s", "INVALID MOVE");
         break;
   }
}

void unparse_jump(char* unparsed, instruction* insn) {
   const char* op;
   char reg0[REGISTER_STRING_LENGTH];
   char reg1[REGISTER_STRING_LENGTH];
   char reg2[REGISTER_STRING_LENGTH];
   op = jump_mnemonic(insn);
   switch(get_jump_op(insn)) {
      case JumpOpUnconditionalImmediate:
         sprintf(unparsed, "%s $%d", op, get_jump_immediate(insn));
         break;
      case JumpOpUnconditionalImmediateLink:
         unparse_register(reg0, get_jump_reg0(insn));
         sprintf(unparsed, "%s %s $%d", op, reg0, get_jump_immediate(insn));
         break;
      case JumpOpUnconditionalRegister:
         unparse_register(reg0, get_jump_reg0(insn));
         sprintf(unparsed, "%s %s", op, reg0);
         break;
      case JumpOpUnconditionalRegisterLink:
         unparse_register(reg0, get_jump_reg0(insn));
         unparse_register(reg1, get_jump_reg1(insn));
         sprintf(unparsed, "%s %s %s", op, reg0, reg1);
         break;
      case JumpOpConditionalTrueImmediate:
      case JumpOpConditionalFalseImmediate:
         unparse_register(reg0, get_jump_reg0(insn));
         sprintf(unparsed, "%s %s $%d", op, reg0, get_jump_immediate(insn));
         break;
      case JumpOpConditionalTrueImmediateLink:
      case JumpOpConditionalFalseImmediateLink:
         /* remember that the predicate is implied */
         unparse_register(reg0, get_jump_reg0(insn));
         sprintf(unparsed, "%s %s $%d", op, reg0, get_jump_immediate(insn));
         break;
      case JumpOpConditionalTrueRegister:
      case JumpOpConditionalFalseRegister:
         unparse_register(reg0, get_jump_reg0(insn));
         unparse_register(reg1, get_jump_reg1(insn));
         sprintf(unparsed, "%s %s %s", op, reg0, reg1);
         break;
      case JumpOpConditionalTrueRegisterLink:
      case JumpOpConditionalFalseRegisterLink:
      case JumpOpIfThenElseNormalPredTrue:
      case JumpOpIfThenElseNormalPredFalse:
      case JumpOpIfThenElseLinkPredTrue: /* implied predicate register */
      case JumpOpIfThenElseLinkPredFalse: /* implied predicate register */
         unparse_register(reg0, get_jump_reg0(insn));
         unparse_register(reg1, get_jump_reg1(insn));
         unparse_register(reg2, get_jump_reg2(insn));
         sprintf(unparsed, "%s %s %s %s", op, reg0, reg1, reg2);
         break;
      default: 
         sprintf(unparsed, "%s", "INVALID JUMP");
         break;
   }
}


void unparse_compare(char* unparsed, instruction* insn) {
   const char* op;
   char reg0[REGISTER_STRING_LENGTH];
   char reg1[REGISTER_STRING_LENGTH];
   char reg2[REGISTER_STRING_LENGTH];

   op = compare_mnemonic(insn);
   unparse_register(reg0, get_compare_reg0(insn));
   unparse_register(reg1, get_compare_reg1(insn));
   unparse_register(reg2, get_compare_reg2(insn));

   sprintf(unparsed, "%s %s %s %s", op, reg0, reg1, reg2);
}
void unparse_misc(char* unparsed, instruction* insn) {
   const char* op;
   char reg0[REGISTER_STRING_LENGTH];
   char reg1[REGISTER_STRING_LENGTH];
   op = misc_mnemonic(insn);
   switch(get_misc_op(insn)) {
      case MiscOpSystemCall:
            unparse_register(reg0, get_misc_reg0(insn));
            unparse_register(reg1, get_misc_reg1(insn));
            sprintf(unparsed, "%s $%d %s %s", op, get_misc_index(insn), reg0, reg1);
            break;
      case MiscOpSetImplicitRegisterImmediate:
            unparse_register(reg0, get_misc_reg0(insn));
            sprintf(unparsed, "%s $%d %s", op, get_misc_index(insn), reg0);
            break;
      case MiscOpSetImplicitRegisterIndirect:
            /* the index becomes a register */
            unparse_register(reg0, get_misc_index(insn));
            unparse_register(reg1, get_misc_reg0(insn));
            sprintf(unparsed, "%s %s %s", op, reg0, reg1);
            break;
            /* Since an implication of <- as being flow order we have to
             * reverse args */
      case MiscOpGetImplicitRegisterImmediate:
            unparse_register(reg0, get_misc_index(insn));
            sprintf(unparsed, "%s %s $%d", op, reg0, get_misc_reg0(insn));
            break;
      case MiscOpGetImplicitRegisterIndirect:
            unparse_register(reg0, get_misc_index(insn));
            unparse_register(reg1, get_misc_reg0(insn));
            sprintf(unparsed, "%s %s %s", op, reg0, reg1);
            break;
      default:
         sprintf(unparsed, "%s", "INVALID MISC");
         break;

   }

}

void unparse_bitstring(char* unparsed, instruction* insn) {
   int bit;
   uint data;
   unparsed[32] = '\0';
   data = insn->full;
   for (bit = 31; bit >= 0; bit -= 1) {
      unparsed[bit] = (data & 1) + '0';
      data >>= 1;
   }
}

/* vim: set expandtab tabstop=3 shiftwidth=3: */
