#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include "iris.h"
enum {
   RegisterStringLength = 5,
};
enum {
   twoOperandRegister,
   twoOperandImmediate,
   threeOperandRegister,
   threeOperandImmediate,
   fourOperandRegister,
   fourOperandImmediate,
   fiveOperandMasked,
   fourOperandSet,
};
const char* operandTypes[] = {
   "%s %s",
   "%s %d",
   "%s %s %s",
   "%s %s %d",
   "%s %s %s %s",
   "%s %s %s %d",
   "%s %d %s %s %x",
   "%s %d %s %d",
};
void iris_unparse(char* unparsed, instruction* insn) {
   switch(get_group(insn)) {
      case InstructionGroupArithmetic:
         iris_unparse_arithmetic(unparsed, insn);
         break;
      case InstructionGroupMove:
         iris_unparse_move(unparsed, insn);
         break;
      case InstructionGroupJump:
         iris_unparse_jump(unparsed, insn);
         break;
      case InstructionGroupCompare:
         iris_unparse_compare(unparsed, insn);
         break;
      case InstructionGroupMisc:
         iris_unparse_misc(unparsed, insn);
         break;
      default:
         sprintf(unparsed, "UNASSIGNED INSTRUCTION GROUP");
         break;
   }
}

void iris_unparse_register(char* unparsed, byte index) {
   if(index < RegisterCount) {
      sprintf(unparsed, "r%d", index);
   } else {
      sprintf(unparsed, "INVALID_REGISTER");
   }
}
void iris_unparse_value(char* unparsed, byte value) {
   sprintf(unparsed, "0x%x", value);
   
}

void iris_unparse_arithmetic(char* unparsed, instruction* insn) {
   const char* op;
   char dest[RegisterStringLength];
   char source0[RegisterStringLength];
   char source1[RegisterStringLength];

   op = iris_arithmetic_mnemonic(insn);
   iris_unparse_register(dest, get_arithmetic_dest(insn));
   iris_unparse_register(source0, get_arithmetic_source0(insn));
   iris_unparse_register(source1, get_arithmetic_source1(insn));

   switch(get_arithmetic_op(insn)) {
      case ArithmeticOpBinaryNot:
         sprintf(unparsed, operandTypes[threeOperandRegister], op, dest, source0);
         break;
      case ArithmeticOpAddImmediate:
      case ArithmeticOpSubImmediate:
      case ArithmeticOpMulImmediate:
      case ArithmeticOpDivImmediate:
      case ArithmeticOpRemImmediate:
      case ArithmeticOpShiftLeftImmediate:
      case ArithmeticOpShiftRightImmediate:
         sprintf(unparsed, operandTypes[fourOperandImmediate], op, dest, source0, get_arithmetic_immediate(insn));
         break;
      default:
         sprintf(unparsed, operandTypes[fourOperandRegister], op, dest, source0, source1);
         break;
   }
}

void iris_unparse_move(char* unparsed, instruction* insn) {
   const char* op;
   char reg0[RegisterStringLength];
   char reg1[RegisterStringLength];

   op = iris_move_mnemonic(insn);
   switch(get_move_op(insn)) {
      case MoveOpMove: /* move <idx> r? r? <mask> */
      case MoveOpSwap: /* swap <idx> r? r? <mask> */
      case MoveOpSlice: /* slice <idx> r? r? <mask> */
         iris_unparse_register(reg0, get_move_reg0(insn));
         iris_unparse_register(reg1, get_move_reg1(insn));
         sprintf(unparsed, operandTypes[fiveOperandMasked], op, get_move_position(insn), reg0, reg1, get_move_mask(insn));
         break;
      case MoveOpSet: /* set <position> r? imm */
         iris_unparse_register(reg0, get_move_reg0(insn));
         sprintf(unparsed, operandTypes[fourOperandSet], op, get_move_position(insn), reg0, get_move_immediate(insn));
         break;
      default:
         sprintf(unparsed, "%s", "INVALID MOVE");
         break;
   }
}
void iris_unparse_jump(char* unparsed, instruction* insn) {
   const char* op;
   char reg0[RegisterStringLength];
   char reg1[RegisterStringLength];
   char reg2[RegisterStringLength];
   op = iris_jump_mnemonic(insn);
   switch(get_jump_form(insn)) {
      case JumpOpUnconditional: 
         {
            if (get_jump_link_flag(insn)) {
               /* link */
               if (get_jump_immediate_flag(insn)) {
                  /* immediate */
                  iris_unparse_register(reg0, get_jump_reg0(insn));
                  sprintf(unparsed, operandTypes[threeOperandImmediate], op, reg0, get_jump_immediate(insn));
               } else {
                  /* register */
                  iris_unparse_register(reg0, get_jump_reg0(insn));
                  iris_unparse_register(reg1, get_jump_reg1(insn));
                  sprintf(unparsed, operandTypes[threeOperandRegister], op, reg0, reg1);
               }
            } else {
               if (get_jump_immediate_flag(insn)) {
                  /* immediate */
                  sprintf(unparsed, operandTypes[twoOperandImmediate], op, get_jump_immediate(insn));
               } else {
                  /* register */
                  iris_unparse_register(reg0, get_jump_reg0(insn));
                  sprintf(unparsed, operandTypes[twoOperandRegister], op, reg0);
               }
            }
            break;
         }
      case JumpOpConditional: 
         {
            if (get_jump_immediate_flag(insn)) {
               /* immediate form */
               iris_unparse_register(reg0, get_jump_reg0(insn));
               sprintf(unparsed, operandTypes[threeOperandImmediate], op, reg0, get_jump_immediate(insn));
            } else {
               /* register form */
               iris_unparse_register(reg0, get_jump_reg0(insn));
               iris_unparse_register(reg1, get_jump_reg1(insn));
               sprintf(unparsed, operandTypes[threeOperandRegister], op, reg0, reg1);
            }
            break;
         }
      case JumpOpIfThenElse:
         iris_unparse_register(reg0, get_jump_reg0(insn));
         iris_unparse_register(reg1, get_jump_reg1(insn));
         iris_unparse_register(reg2, get_jump_reg2(insn));
         sprintf(unparsed, operandTypes[fourOperandRegister], op, reg0, reg1, reg2);
         break;
      default: 
         sprintf(unparsed, "%s", "INVALID JUMP");
         break;
   }
}


void iris_unparse_compare(char* unparsed, instruction* insn) {
   const char* op;
   char reg0[RegisterStringLength];
   char reg1[RegisterStringLength];
   char reg2[RegisterStringLength];

   op = iris_compare_mnemonic(insn);
   iris_unparse_register(reg0, get_compare_reg0(insn));
   iris_unparse_register(reg1, get_compare_reg1(insn));
   iris_unparse_register(reg2, get_compare_reg2(insn));

   sprintf(unparsed, operandTypes[fourOperandRegister], op, reg0, reg1, reg2);
}
void iris_unparse_misc(char* unparsed, instruction* insn) {
   const char* op;
   char reg0[RegisterStringLength];
   char reg1[RegisterStringLength];
   op = iris_misc_mnemonic(insn);
   switch(get_misc_op(insn)) {
      case MiscOpSystemCall:
         iris_unparse_register(reg0, get_misc_reg0(insn));
         iris_unparse_register(reg1, get_misc_reg1(insn));
         sprintf(unparsed, "%s %d %s %s", op, get_misc_index(insn), reg0, reg1);
         break;
      default:
         sprintf(unparsed, "%s", "INVALID MISC");
         break;

   }

}

void iris_unparse_bitstring(char* unparsed, instruction* insn) {
   int bit;
   uint32_t data;
   unparsed[32] = '\0';
   data = *insn;
   for (bit = 31; bit >= 0; bit -= 1) {
      unparsed[bit] = (data & 1) + '0';
      data >>= 1;
   }
}

/* vim: set expandtab tabstop=3 shiftwidth=3: */
