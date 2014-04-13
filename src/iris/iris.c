#include <stdlib.h>
#include <stdio.h>
#include "iris.h"

void decode(core* proc, instruction* value) {
   /* reset the advancepc value */
   proc->advancepc = 1;
   switch(get_group(value)) {
      case InstructionGroupArithmetic:
         arithmetic(proc, value);
         break;
      case InstructionGroupMove:
         move(proc, value);
         break;
      case InstructionGroupJump:
         jump(proc, value);
         break;
      case InstructionGroupCompare:
         compare(proc, value);
         break;
      case InstructionGroupMisc:
         misc(proc, value);
         break;
      default:
         error("invalid instruction group provided", ErrorInvalidInstructionGroupProvided);
         break;
   }
}

void put_register(core* proc, byte index, datum value) {
   if(index < RegisterCount) {
      proc->gpr[index] = value;
   } else {
      error("attempted to store to a register out of range", ErrorPutRegisterOutOfRange);
   }
}

datum get_register(core* proc, byte index) {
   if(index < RegisterCount) {
      return proc->gpr[index];
   } else {
      error("attempted to retrieve a value from a register out of range", ErrorGetRegisterOutOfRange);
      return 0;
   }
}

void arithmetic(core* proc, instruction* inst) {
#define perform_operation(symbol) (put_register(proc, \
         get_arithmetic_dest(inst), (get_register(proc, \
               get_arithmetic_source0(inst) symbol \
               get_arithmetic_source1(inst)))))
   switch(get_arithmetic_op(inst)) {
      case ArithmeticOpAdd:
         perform_operation(+);
         break;
      case ArithmeticOpSub:
         perform_operation(-);
         break;
      case ArithmeticOpMul:
         perform_operation(*);
         break;
      case ArithmeticOpDiv:
         perform_operation(/);
         break;
      case ArithmeticOpRem:
         perform_operation(%);
         break;
      case ArithmeticOpShiftLeft:
         perform_operation(<<);
         break;
      case ArithmeticOpShiftRight:
         perform_operation(>>);
         break;
      case ArithmeticOpBinaryAnd:
         perform_operation(&);
         break;
      case ArithmeticOpBinaryOr:
         perform_operation(|);
         break;
      case ArithmeticOpBinaryNot:
         put_register(proc, get_arithmetic_dest(inst), 
               ~(get_register(proc, get_arithmetic_source0(inst))));
         break;
      case ArithmeticOpBinaryXor:
         perform_operation(^);
         break;
      default:
         error("invalid arithmetic operation", ErrorInvalidArithmeticOperation);
   }
#undef perform_operation
}
void move(core* proc, instruction* inst) {
   ushort a, b;
   ushort addr0, addr1;
   a = 0;
   b = 0;
   addr0 = 0;
   addr1 = 0;
   switch(get_move_op(inst)) {

      case MoveOpMove: /* move r? r? */
         put_register(proc, get_move_reg0(inst), 
               get_register(proc, get_move_reg1(inst)));
         break;
      case MoveOpSwap: /* swap r? r? */
         a = get_register(proc, get_move_reg0(inst));
         b = get_register(proc, get_move_reg1(inst));
         put_register(proc, get_move_reg0(inst), b);
         put_register(proc, get_move_reg1(inst), a);
         break;
      case MoveOpSwapRegAddr: /* swap.reg.addr r? r? */
         /* need to preserve the address in case it gets overwritten */
         /* for example, swap.reg.addr r0 r0 */
         addr0 = get_register(proc, get_move_reg1(inst));
         a = get_register(proc, get_move_reg0(inst));
         b = proc->data[addr0];
         put_register(proc, get_move_reg0(inst), b);
         proc->data[addr0] = a;
         break;
      case MoveOpSwapAddrAddr: /* swap.addr.addr r? r? */
         /* we're not touching registers */
         addr0 = get_register(proc, get_move_reg0(inst));
         addr1 = get_register(proc, get_move_reg1(inst));
         a = proc->data[addr0];
         b = proc->data[addr1];
         proc->data[addr0] = b;
         proc->data[addr1] = a;
         break;
      case MoveOpSwapRegMem: /* swap.reg.mem r? $imm */
         addr0 = get_move_immediate(inst);
         a = get_register(proc, get_move_reg0(inst));
         b = proc->data[addr0];
         put_register(proc, get_move_reg0(inst), b);
         proc->data[addr0] = a;
         break;
      case MoveOpSwapAddrMem: /* swap.addr.mem r? $imm */
         addr0 = get_register(proc, get_move_reg0(inst));
         addr1 = get_move_immediate(inst);
         a = proc->data[addr0];
         b = proc->data[addr1];
         proc->data[addr0] = b;
         proc->data[addr1] = a;
         break;
      case MoveOpSet: /* set r? $imm */
         put_register(proc, get_move_reg0(inst), get_move_immediate(inst));
         break;
      case MoveOpLoad: /* load r? r? */
         addr0 = get_register(proc, get_move_reg1(inst));
         a = proc->data[addr0];
         put_register(proc, get_move_reg0(inst), a);
         break;
      case MoveOpLoadMem: /* load.mem r? $imm */
         addr0 = get_move_immediate(inst);
         a = proc->data[addr0];
         put_register(proc, get_move_reg0(inst), a);
         break;

         /* In the case of stores, reg0 contains the address and the second
          * field contains the value to be stored. This maintains the idea that
          * the destination comes before the source registers. Think of the
          * following flow direction <- and it should make sense */

      case MoveOpStore: /* store r? r? */
         addr0 = get_register(proc, get_move_reg0(inst));
         a = get_register(proc, get_move_reg1(inst));
         proc->data[addr0] = a;
         break;
      case MoveOpStoreAddr: /* store.addr r? r? */
         addr0 = get_register(proc, get_move_reg0(inst));
         addr1 = get_register(proc, get_move_reg1(inst));
         a = proc->data[addr1];
         proc->data[addr0] = a;
         break;
      case MoveOpStoreMem: /* memcopy r? $imm */
         addr0 = get_register(proc, get_move_reg0(inst));
         addr1 = get_move_immediate(inst);
         a = proc->data[addr1];
         proc->data[addr0] = a;
         break;
      case MoveOpStoreImm: /* memset r? $imm */
         addr0 = get_register(proc, get_move_reg0(inst));
         a = get_move_immediate(inst);
         proc->data[addr0] = a;
         break;
      default:
         error("invalid move operation", ErrorInvalidMoveOperation);
   }
}
void jump(core* proc, instruction* inst) {
   proc->advancepc = 0;
   switch(get_jump_op(inst)) {
      case JumpOpUnconditionalImmediate:
         proc->pc = get_jump_immediate(inst);
         break;
      case JumpOpUnconditionalImmediateLink:
         put_register(proc, get_jump_reg0(inst), proc->pc + 1);
         proc->pc = get_jump_immediate(inst);
         break;
      case JumpOpUnconditionalRegister:
         proc->pc = get_register(proc, get_jump_reg0(inst));
         break;
      case JumpOpUnconditionalRegisterLink:
         put_register(proc, get_jump_reg0(inst), proc->pc + 1);
         proc->pc = get_register(proc, get_jump_reg1(inst));
         break;
      case JumpOpConditionalTrueImmediate: 
         {
            if((get_register(proc, get_jump_reg0(inst)) != 0)) {
               proc->pc = get_jump_immediate(inst); 
            } else {
               proc->advancepc = 1;
            }
            break;
         }
      case JumpOpConditionalTrueImmediateLink:
         {
            /* load the implied predicate register */
            if((get_register(proc, proc->impliedregisters[ImplicitRegisterPredicate]) != 0)) {
               put_register(proc, get_jump_reg0(inst), proc->pc + 1);
               proc->pc = get_jump_immediate(inst); 
            } else {
               proc->advancepc = 1;
            }
            break;
         }
      case JumpOpConditionalTrueRegister:
         {
            if((get_register(proc, get_jump_reg0(inst)) != 0)) {
               proc->pc = get_register(proc, get_jump_reg1(inst));
            } else {
               /* fall through */
               proc->advancepc = 1;
            }
            break;
         }
      case JumpOpConditionalTrueRegisterLink:
         {
            /* load the implied predicate register */
            if((get_register(proc, get_jump_reg0(inst)) != 0)) {
               put_register(proc, get_jump_reg1(inst), proc->pc + 1);
               proc->pc = get_register(proc, get_jump_reg2(inst));
            } else {
               proc->advancepc = 1;
            }
            break;
         }
      case JumpOpConditionalFalseImmediate: 
         {
            if((get_register(proc, get_jump_reg0(inst)) == 0)) {
               proc->pc = get_jump_immediate(inst); 
            } else {
               proc->advancepc = 1;
            }
            break;
         }
      case JumpOpConditionalFalseImmediateLink:
         {
            /* load the implied predicate register */
            if((get_register(proc, proc->impliedregisters[ImplicitRegisterPredicate]) == 0)) {
               put_register(proc, get_jump_reg0(inst), proc->pc + 1);
               proc->pc = get_jump_immediate(inst); 
            } else {
               proc->advancepc = 1;
            }
            break;
         }
      case JumpOpConditionalFalseRegister:
         {
            if((get_register(proc, get_jump_reg0(inst)) == 0)) {
               proc->pc = get_register(proc, get_jump_reg1(inst));
            } else {
               /* fall through */
               proc->advancepc = 1;
            }
            break;
         }
      case JumpOpConditionalFalseRegisterLink:
         {
            /* load the implied predicate register */
            if((get_register(proc, get_jump_reg0(inst)) == 0)) {
               put_register(proc, get_jump_reg1(inst), proc->pc + 1);
               proc->pc = get_register(proc, get_jump_reg2(inst));
            } else {
               proc->advancepc = 1;
            }
            break;
         }
      case JumpOpIfThenElseNormalPredTrue: 
         {
            if((get_register(proc, get_jump_reg0(inst)) != 0)) {
               proc->pc = get_register(proc, get_jump_reg1(inst));
            } else {
               /* fall through */
               proc->pc = get_register(proc, get_jump_reg2(inst));
            }
            break;
         }
      case JumpOpIfThenElseNormalPredFalse:
         {
            if((get_register(proc, get_jump_reg0(inst)) == 0)) {
               proc->pc = get_register(proc, get_jump_reg1(inst));
            } else {
               /* fall through */
               proc->pc = get_register(proc, get_jump_reg2(inst));
            }
            break;
         }
      case JumpOpIfThenElseLinkPredTrue:
         {
            put_register(proc, get_jump_reg0(inst), proc->pc + 1);
            if((get_register(proc, proc->impliedregisters[ImplicitRegisterPredicate]) != 0)) {
               proc->pc = get_register(proc, get_jump_reg1(inst));
            } else {
               proc->pc = get_register(proc, get_jump_reg2(inst));
            }
            break;
         }
      case JumpOpIfThenElseLinkPredFalse:
         {
            put_register(proc, get_jump_reg0(inst), proc->pc + 1);
            if((get_register(proc, proc->impliedregisters[ImplicitRegisterPredicate]) == 0)) {
               proc->pc = get_register(proc, get_jump_reg1(inst));
            } else {
               proc->pc = get_register(proc, get_jump_reg2(inst));
            }
            break;
         }
      default:
         error("invalid jump operation", ErrorInvalidJumpOperation);
   }
}

void compare(core* proc, instruction* inst) {
   ushort value;
   /* grab the appropriate value */
   value = get_register(proc, get_compare_reg0(inst));
   switch(get_compare_op(inst)) {
#define perform_operation(symbol, assign) \
      value assign (get_register(proc, get_compare_reg1(inst)) symbol \
            get_register(proc, get_compare_reg2(inst))); \
      put_register(proc, get_compare_reg0(inst), value)
#define define_group(class, symbol) \
      case CompareOp ## class :        perform_operation(symbol, =); break; \
      case CompareOp ## class ## And : perform_operation(symbol, &=); break; \
      case CompareOp ## class ## Or :  perform_operation(symbol, |=); break; \
      case CompareOp ## class ## Xor : perform_operation(symbol, ^=); break
                                       define_group(Eq, ==);
                                       define_group(Neq, !=);
                                       define_group(LessThan, <);
                                       define_group(GreaterThan, >);
                                       define_group(LessThanOrEqualTo, <=);
                                       define_group(GreaterThanOrEqualTo, >=);
#undef define_group
#undef perform_operation
      default:
         error("invalid compare operation", ErrorInvalidCompareOperation);
   }
}
static void iris_system_call(core* proc, instruction* j);
static void iris_set_implicit_register(core* proc, byte index, byte value);
static byte iris_get_implicit_register(core* proc, byte index);
void misc(core* proc, instruction* j) {
   byte index, value;
   index = 0;
   value = 0;
   /* implement system commands */
   switch(get_misc_op(j)) {
      case MiscOpSystemCall:
         iris_system_call(proc, j);
         break;
      case MiscOpSetImplicitRegisterImmediate:
         index = get_misc_index(j);
         value = (byte)get_register(proc, get_misc_reg0(j));
         iris_set_implicit_register(proc, index, value);
         break;
      case MiscOpSetImplicitRegisterIndirect:
         index = (byte)get_register(proc, get_misc_index(j));
         value = (byte)get_register(proc, get_misc_reg0(j));
         iris_set_implicit_register(proc, index, value);
         break;
      /* we need to imply a <- flow direction to maintain consistency.
       * Therefore we need to swap argument orders */
      case MiscOpGetImplicitRegisterImmediate:
         value = get_misc_index(j);
         index = get_misc_reg0(j);
         put_register(proc, value, iris_get_implicit_register(proc, index));
         break;
      case MiscOpGetImplicitRegisterIndirect:
         value = get_misc_reg0(j);
         index = (byte)get_register(proc, get_misc_index(j));
         put_register(proc, value, iris_get_implicit_register(proc, index));
         break;
      default:
         error("invalid misc operation", ErrorInvalidMiscOperation);
   }
}

void iris_set_implicit_register(core* proc, byte index, byte value) {
   proc->impliedregisters[index] = value;
}
byte iris_get_implicit_register(core* proc, byte index) {
   return proc->impliedregisters[index];
}
void iris_system_call(core* proc, instruction* j) {
   byte reg0, reg1;
   int result;
   datum value;
   reg0 = get_misc_reg0(j);
   reg1 = get_misc_reg1(j);
   switch(get_misc_index(j)) {
      case SystemCommandTerminate: /* init 0 */
         proc->terminateexecution = 1;
         break;
      case SystemCommandGetC:
         result = getchar();
         put_register(proc, reg0, (datum)result);
         break;
      case SystemCommandPutC:
         value = get_register(proc, reg0);
         putchar(value);
         break;
      default:
         error("invalid system command provided", ErrorInvalidSystemCommand);
   }
}

void error(char* message, int code) {
   fprintf(stderr, "%s\n", message);
   exit(code);
}

/* vim: set expandtab tabstop=3 shiftwidth=3: */
