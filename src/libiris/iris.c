
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include "iris.h"

void iris_dispatch(iris_core* proc, instruction* value) {
   /* reset the advancepc value */
   proc->advancepc = 1;
   switch(get_group(value)) {
      case InstructionGroupArithmetic:
         iris_arithmetic(proc, value);
         break;
      case InstructionGroupMove:
         iris_move(proc, value);
         break;
      case InstructionGroupJump:
         iris_jump(proc, value);
         break;
      case InstructionGroupCompare:
         iris_compare(proc, value);
         break;
      case InstructionGroupMisc:
         iris_misc(proc, value);
         break;
      default:
         iris_error("invalid instruction group provided", ErrorInvalidInstructionGroupProvided);
         break;
   }
}

void iris_put_register(iris_core* proc, byte index, word value) {
   if(index < RegisterCount) {
      proc->gpr[index] = value;
   } else {
      iris_error("attempted to store to a register out of range", ErrorPutRegisterOutOfRange);
   }
}

word iris_get_register(iris_core* proc, byte index) {
   if(index < RegisterCount) {
      return proc->gpr[index];
   } else {
      iris_error("attempted to retrieve a value from a register out of range", ErrorGetRegisterOutOfRange);
      return 0;
   }
}

void iris_arithmetic(iris_core* proc, instruction* inst) {
#define perform_operation(symbol) \
   (iris_put_register(proc, get_arithmetic_dest(inst), \
         (iris_get_register(proc, get_arithmetic_source0(inst))) \
          symbol \
         (iris_get_register(proc, get_arithmetic_source1(inst)))))
#define perform_operation_immediate(symbol) \
   (iris_put_register(proc, get_arithmetic_dest(inst), \
         (iris_get_register(proc, get_arithmetic_source0(inst))) \
          symbol \
         (get_arithmetic_source1(inst))))
#define defop(tag, symbol) \
      case tag: \
         perform_operation(symbol); \
         break 
#define defiop(tag, symbol) \
      case tag: \
         perform_operation_immediate(symbol); \
         break
   switch(get_arithmetic_op(inst)) {
      defop(ArithmeticOpAdd, +);
      defop(ArithmeticOpSub, -);
      defop(ArithmeticOpMul, *);
      defop(ArithmeticOpDiv, /);
      defop(ArithmeticOpRem, %);
      defop(ArithmeticOpShiftLeft, <<);
      defop(ArithmeticOpShiftRight, >>);
      defop(ArithmeticOpBinaryAnd, &);
      defop(ArithmeticOpBinaryOr, |);
      case ArithmeticOpBinaryNot:
         iris_put_register(proc, get_arithmetic_dest(inst), 
               ~(iris_get_register(proc, get_arithmetic_source0(inst))));
         break;
      defop(ArithmeticOpBinaryXor, ^);
      /* immediate operations */
      defiop(ArithmeticOpAddImmediate, +);
      defiop(ArithmeticOpSubImmediate, -);
      defiop(ArithmeticOpMulImmediate, *);
      defiop(ArithmeticOpDivImmediate, /);
      defiop(ArithmeticOpRemImmediate, %);
      defiop(ArithmeticOpShiftLeftImmediate, <<);
      defiop(ArithmeticOpShiftRightImmediate, >>);
      default:
         iris_error("invalid arithmetic operation", ErrorInvalidArithmeticOperation);
   }
#undef perform_operation
#undef perform_operation_immediate
#undef defop
#undef defiop
}
static void iris_push(iris_core* proc, instruction* inst);
static void iris_push_immediate(iris_core* proc, instruction* inst);
static void iris_pop(iris_core* proc, instruction* inst);

void iris_move(iris_core* proc, instruction* inst) {
   word a = 0, b = 0;
   word addr0 = 0, addr1 = 0;
   switch(get_move_op(inst)) {

      case MoveOpMove: /* move r? r? */
         iris_put_register(proc, get_move_reg0(inst), 
               iris_get_register(proc, get_move_reg1(inst)));
         break;
      case MoveOpSwap: /* swap r? r? */
         a = iris_get_register(proc, get_move_reg0(inst));
         b = iris_get_register(proc, get_move_reg1(inst));
         iris_put_register(proc, get_move_reg0(inst), b);
         iris_put_register(proc, get_move_reg1(inst), a);
         break;
      case MoveOpSwapRegAddr: /* swap.reg.addr r? r? */
         /* need to preserve the address in case it gets overwritten */
         /* for example, swap.reg.addr r0 r0 */
         addr0 = iris_get_register(proc, get_move_reg1(inst));
         a = iris_get_register(proc, get_move_reg0(inst));
         b = proc->data[addr0];
         iris_put_register(proc, get_move_reg0(inst), b);
         proc->data[addr0] = a;
         break;
      case MoveOpSwapAddrAddr: /* swap.addr.addr r? r? */
         /* we're not touching registers */
         addr0 = iris_get_register(proc, get_move_reg0(inst));
         addr1 = iris_get_register(proc, get_move_reg1(inst));
         a = proc->data[addr0];
         b = proc->data[addr1];
         proc->data[addr0] = b;
         proc->data[addr1] = a;
         break;
      case MoveOpSwapRegMem: /* swap.reg.mem r? $imm */
         addr0 = get_move_immediate(inst);
         a = iris_get_register(proc, get_move_reg0(inst));
         b = proc->data[addr0];
         iris_put_register(proc, get_move_reg0(inst), b);
         proc->data[addr0] = a;
         break;
      case MoveOpSwapAddrMem: /* swap.addr.mem r? $imm */
         addr0 = iris_get_register(proc, get_move_reg0(inst));
         addr1 = get_move_immediate(inst);
         a = proc->data[addr0];
         b = proc->data[addr1];
         proc->data[addr0] = b;
         proc->data[addr1] = a;
         break;
      case MoveOpSet: /* set r? $imm */
         iris_put_register(proc, get_move_reg0(inst), get_move_immediate(inst));
         break;
      case MoveOpLoad: /* load r? r? */
         addr0 = iris_get_register(proc, get_move_reg1(inst));
         a = proc->data[addr0];
         iris_put_register(proc, get_move_reg0(inst), a);
         break;
      case MoveOpLoadMem: /* load.mem r? $imm */
         addr0 = get_move_immediate(inst);
         a = proc->data[addr0];
         iris_put_register(proc, get_move_reg0(inst), a);
         break;

         /* In the case of stores, reg0 contains the address and the second
          * field contains the value to be stored. This maintains the idea that
          * the destination comes before the source registers. Think of the
          * following flow direction <- and it should make sense */

      case MoveOpStore: /* store r? r? */
         addr0 = iris_get_register(proc, get_move_reg0(inst));
         a = iris_get_register(proc, get_move_reg1(inst));
         proc->data[addr0] = a;
         break;
      case MoveOpStoreAddr: /* store.addr r? r? */
         addr0 = iris_get_register(proc, get_move_reg0(inst));
         addr1 = iris_get_register(proc, get_move_reg1(inst));
         a = proc->data[addr1];
         proc->data[addr0] = a;
         break;
      case MoveOpStoreMem: /* memcopy r? $imm */
         addr0 = iris_get_register(proc, get_move_reg0(inst));
         addr1 = get_move_immediate(inst);
         a = proc->data[addr1];
         proc->data[addr0] = a;
         break;
      case MoveOpStoreImm: /* memset r? $imm */
         addr0 = iris_get_register(proc, get_move_reg0(inst));
         a = get_move_immediate(inst);
         proc->data[addr0] = a;
         break;
      case MoveOpPush: /* push r? */
         iris_push(proc, inst);
         break;
      case MoveOpPushImmediate: /* push.imm $imm */
         iris_push_immediate(proc, inst);
         break;
      case MoveOpPop: /* pop r? */
         iris_pop(proc, inst);
         break;
      default:
         iris_error("invalid move operation", ErrorInvalidMoveOperation);
   }
}
void iris_jump(iris_core* proc, instruction* inst) {
   word a = 0;
   proc->advancepc = false;
   switch(get_jump_form(inst)) {
      case JumpOpUnconditional: 
         {
            if (get_jump_link_flag(inst)) {
               iris_put_register(proc, get_jump_reg0(inst), proc->pc + 1);
               if (get_jump_immediate_flag(inst)) {
                  proc->pc = get_jump_immediate(inst);
               } else {
                  proc->pc = iris_get_register(proc, get_jump_reg1(inst));
               }
            } else {
              if (get_jump_immediate_flag(inst)) {
                 proc->pc = get_jump_immediate(inst);
              } else {
                 proc->pc = iris_get_register(proc, get_jump_reg0(inst));
              }
            }
            break;
         }
      case JumpOpConditional: 
         {
            if (get_jump_true_false_flag(inst)) { 
               /* true form */
               if (get_jump_link_flag(inst)) {
                  /* link form */
                  if (get_jump_immediate_flag(inst)) {
                     /* immediate form */
                     if ((iris_get_register(proc, PredicateRegisterIndex) != 0)) {
                        iris_put_register(proc, get_jump_reg0(inst), proc->pc + 1);
                        proc->pc = get_jump_immediate(inst); 
                     } else {
                        proc->pc = proc->pc + 1;
                     }
                  } else {
                     /* register form */
                     if ((iris_get_register(proc, get_jump_reg0(inst)) != 0)) {
                        // cache this result first in-case the same register is
                        // used in both address to jump to and link register...
                        a = iris_get_register(proc, get_jump_reg2(inst)); 
                        iris_put_register(proc, get_jump_reg1(inst), proc->pc + 1);
                        proc->pc = a;
                     } else {
                        proc->pc = proc->pc + 1;
                     }
                  }
               } else {
                  /* non link form */
                  if (get_jump_immediate_flag(inst)) {
                     /* immediate form */
                     if ((iris_get_register(proc, get_jump_reg0(inst)) != 0)) {
                        proc->pc = get_jump_immediate(inst);
                     } else {
                        proc->pc = proc->pc + 1;
                     }
                  } else {
                     /* register form */
                     if ((iris_get_register(proc, get_jump_reg0(inst)) != 0)) {
                        proc->pc = iris_get_register(proc, get_jump_reg1(inst));
                     } else {
                        proc->pc = proc->pc + 1;
                     }
                  }
               }
            } else {
               /* false form */
               if (get_jump_link_flag(inst)) {
                  /* link */
                  if (get_jump_immediate_flag(inst)) {
                     /* immediate */
                     if ((iris_get_register(proc, PredicateRegisterIndex) == 0)) {
                        iris_put_register(proc, get_jump_reg0(inst), proc->pc + 1);
                        proc->pc = get_jump_immediate(inst); 
                     } else {
                        proc->pc = proc->pc + 1;
                     }
                  } else {
                     /* register */
                     if ((iris_get_register(proc, get_jump_reg0(inst)) == 0)) {
                        // cache this result first in-case the same register is
                        // used in both address to jump to and link register...
                        a = iris_get_register(proc, get_jump_reg2(inst));
                        iris_put_register(proc, get_jump_reg1(inst), proc->pc + 1);
                        proc->pc = a;
                     } else {
                        proc->pc = proc->pc + 1;
                     }
                  }
               } else {
                  /* non-link */
                  if (get_jump_immediate_flag(inst)) {
                     /* immediate */
                     if ((iris_get_register(proc, get_jump_reg0(inst)) == 0)) {
                        proc->pc = get_jump_immediate(inst);
                     } else {
                        proc->pc = proc->pc + 1;
                     }

                  } else {
                     /* register */
                     if ((iris_get_register(proc, get_jump_reg0(inst)) == 0)) {
                        proc->pc = iris_get_register(proc, get_jump_reg1(inst));
                     } else {
                        proc->pc = proc->pc + 1;
                     }
                  }
               }
            }
            break;
         }
      case JumpOpIfThenElse:
         {
            byte onTrue = 0,
                 onFalse = 0;

            if (get_jump_true_false_flag(inst)) {
               /* true */
               onTrue = get_jump_reg1(inst);
               onFalse = get_jump_reg2(inst);
            } else {
               /* false, just reverse the registers internally so that we can
                * use the same code for both paths */
               onTrue = get_jump_reg2(inst);
               onFalse = get_jump_reg1(inst);
            }
            if (get_jump_link_flag(inst)) {
               a = iris_get_register(proc, PredicateRegisterIndex);
               /* do this second in case the predicate register is reg0  as
                * well.... */
               iris_put_register(proc, get_jump_reg0(inst), proc->pc + 1);
            } else {
               a = iris_get_register(proc, get_jump_reg0(inst));
            }
            if (a != 0) {
               proc->pc = iris_get_register(proc, onTrue);
            } else {
               proc->pc = iris_get_register(proc, onFalse);
            }
            break;
         }
      default:
         iris_error("invalid jump operation", ErrorInvalidJumpOperation);
         break;
   }
}

void iris_compare(iris_core* proc, instruction* inst) {
   /* grab the appropriate value */
   word tmp = 0;
   switch(get_compare_op(inst)) {
#define X(class, symbol) \
      case class: \
                  tmp = (iris_get_register(proc, get_compare_reg1(inst))) symbol (iris_get_register(proc, get_compare_reg2(inst))); \
      break; 
#include "moveops.def"
#undef X
      default:
         iris_error("invalid compare operation", ErrorInvalidCompareOperation);
   }
   switch(get_combine_op(inst)) {
      case CombineOpSet:
         iris_put_register(proc, get_compare_reg0(inst), tmp);
         break;
      case CombineOpAnd:
         iris_put_register(proc, get_compare_reg0(inst), tmp & iris_get_register(proc, get_compare_reg0(inst)));
         break;
      case CombineOpOr:
         iris_put_register(proc, get_compare_reg0(inst), tmp | iris_get_register(proc, get_compare_reg0(inst)));
         break;
      case CombineOpXor:
         iris_put_register(proc, get_compare_reg0(inst), tmp ^ iris_get_register(proc, get_compare_reg0(inst)));
         break;
      default:
         iris_error("invalid combine operation", ErrorInvalidCombineOperation);
   }
}
static void iris_system_call(iris_core* proc, instruction* j);
void iris_misc(iris_core* proc, instruction* j) {
   /* implement system commands */
   switch(get_misc_op(j)) {
      case MiscOpSystemCall:
         iris_system_call(proc, j);
         break;
      default:
         iris_error("invalid misc operation", ErrorInvalidMiscOperation);
   }
}

void iris_system_call(iris_core* proc, instruction* j) {
   byte reg0 = get_misc_reg0(j);
   //byte reg1 = get_misc_reg1(j); // currently unused
   switch(get_misc_index(j)) {
      case SystemCommandTerminate: /* init 0 */
         proc->terminateexecution = 1;
         break;
      case SystemCommandGetC:
         iris_put_register(proc, reg0, (word)getchar());
         break;
      case SystemCommandPutC:
         putchar((word)iris_get_register(proc, reg0));
         break;
      default:
         iris_error("invalid system command provided", ErrorInvalidSystemCommand);
   }
}

void iris_error(char* message, int code) {
   fprintf(stderr, "%s\n", message);
   exit(code);
}

void iris_rom_init(iris_core* proc) {
   proc->pc = 0;
   proc->terminateexecution = 0;
   proc->advancepc = 1;
   /* clear out processor space to be sure everything is okay */
   for(int i = 0; i < RegisterCount; i++) {
      proc->gpr[i] = 0;
   }
   for(int i = 0; i < MemorySize; ++i) {
      proc->data[i] = 0;
      proc->stack[i] = 0;
      proc->code[i] = 0;
   }
   /* by default we use these registers for stack and predicate so set them as such */
   proc->gpr[PredicateRegisterIndex] = 0;
   proc->gpr[StackPointerRegisterIndex] = 0xFFFF;
}
static void iris_push_onto_stack(iris_core* proc, word value);
static word iris_pop_off_stack(iris_core* proc);

void iris_push(iris_core* proc, instruction* inst) {
   byte index;
   word value;
   index = get_reg0(inst);
   value = iris_get_register(proc, index);
   iris_push_onto_stack(proc, value);
}
void iris_push_immediate(iris_core* proc, instruction* inst) {
   word value;
   value = get_immediate(inst);
   iris_push_onto_stack(proc, value);
}
void iris_pop(iris_core* proc, instruction* inst) {
   iris_put_register(proc, get_reg0(inst), iris_pop_off_stack(proc));
}

void iris_push_onto_stack(iris_core* proc, word value) {
   word index = iris_get_register(proc, StackPointerRegisterIndex);
   /* increment and then set */
   index++;
   proc->stack[index] = value;
   iris_put_register(proc, StackPointerRegisterIndex, index);
}

word iris_pop_off_stack(iris_core* proc) {
   word index = iris_get_register(proc, StackPointerRegisterIndex);
   /* get the value and then decrement */
   word value = proc->stack[index];
   index--;
   iris_put_register(proc, StackPointerRegisterIndex, index);
   return value;
}

void iris_shutdown(iris_core* c) {
   /* do nothing right now */
}
void iris_new_core(iris_core* proc, iris_memory_map* mm) {
   proc->memory = calloc(mm->memorysize, sizeof(byte));
   proc->memorysize = mm->memorysize;
   proc->code = (instruction*)(proc->memory + mm->codestart);
   proc->codesize = mm->codesize;
   proc->data = (proc->memory + mm->datastart);
   proc->datasize = mm->datasize;
}

/* vim: set expandtab tabstop=3 shiftwidth=3: */

