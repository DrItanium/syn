#include <stdlib.h>
#include <stdio.h>
#include "iris.h"

void decode(core* proc, instruction* value) {
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
         proc->advancepc = 0;
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

void move(core* proc, instruction* inst) {
   ushort tmp;
   switch(get_move_op(inst)) {
      case MoveOpRegToReg:
         put_register(proc, get_move_reg0(inst), get_register(proc, get_move_reg1(inst)));
         break;
      case MoveOpImmediateToReg:
         put_register(proc, get_move_reg0(inst), get_move_immediate(inst));
         break;
      case MoveOpRegToAddress:
         tmp = (ushort)(((ushort)get_register(proc, get_move_reg2(inst))) << 8);
         tmp += get_register(proc, get_move_reg1(inst));
         if(get_move_accessmode(inst) == AccessModeMoveOpLoad) {
            put_register(proc, get_move_reg0(inst), proc->data[tmp]);
         } else {
            proc->data[tmp] = get_register(proc, get_move_reg0(inst));
         }
         break;
      default:
         error("invalid move operation conditional type", ErrorInvalidMoveOperationConditionalType);
   }
}
void jump(core* proc, instruction* inst) {
   byte address;
   schar saddress;
   byte shouldJump;
   byte normalMode;
   byte conditional;
   byte immediatemode;
   ushort v0;
   address = 0;
   shouldJump = 0;
   v0 = 0;
   saddress = 0;
   normalMode = 1;
   conditional = get_jump_conditional(inst);
   immediatemode = get_jump_immediatemode(inst);

   switch(conditional) {
      case JumpOpUnconditional:
         shouldJump = 1;
         break;
      case JumpOpIfTrue:
         shouldJump = (proc->predicateregister != 0);
         break;
      case JumpOpIfFalse:
         shouldJump = (proc->predicateregister == 0);
         break;
      case JumpOpIfThenElse:
         normalMode = 0;
         shouldJump = proc->predicateregister;
         break;
      default:
         error("invalid jump conditional type", ErrorInvalidJumpConditionalType);
   }
   if(normalMode == 1) {
      if(shouldJump == 1) {
         if(get_jump_distance(inst) == JumpDistanceShort) {
            /* short form (relative) */
            if(immediatemode == 0) {
               /* register mode */
               address = get_register(proc, get_jump_reg1(inst));
            } else {
               address = get_jump_immediate(inst);
            }
            if(get_jump_signedmode(inst) == 0) {
               /* we are in unsigned mode */
               proc->pc += address;
            } else {
               /* we are in signed mode */
               saddress = (schar)address;
               proc->pc += saddress;
            }
         } else {
            /* long form (absolute) */ 
            /* little endian */
            v0 = (ushort)((ushort)get_register(proc, get_jump_reg1(inst)) << 8);
            v0 += get_register(proc, get_jump_reg0(inst));
            proc->pc = v0;
         }
      }
   } else {
      /* now this is going to get a tad confusing */
      if(immediatemode == JumpOpIfThenElse_TrueFalse) {
         shouldJump = (shouldJump != 0);
      } else if(immediatemode == JumpOpIfThenElse_FalseTrue) {
         shouldJump = (shouldJump == 0); 
      } else {
         /* this should never ever get executed! */
         error("invalid ifthenelse instruction type", ErrorInvalidIfThenElseInstructionType);
      }
      if(shouldJump == 1) {
         if(get_jump_signedmode(inst) == 1) {
            proc->pc += (schar)get_register(proc, get_jump_reg0(inst));
         } else {
            proc->pc += get_register(proc, get_jump_reg0(inst));
         }
      } else {
         if(get_jump_reg1issigned(inst) == 1) {
            proc->pc += (schar)get_register(proc, get_jump_reg1(inst));
         } else {
            proc->pc += get_register(proc, get_jump_reg1(inst));
         }
      }
   }
}
static void iris_system_call(core* proc, instruction* j);
static void iris_set_implicit_register_immediate(core* proc, instruction* j);
static void iris_set_implicit_register_indirect(core* proc, instruction* j);
static void iris_get_implicit_register_immediate(core* proc, instruction* j);
static void iris_get_implicit_register_indirect(core* proc, instruction* j);
void iris_misc(core* proc, instruction* j) {
   /* implement system commands */
   switch(get_misc_op(j)) {
      case MiscOpSystemCall:
         iris_system_call(proc, j);
         break;
      case MiscOpSetImplicitRegisterImmediate:
         iris_set_implicit_register_immediate(core, j);
         break;
      case MiscOpSetImplicitRegisterIndirect:
         iris_set_implicit_register_indirect(core, j);
         break;
      case MiscOpGetImplicitRegisterImmediate:
         iris_get_implicit_register_immediate(core, j);
         break;
      case MiscOpGetImplicitRegisterIndirect:
         iris_get_implicit_register_indirect(core, j);
         break;
      default:
         error("invalid misc operation", ErrorInvalidMiscCommand);
   }
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
