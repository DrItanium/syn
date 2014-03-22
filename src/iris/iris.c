#include <stdlib.h>
#include <stdio.h>
#include "iris.h"

void decode(core* proc, ushort value) {
   byte group;
   group = getgroup(value);
   switch(group) {
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
      case InstructionGroupSystem:
         irissystem(proc, value);
         break;
      default:
         error("invalid instruction group provided", ErrorInvalidInstructionGroupProvided);
   }
}

void putregister(core* proc, byte index, byte value) {
   if(index < RegisterCount) {
      proc->gpr[index] = value;
   } else {
      error("attempted to store to a register out of range", ErrorPutRegisterOutOfRange);
   }
}

byte getregister(core* proc, byte index) {
   if(index < RegisterCount) {
      return proc->gpr[index];
   } else {
      error("attempted to retrieve a value from a register out of range", ErrorGetRegisterOutOfRange);
      return 0;
   }
}

void arithmetic(core* proc, datum inst) {
   byte arithmeticOp, dest, source0, source1;
   arithmeticOp = getarithmeticop(inst);
   dest = getarithmeticdest(inst);
   source0 = getarithmeticsource0(inst);
   source1 = getarithmeticsource1(inst);
   switch(arithmeticOp) {
      case ArithmeticOpAdd:
         putregister(proc, dest,
               (getregister(proc, source0) +
                getregister(proc, source1)));
         break;
      case ArithmeticOpSub:
         putregister(proc, dest, 
               (getregister(proc, source0) -
                getregister(proc, source1)));
         break;
      case ArithmeticOpMul:
         putregister(proc, dest, 
               (getregister(proc, source0) *
                getregister(proc, source1)));
         break;
      case ArithmeticOpDiv:
         putregister(proc, dest, 
               (getregister(proc, source0) /
                getregister(proc, source1)));
         break;
      case ArithmeticOpRem:
         putregister(proc, dest, 
               (getregister(proc, source0) %
                getregister(proc, source1)));
         break;
      case ArithmeticOpShiftLeft:
         putregister(proc, dest, 
               (getregister(proc, source0) << 
                getregister(proc, source1)));
         break;
      case ArithmeticOpShiftRight:
         putregister(proc, dest, 
               (getregister(proc, source0) >>
                getregister(proc, source1)));
         break;
      case ArithmeticOpBinaryAnd:
         putregister(proc, dest, 
               (getregister(proc, source0) &
                getregister(proc, source1)));
         break;
      case ArithmeticOpBinaryOr:
         putregister(proc, dest, 
               (getregister(proc, source0) |
                getregister(proc, source1)));
         break;
      case ArithmeticOpBinaryNot:
         putregister(proc, dest, ~getregister(proc, source0));
         break;
      case ArithmeticOpBinaryXor:
         putregister(proc, dest, 
               (getregister(proc, source0) ^
                getregister(proc, source1)));
         break;
      default:
         error("invalid arithmetic operation", ErrorInvalidArithmeticOperation);
   }
}
void move(core* proc, datum inst) {
   ushort tmp;
   byte op;
   op = getmoveop(inst);
   switch(op) {
      case MoveOpRegToReg:
         putregister(proc, getmovereg0(inst), getregister(proc, getmovereg1(inst)));
         break;
      case MoveOpImmediateToReg:
         putregister(proc, getmovereg0(inst), getmoveimmediate(inst));
         break;
      case MoveOpRegToAddress:
         tmp = (ushort)(((ushort)getregister(proc, getmovereg2(inst))) << 8);
         tmp += getregister(proc, getmovereg1(inst));
         if(getmoveaccessmode(inst) == AccessModeMoveOpLoad) {
            putregister(proc, getmovereg0(inst), proc->memory[tmp]);
         } else {
            proc->memory[tmp] = getregister(proc, getmovereg0(inst));
         }
         break;
      default:
         error("invalid move operation conditional type", ErrorInvalidMoveOperationConditionalType);
   }
}
void jump(core* proc, datum inst) {
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
   conditional = getjumpconditional(inst);
   immediatemode = getjumpimmediatemode(inst);

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
         if(getjumpdistance(inst) == JumpDistanceShort) {
            /* short form (relative) */
            if(immediatemode == 0) {
               /* register mode */
               address = getregister(proc, getjumpreg1(inst));
            } else {
               address = getjumpimmediate(inst);
            }
            if(getjumpsignedmode(inst) == 0) {
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
            v0 = (ushort)((ushort)getregister(proc, getjumpreg1(inst)) << 8);
            v0 += getregister(proc, getjumpreg0(inst));
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
         if(getjumpsignedmode(inst) == 1) {
            proc->pc += (schar)getregister(proc, getjumpreg0(inst));
         } else {
            proc->pc += getregister(proc, getjumpreg0(inst));
         }
      } else {
         if(getjumpreg1issigned(inst) == 1) {
            proc->pc += (schar)getregister(proc, getjumpreg1(inst));
         } else {
            proc->pc += getregister(proc, getjumpreg1(inst));
         }
      }
   }
}
void compare(core* proc, datum inst) {
   byte value, op, reg0, reg1, combine; 
   value = 0;
   op = getcompareop(inst);
   reg0 = getcomparereg0(inst);
   reg1 = getcomparereg1(inst);
   combine = getcomparecombinebits(inst);
   switch(op) {
      case CompareOpEq:
         value = (getregister(proc, reg0) ==
               getregister(proc, reg1));
         break;
      case CompareOpNeq:
         value = (getregister(proc, reg0) !=
               getregister(proc, reg1));
         break;
      case CompareOpLessThan:
         value = (getregister(proc, reg0) < 
               getregister(proc, reg1));
         break;
      case CompareOpGreaterThan:
         value = (getregister(proc, reg0) > 
               getregister(proc, reg1));
         break;
      case CompareOpLessThanOrEqualTo:
         value = (getregister(proc, reg0) <= 
               getregister(proc, reg1));
         break;
      case CompareOpGreaterThanOrEqualTo:
         value = (getregister(proc, reg0) >= 
               getregister(proc, reg1));
         break;
      default:
         error("invalid compare operation", ErrorInvalidCompareOperation);
   }

   switch(combine) {
      case CombineBitsOpNil:
         proc->predicateregister = value;
         break;
      case CombineBitsOpAnd:
         proc->predicateregister &= value;
         break;
      case CombineBitsOpOr:
         proc->predicateregister |= value;
         break;
      case CombineBitsOpXor:
         proc->predicateregister ^= value;
         break;
      default:
         error("invalid compare combine bits", ErrorInvalidCombineBits);
   }
}

void error(char* message, int code) {
   fprintf(stderr, "%s\n", message);
   exit(code);
}

