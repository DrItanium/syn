#include <u.h>
#include <libc.h>
#include <stdio.h>
#include "processor.h"

void main() {
   processor proc;
   instruction i;
   i.value = 12345;
   printf("sizeof(instruction) == %ld\n", sizeof(instruction));

   exits(0);
}
void decode(processor* proc, ushort value) {
   instruction i;
   i.value = value;
   switch(i.opgroup) {
      case InstructionGroupArithmetic:
         arithmetic(proc, i);
         break;
      case InstructionGroupMove:
         move(proc, i);
         break;
      case InstructionGroupJump:
         move(proc, i);
         break;
      case InstructionGroupCompare:
         compare(proc, i);
         break;
      default:
         sysfatal("panic: invalid instruction group provided");
         exits("invalidinstructiongroup");
   }
}
void putregister(processor* proc, byte index, byte value) {
   if(index >= 0 && index < RegisterCount) {
      proc->gpr[index] = value;
   } else {
      sysfatal("panic: attempted to store to a register out of range");
      exits("registerputoutofrange");
   }
}
byte getregister(processor* proc, byte index) {
   if(index >= 0 && index < RegisterCount) {
      return proc->gpr[index];
   } else {
      sysfatal("panic: attempted to retrieve a value from a register out of range");
      exits("registergetoutofrange");
      return 0;
   }
}
void arithmetic(processor* proc, instruction inst) {
   switch(inst.arithmetic.op) {
      case ArithmeticOpAdd:
         putregister(proc, inst.arithmetic.dest, 
               (getregister(proc, inst.arithmetic.source0) +
                getregister(proc, inst.arithmetic.source1)));
         break;
      case ArithmeticOpSub:
         putregister(proc, inst.arithmetic.dest, 
               (getregister(proc, inst.arithmetic.source0) -
                getregister(proc, inst.arithmetic.source1)));
         break;
      case ArithmeticOpMul:
         putregister(proc, inst.arithmetic.dest, 
               (getregister(proc, inst.arithmetic.source0) *
                getregister(proc, inst.arithmetic.source1)));
         break;
      case ArithmeticOpDiv:
         putregister(proc, inst.arithmetic.dest, 
               (getregister(proc, inst.arithmetic.source0) /
                getregister(proc, inst.arithmetic.source1)));
         break;
      case ArithmeticOpRem:
         putregister(proc, inst.arithmetic.dest, 
               (getregister(proc, inst.arithmetic.source0) %
                getregister(proc, inst.arithmetic.source1)));
         break;
      case ArithmeticOpShiftLeft:
         putregister(proc, inst.arithmetic.dest, 
               (getregister(proc, inst.arithmetic.source0) << 
                getregister(proc, inst.arithmetic.source1)));
         break;
      case ArithmeticOpShiftRight:
         putregister(proc, inst.arithmetic.dest, 
               (getregister(proc, inst.arithmetic.source0) >>
                getregister(proc, inst.arithmetic.source1)));
         break;
      case ArithmeticOpBinaryAnd:
         putregister(proc, inst.arithmetic.dest, 
               (getregister(proc, inst.arithmetic.source0) &
                getregister(proc, inst.arithmetic.source1)));
         break;
      case ArithmeticOpBinaryOr:
         putregister(proc, inst.arithmetic.dest, 
               (getregister(proc, inst.arithmetic.source0) |
                getregister(proc, inst.arithmetic.source1)));
         break;
      case ArithmeticOpBinaryNot:
         putregister(proc, inst.arithmetic.dest, ~getregister(proc, inst.arithmetic.source0));
         break;
      case ArithmeticOpBinaryXor:
         putregister(proc, inst.arithmetic.dest, 
               (getregister(proc, inst.arithmetic.source0) ^
                getregister(proc, inst.arithmetic.source1)));
         break;
      default:
         sysfatal("panic: invalid arithmetic operation");
         exits("invalidarithmeticoperation");
   }
}
void move(processor* proc, instruction inst) {
   ushort tmp;
   switch(inst.move.op) {
      case MoveOpRegToReg:
         putregister(proc, inst.move.reg0, getregister(proc, inst.move.regtoregmode.reg1));
         break;
      case MoveOpImmediateToReg:
         putregister(proc, inst.move.reg0, inst.move.immediate);
         break;
      case MoveOpRegToAddress:
         tmp = (ushort)(((ushort)getregister(proc, inst.move.addressmode.reg2)) << 8);
         tmp += getregister(proc, inst.move.addressmode.reg1);
         putregister(proc, inst.move.reg0, proc->memory[tmp]);
         break;
      default:
         sysfatal("panic: invalid move operation conditional type");
         exits("invalidmoveoperationconditionaltype");
   }
}
void jump(processor* proc, instruction inst) {
   schar address;
   byte shouldJump;
   ushort v0;
   switch(inst.jump.conditional) {
      case JumpOpUnconditional:
         shouldJump = 1;
         break;
      case JumpOpIfTrue:
         shouldJump = (proc->predicateregister != 0);
         break;
      case JumpOpIfFalse:
         shouldJump = (proc->predicateregister == 0);
         break;
      default:
         sysfatal("panic: invalid jump conditional type");
         exits("invalidjumpconditionaltype");
   }
   if(shouldJump == 1) {
      if(inst.jump.distance == JumpDistanceShort) {
         /* short form (relative) */
         if(inst.jump.shortform.immediatemode == 0) {
            /* register mode */
            address = getregister(proc, inst.jump.shortform.reg1);
         } else {
            address = inst.jump.shortform.immediate;
         }
         proc->pc += address;
      } else {
         /* long form (absolute) */ 
         /* little endian */
         v0 = (ushort)((ushort)getregister(proc, inst.jump.longtype.reg0) << 8);
         v0 += getregister(proc, inst.jump.longtype.reg1);
         proc->pc = v0;
      }
   }


}
void compare(processor* proc, instruction inst) {
   byte value;
   switch(inst.compare.op) {
      case CompareOpEq:
         value = (getregister(proc, inst.compare.reg0) ==
               getregister(proc, inst.compare.reg1));
         break;
      case CompareOpNeq:
         value = (getregister(proc, inst.compare.reg0) !=
               getregister(proc, inst.compare.reg1));
         break;
      case CompareOpLessThan:
         value = (getregister(proc, inst.compare.reg0) < 
               getregister(proc, inst.compare.reg1));
         break;
      case CompareOpGreaterThan:
         value = (getregister(proc, inst.compare.reg0) > 
               getregister(proc, inst.compare.reg1));
         break;
      case CompareOpLessThanOrEqualTo:
         value = (getregister(proc, inst.compare.reg0) <= 
               getregister(proc, inst.compare.reg1));
         break;
      case CompareOpGreaterThanOrEqualTo:
         value = (getregister(proc, inst.compare.reg0) >= 
               getregister(proc, inst.compare.reg1));
         break;
      default:
         sysfatal("panic: invalid compare operation");
         exits("invalidcompareopcode");
   }

   switch(inst.compare.combinebits) {
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
         sysfatal("panic: invalid compare combine bits");
         exits("invalidcomparecombinebits");
   }
}


