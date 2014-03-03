#include <u.h>
#include <libc.h>
#include <stdio.h>
#include "processor.h"

void main() {
   processor proc;
   datum d;
   instruction tmp;
   d.group = InstructionGroupCompare;
   tmp.compare.op = CompareOpEq;
   tmp.compare.reg0 = 0;
   tmp.compare.reg1 = 1;
   tmp.compare.combinebits = CombineBitsOpNil;
   d.rest = tmp.value;
   decode(&proc, d.value);
   printf("equality = %d\n", proc.predicateregister);

   exits(0);
}
void panic(char* message, char* code) {
   sysfatal(message);
   exits(code);
}
void decode(processor* proc, ushort value) {
   datum i;
   instruction j;
   i.value = value;
   j.value = i.rest;
   switch(i.group) {
      case InstructionGroupArithmetic:
         arithmetic(proc, j);
         break;
      case InstructionGroupMove:
         move(proc, j);
         break;
      case InstructionGroupJump:
         move(proc, j);
         break;
      case InstructionGroupCompare:
         compare(proc, j);
         break;
      default:
         panic("panic: invalid instruction group provided", 
               "invalidinstructiongroup");
   }
}
void putregister(processor* proc, byte index, byte value) {
   if(index < RegisterCount) {
      proc->gpr[index] = value;
   } else {
      panic("panic: attempted to store to a register out of range", 
            "registerputoutofrange");
   }
}
byte getregister(processor* proc, byte index) {
   if(index < RegisterCount) {
      return proc->gpr[index];
   } else {
      panic("panic: attempted to retrieve a value from a register out of range", 
            "registergetoutofrange");
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
         panic("panic: invalid arithmetic operation",
               "invalidarithmeticoperation");
   }
}
void move(processor* proc, instruction inst) {
   ushort tmp;
   switch(inst.move.op) {
      case MoveOpRegToReg:
         putregister(proc, inst.move.reg0, getregister(proc, inst.move.reg1));
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
         panic("panic: invalid move operation conditional type",
               "invalidmoveoperationconditionaltype");
   }
}
void jump(processor* proc, instruction inst) {
   schar address;
   byte shouldJump;
   ushort v0;
   address = 0;
   shouldJump = 0;
   v0 = 0;
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
         panic("panic: invalid jump conditional type", 
               "invalidjumpconditionaltype");
   }
   if(shouldJump == 1) {
      if(inst.jump.distance == JumpDistanceShort) {
         /* short form (relative) */
         if(inst.jump.immediatemode == 0) {
            /* register mode */
            address = getregister(proc, inst.jump.shortform.reg1);
         } else {
            address = inst.jump.immediate;
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
   value = 0;
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
         panic("panic: invalid compare operation", 
               "invalidcompareopcode");
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
         panic("panic: invalid compare combine bits", 
               "invalidcomparecombinebits");
   }
}
