#include <stdlib.h>
#include <stdio.h>
#include "iris.h"
#include "unparse.h"

/*
int main() {
   core proc;
   datum d;
   instruction tmp;
   d.group = InstructionGroupCompare;
   tmp.compare.op = CompareOpEq;
   tmp.compare.reg0 = 7;
   tmp.compare.reg1 = 5;
   tmp.compare.combinebits = CombineBitsOpNil;
   d.rest = tmp.value;
   decode(&proc, d.value);
   printf("equality = %d\n", proc.predicateregister);
   printf("sizeof(instruction) = %ld\n", sizeof(instruction));

   return 0;
}
*/

int main() {
   char unparsed[20];
   ushort inst;

   for(inst = 0; inst < 65535; inst++) {
      unparse(unparsed, inst);
      printf("%s\n", unparsed);
   }

   return 0;
}

void decode(core* proc, ushort value) {
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
         jump(proc, j);
         break;
      case InstructionGroupCompare:
         compare(proc, j);
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
void arithmetic(core* proc, instruction inst) {
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
         error("invalid arithmetic operation", ErrorInvalidArithmeticOperation);
   }
}
void move(core* proc, instruction inst) {
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
         if(inst.move.addressmode.accessmode == AccessModeMoveOpLoad) {
            putregister(proc, inst.move.reg0, proc->memory[tmp]);
         } else {
            proc->memory[tmp] = inst.move.reg0;
         }
         break;
      default:
         error("invalid move operation conditional type", ErrorInvalidMoveOperationConditionalType);
   }
}
void jump(core* proc, instruction inst) {
   byte address;
   schar saddress;
   byte shouldJump;
   byte normalMode;
   ushort v0;
   address = 0;
   shouldJump = 0;
   v0 = 0;
   saddress = 0;
   normalMode = 1;
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
      case JumpOpIfThenElse:
         normalMode = 0;
         shouldJump = proc->predicateregister;
         break;
      default:
         error("invalid jump conditional type", ErrorInvalidJumpConditionalType);
   }
   if(normalMode == 1) {
      if(shouldJump == 1) {
         if(inst.jump.distance == JumpDistanceShort) {
            /* short form (relative) */
            if(inst.jump.immediatemode == 0) {
               /* register mode */
               address = getregister(proc, inst.jump.shortform.reg1);
            } else {
               address = inst.jump.immediate;
            }
            if(inst.jump.signedmode == 0) {
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
            v0 = (ushort)((ushort)getregister(proc, inst.jump.longtype.reg1) << 8);
            v0 += getregister(proc, inst.jump.longtype.reg0);
            proc->pc = v0;
         }
      }
   } else {
      /* now this is going to get a tad confusing */
      if(inst.jump.immediatemode == JumpOpIfThenElse_TrueFalse) {
         shouldJump = (shouldJump != 0);
      } else if(inst.jump.immediatemode == JumpOpIfThenElse_FalseTrue) {
         shouldJump = (shouldJump == 0); 
      } else {
         /* this should never ever get executed! */
         error("invalid ifthenelse instruction type", ErrorInvalidIfThenElseInstructionType);
      }
      if(shouldJump == 1) {
         if(inst.jump.signedmode == 1) {
            proc->pc += (schar)getregister(proc, inst.jump.ifthenelsetype.reg0);
         } else {
            proc->pc += getregister(proc, inst.jump.ifthenelsetype.reg0);
         }
      } else {
         if(inst.jump.ifthenelsetype.reg1issigned == 1) {
            proc->pc += (schar)getregister(proc, inst.jump.ifthenelsetype.reg1);
         } else {
            proc->pc += getregister(proc, inst.jump.ifthenelsetype.reg1);
         }
      }
   }
}
void compare(core* proc, instruction inst) {
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
         error("invalid compare operation", ErrorInvalidCompareOperation);
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
         error("invalid compare combine bits", ErrorInvalidCombineBits);
   }
}

void error(char* message, int code) {
   fprintf(stderr, "%s\n", message);
   exit(code);
}
