#include <u.h>
#include <libc.h>
#include <stdio.h>
#include "processor.h"

void main() {
   processor proc;

   exits(0);
}
void putregister(processor* proc, byte index, byte value) {
   if(index >= 0 && index < 4) {
      proc->gpr[index] = value;
   } else if(index >= 4 && index < 8) {
      proc->memory[proc->gpr[index % 4]] = value;
   } else {
      sysfatal("panic: attempted to store to a register out of range");
      exits("registerputoutofrange");
   }
}
byte getregister(processor* proc, byte index) {
   if(index >= 0 && index < 4) {
      return proc->gpr[index];
   } else if(index >= 4 && index < 8) {
      return proc->memory[proc->gpr[index % 4]];
   } else {
      sysfatal("panic: attempted to retrieve a value from a register out of range");
      exits("registergetoutofrange");
      return 0;
   }
}
void arithmetic(processor* proc, instruction inst) {
   
}
void move(processor* proc, instruction inst) {

}

void jump(processor* proc, instruction inst) {
   switch(inst.jump.op) {
      case JumpOpUnconditional:
      case JumpOpIfTrue:
      case JumpOpIfFalse:
      default:
         sysfatal("panic: invalid jump type!");
         exits("invalidjumptype");
   }

}

void compare(processor* proc, instruction inst) {

}


