#include <u.h>
#include <libc.h>
#include <stdio.h>
#include "core.h"

void main() {
   processor proc;
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

   exits(0);
}
