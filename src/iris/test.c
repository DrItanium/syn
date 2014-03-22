#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "iris.h"

int main() {
   ushort value;
   datum d;
   instruction i;
   value = 0x0400; // 0000 0100 0000 0000
   d.value = 0;
   i.value = 0;
   d.value = value;
   i.value = d.rest;
   char c = 0x80;
   unsigned char c2 = 0x80;
   printf("c = 0x80 = %d\n", c);
   printf("c2 = 0x80 = %d\n", c2);
   printf("sizeof(instruction) = %d\n", sizeof(instruction));
   printf("sizeof(datum) = %d\n", sizeof(datum));
   printf("value = %d\n", value);
   printf("d.{value,group,rest} = %d, %d, %d\n", d.value, d.group, d.rest);
   printf("i.value = %d\n", i.value);

   printf("i.arithmetic.{op,dest,source0,source1} = %d, %d, %d, %d\n", i.arithmetic.op, i.arithmetic.dest, i.arithmetic.source0, i.arithmetic.source1);

   printf("i.move.{op,reg0,immediate,reg1,addressmode.{reg1,reg2,accessmode}} = %d, %d, %d, %d, %d, %d, %d\n", i.move.op, i.move.reg0, i.move.immediate, i.move.reg1, i.move.addressmode.reg1, i.move.addressmode.reg2, i.move.addressmode.accessmode);

   printf("i.jump.{distance,conditional,immediatemode,signedmode,immediate,shortform.reg1,longtype.{reg0,reg1},ifthenelsetype.{reg0,reg1issigned,reg1}} = %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d\n", i.jump.distance, i.jump.conditional, i.jump.immediatemode, i.jump.signedmode, i.jump.immediate, i.jump.shortform.reg1, i.jump.longtype.reg0, i.jump.longtype.reg1, i.jump.ifthenelsetype.reg0, i.jump.ifthenelsetype.reg1issigned, i.jump.ifthenelsetype.reg1);

   printf("i.compare.{op,reg0,reg1,combinbits} = %d, %d, %d, %d\n", i.compare.op, i.compare.reg0, i.compare.reg1, i.compare.combinebits);

   return 0;
}

void irissystem(core* core, instruction j) {

}
