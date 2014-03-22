/* testdecode.c - Runs simple tests to make sure we haven't broken the decoder
 * logic */
#include <stdio.h>
#include <stdlib.h>
#include "iris.h"
datum control[] = {
   0x7, /* group */
   /* arithmetic */
   0x78, /* op */
   0x380, /* dest */
   0x1C00, /* source0 */
   0xE000, /* source1 */
};
byte results[] = {
   /* max values for each bit width */
   0x0,
   0x1,
   0x3,
   0x7, 
   0xF,
   0x1F,
   0x3F,
   0x7F,
   0xFF,
};
char* strings[] = {
   "FAIL",
   "PASS",
};
enum {
   ControlGroup = 0,
   ControlArithmeticOp,
   ControlArithmeticDest,
   ControlArithmeticSource0,
   ControlArithmeticSource1,
};

static int checkandreport(char* field, byte value, byte against);

int main() {
   datum j;
   j = control[ControlGroup];
   checkandreport("group", getgroup(j), results[3]);


   return 0;
}




void irissystem(core* proc, datum j) { }
int checkandreport(char* field, byte value, byte against) {
   int result = (value == against);
   printf("field %s: value: %d control: %d result: %s\n", 
         field, value, against, strings[result]);
   return result;
}
