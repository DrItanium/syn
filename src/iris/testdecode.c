/* testdecode.c - Runs simple tests to make sure we haven't broken the decoder
 * logic */
#include <stdio.h>
#include <stdlib.h>
#include "iris.h"
char* fieldNames[] = {
   "group",
   "arithmetic.op",
   "arithmetic.dest",
   "arithmetic.source0",
   "arithmetic.source1",
   "move.op",
   "move.reg0",
   "move.immediate",
   "move.reg1",
   "move.reg2",
   "move.accessmode",
   "jump.distance",
   "jump.conditional",
   "jump.immediatemode",
   "jump.signedmode",
   "jump.immediate",
   "jump.reg0",
   "jump.reg1",
   "jump.reg1issigned",
   "compare.op",
   "compare.reg0",
   "compare.reg1",
   "compare.combinebits",
   "system.operation",
   "system.reg0",
   "system.reg1",
};
datum control[] = {
   0x7, /* group */
   /* arithmetic */
   0x78, /* op */
   0x380, /* dest */
   0x1C00, /* source0 */
   0xE000, /* source1 */
   /* move */
   0x18, /* op */
   0xE0, /* reg0 */
   0xFF00, /* immediate */
   0x700, /* reg1 */
   0x3800, /* reg2 */
   0x4000, /* accessmode */
   /* jump */
   0x8, /* jumpdistance */
   0x30, /* jumpconditional */
   0x40, /* jumpimmediatemode */
   0x80, /* signedmode */
   0xFF00, /* immediate */
   0x700, /* reg0 */
   0x3800, /* reg1 */
   0x4000, /* reg1issigned */
   /* compare */
   0x38, /* op */
   0x1C0, /* reg0 */
   0xE00, /* reg1 */
   0x7000, /* combinebits */
   /* system */
   0x3F8, /* operation */
   0x1C00, /* reg0 */
   0xE000, /* reg1 */
};
byte widths[] = {
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

typedef struct testcase {
   int index;
   int width;
} testcase;

int cases[] = {
   /* store only the widths */
   /* index of cases implies target field */
   3, /* group */
   /* arithmetic */
   4, /* op */
   3, /* dest */
   3, /* source0 */
   3, /* source1 */
   /* move */
   2, /* op */
   3, /* reg0 */
   8, /* immediate */
   3, /* reg1 */
   3, /* reg2 */
   1, /* accessmode */
   /* jump */
   1, /* distance */
   2, /* conditional */
   1, /* immediatemode */
   1, /* signedmode */
   8, /* immediate */
   3, /* reg0 */
   3, /* reg1 */
   1, /* reg1issigned */
   /* compare */
   3, /* op */
   3, /* reg0 */
   3, /* reg1 */
   3, /* combinebits */
   /* system */
   7, /* operation */
   3, /* reg0 */
   3, /* reg1 */
   /* terminator, this must be last */
   0,
};
static int checkandreport(testcase* test);
int main() {
   testcase j;
   int index, passCount, failCount;
   passCount = 0;
   failCount = 0;
   for(index = 0; cases[index] != 0; index++) {
      j.index = index;
      j.width = cases[index];
      if(checkandreport(&j)) {
         passCount++; 
      } else {
         failCount++;
      }
   }
   printf("Number of cases: %d\n", index);
   printf("Passing cases: %d\n", passCount);
   printf("Failing cases: %d\n", failCount);
   return 0;
}

int checkandreport(testcase* test) {
   datum value;
   char* fieldName;
   byte result;
   byte against;
   byte compare;
   fieldName = fieldNames[test->index];
   value = control[test->index];
   against = widths[test->width];
   switch(test->index) {
      case 0: result = getgroup(value); break;
      case 1: result = getarithmeticop(value); break;
      case 2: result = getarithmeticdest(value); break;
      case 3: result = getarithmeticsource0(value); break;
      case 4: result = getarithmeticsource1(value); break;
      case 5: result = getmoveop(value); break;
      case 6: result = getmovereg0(value); break;
      case 7: result = getmoveimmediate(value); break;
      case 8: result = getmovereg1(value); break;
      case 9: result = getmovereg2(value); break;
      case 10: result = getmoveaccessmode(value); break;
      case 11: result = getjumpdistance(value); break;
      case 12: result = getjumpconditional(value); break;
      case 13: result = getjumpimmediatemode(value); break;
      case 14: result = getjumpsignedmode(value); break;
      case 15: result = getjumpimmediate(value); break;
      case 16: result = getjumpreg0(value); break;
      case 17: result = getjumpreg1(value); break;
      case 18: result = getjumpreg1issigned(value); break;
      case 19: result = getcompareop(value); break;
      case 20: result = getcomparereg0(value); break;
      case 21: result = getcomparereg1(value); break;
      case 22: result = getcomparecombinebits(value); break;
      case 23: result = getsystemoperation(value); break;
      case 24: result = getsystemreg0(value); break;
      case 25: result = getsystemreg1(value); break;
      default: printf("%s\n", "Error: unknown test case"); return 0;
   }
   compare = (result == against) % 2;
   printf("field: %s value: %d result: %d against: %d status: %s\n",
         fieldName, value, result, against, strings[compare]);
   return compare;
}

void irissystem(core* proc, datum j) { }
