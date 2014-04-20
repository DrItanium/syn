/* Runs simple tests to make sure we haven't broken the decoder logic */
#include <stdio.h>
#include <stdlib.h>
#include "iris.h"
typedef struct testcase {
   const char* name;
   uint control;
   ushort compare;
} testcase;
#define LENGTH(X) (sizeof(X) / (sizeof(X[0])))
testcase tests [] = {
   { "group", 0x00000006, 0x6 },
   { "op", 0x0000001A, 0x1A, },
   { "reg0", 0x0000F000, 0xF0, },
   { "reg1", 0x00DE0000, 0xDE, },
   { "reg2", 0xAD000000, 0xAD, },
   { "immediate", 0xFDED0000, 0xFDED, },
};

char* strings[] = {
   "FAIL",
   "PASS",
};

static int checkandreport(int tc);
int main() {
   int i, passCount, failCount, length;
   passCount = 0;
   failCount = 0;
   length = LENGTH(tests);
   for(i = 0; i < length; i++) {
      if(checkandreport(i)) {
         passCount++; 
      } else {
         failCount++;
      }
   }
   printf("Number of cases: %d\n", i);
   printf("Passing cases: %d\n", passCount);
   printf("Failing cases: %d\n", failCount);
   return 0;
}

int checkandreport(int tc) {
   testcase z;
   instruction value;
   int compare;
   ushort result;
   compare = 0;
   z = tests[tc];
   result = 0;
   value.full = z.control;
   switch(tc) {
#define defcase(ind, op) case ind: result = op(&value); break
      defcase(0, get_group);
      defcase(1, get_op);
      defcase(2, get_reg0);
      defcase(3, get_reg1);
      defcase(4, get_reg2);
      defcase(5, get_immediate);
#undef defcase
      default: printf("%s\n", "Error: unknown test case"); return 0;
   }
   compare = (result == z.compare) % 2;
   printf("field: %s value: %d result: %d against: %d status: %s\n",
         z.name, z.control, result, z.compare, strings[compare]);
   return compare;
}
