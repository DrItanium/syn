#include <stdio.h>
#include <stdlib.h>
#include "../iris.h"
#include "../unparse.h"

int main() {
   char unparsed[100];
   ushort insn;

   for(insn = 0; insn < 65535; insn++) {
      unparse_bitstring(unparsed, insn);
      printf("%s : ", unparsed);
      unparse(unparsed, insn);
      printf("%s\n", unparsed);
   }
   unparse_bitstring(unparsed, insn);
   printf("%s : ", unparsed);
   unparse(unparsed, insn);
   printf("%s\n", unparsed);

   return 0;
}
