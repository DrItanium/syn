#include <stdio.h>
#include <stdlib.h>
#include "../iris.h"
#include "../unparse.h"

int main() {
   char unparsed[100];
   datum d;
   ushort value;

   for(value = 0; value < 65535; value++) {
      d.value = value;
      unparse_bitstring(unparsed, d);
      printf("%s : ", unparsed);
      unparse(unparsed, d);
      printf("%s\n", unparsed);
   }
   unparse_bitstring(unparsed, d);
   printf("%s : ", unparsed);
   unparse(unparsed, d);
   printf("%s\n", unparsed);

   return 0;
}
