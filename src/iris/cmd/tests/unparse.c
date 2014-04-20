#include <stdio.h>
#include <stdlib.h>
#include "iris.h"
typedef struct container {
   byte a;
   byte b;
   byte c;
} container;
#define LENGTH(X) (sizeof(X) / (sizeof(X[0])))
container values[] = {
   { 0x00, 0x01, 0x02 },
   { 0xFF, 0xFD, 0xED },
   { 0xCA, 0xFE, 0xBA },
   { 0xBE, 0xDE, 0xAD },
   { 0xBE, 0xEF, 0x1A },
   { 0x03, 0x04, 0x05 },
   { 0x06, 0x07, 0x08 },
   { 0xF0, 0x0F, 0xAB },
   { 0xCD, 0xEF, 0x01 },
   { 0x23, 0x45, 0x67 },
   { 0x89, 0xAB, 0xCD },
   { 0xEF, 0x0F, 0xED },
   { 0xCB, 0xA9, 0x87 },
   { 0x65, 0x43, 0x21 },
};
int main() {
   char unparsed[100];
   instruction insn;
   int k, len, i;
   len = LENGTH(values);
   insn.full = 0;
   /* limited test cases */
   for(i = 0; i < 256; i++) {
      insn.bytes[0] = i;
      for(k = 0; k < len; k++) {
         insn.bytes[1] = values[k].a;
         insn.bytes[2] = values[k].b;
         insn.bytes[3] = values[k].c;
         unparse_bitstring(unparsed, &insn);
         printf("%s : ", unparsed);
         unparse(unparsed, &insn);
         printf("%s\n", unparsed);
      }
   }
   return 0;
}
