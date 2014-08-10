#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
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
   insn = 0;
   /* limited test cases */
   for(i = 0; i < 256; i++) {
      for(k = 0; k < len; k++) {
         insn = ((insn & ~0xFF000000) | (values[k].a << 24)) |
            ((insn & ~0x00FF0000) | (values[k].b << 16)) |
            ((insn & ~0x0000FF00) | (values[k].c << 8)) | 
            ((insn & ~0x000000FF) | i);
         iris_unparse_bitstring(unparsed, &insn);
         printf("%s : ", unparsed);
         iris_unparse(unparsed, &insn);
         printf("%s\n", unparsed);
      }
   }
   return 0;
}
/* vim: set expandtab tabstop=3 shiftwidth=3: */
