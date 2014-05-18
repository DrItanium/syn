/* decode.c - decodes a stream of 32-bit numbers into an iris command */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "iris.h"
static int readinstruction(FILE* input, instruction* container);
int main() {
   instruction curr;
   int i;
   char buffer[80];
   for(i = 0; i < 80; i++) {
      buffer[i] = 0;
   }
   while(readinstruction(stdin, &curr)) {
      iris_unparse(buffer, &curr);
      printf("%s\n", buffer);
   }
   return 0;
}

int readinstruction(FILE* input, instruction* container) {
   int a, b, c, d;
   a = fgetc(input);
   b = fgetc(input);
   c = fgetc(input);
   d = fgetc(input);
   if(a == EOF || b == EOF || c == EOF || d == EOF) {
      return 0;
   } else {
      container->full = (((d & 0x000000FF) << 24) + ((c & 0x000000FF) << 16) + 
            ((b & 0x000000FF) << 8) + (a & 0x000000FF));

      return 1;
   }

}
