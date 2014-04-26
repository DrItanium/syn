#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "util.h"

void openfw(FileWrapper* fw) {
   FILE* tmp;
   tmp = fopen(fw->line, fw->permissions);
   if(!tmp) {
      fprintf(stderr, "couldn't open %s\n", fw->line);
      exit(errno);
   }
   fw->needsclosing = 1;
   fw->fptr = tmp;
}

void closefw(FileWrapper* fw) {
   if(fw->needsclosing && fclose(fw->fptr) != 0) {
      fprintf(stderr, "couldn't close %s\n", fw->line); 
      exit(errno);
   }
}
