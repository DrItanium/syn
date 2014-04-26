/* asm.c - the iris assembler */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "iris.h"
#include "util.h"


enum {
   FileWrapperInput = 0,
   FileWrapperOutput,

   /* always last */
   FileWrapperCount,
};

FileWrapper files[] = {
   /* input */
   { 0, 0, "r", 0 },
   /* output */
   { 0, 0, "w", 0 },
};

#define outfile (files[FileWrapperOutput])
#define infile (files[FileWrapperInput])

#define outfile_ptr (&outfile)
#define infile_ptr (&infile)

static void usage(char* arg0);

int main(int argc, char* argv[]) {
   char* tmpline;
   int last, i, errorfree;
   /* make sure these are properly initialized */
   last = argc - 1;
   tmpline = 0;
   errorfree = 1;
   i = 0;
   if(argc > 1) {
      for(i = 1; errorfree && (i < last); ++i) {
         tmpline = argv[i];
         if(strlen(tmpline) == 2 && tmpline[0] == '-') {
            switch(tmpline[1]) {
               case 'o':
                  i++;
                  outfile.line = argv[i];
                  break;
               case 'h':
               default:
                  errorfree = 0;
                  break;
            }
         } else {
            errorfree = 0;
            break;
         }
      }
      if(errorfree) {
         if(i == last) {
            /* open the input file */
            tmpline = argv[i];
            if(strlen(tmpline) == 1 && tmpline[0] == '-') {
               infile.fptr = stdin;
            } else if(strlen(tmpline) >= 1 && tmpline[0] != '-') {
               infile.line = tmpline;
               openfw(infile_ptr);
            }

            /* open the output */
            if(!(outfile.line)) {
               outfile.line = "v.obj";
            }
            if(strlen(outfile.line) == 1 && (outfile.line)[0] == '-') {
               outfile.fptr = stdout; 
            } else {
               openfw(outfile_ptr);
            }
         } else {
            fprintf(stderr, "no file provided\n");
         }
      }
   }

   if(outfile.fptr && infile.fptr) {
      startup();
      execute(infile.fptr);
      shutdown(outfile.fptr);
      for(i = 0; i < FileWrapperCount; i++) {
         closefw(&(files[i]));
      }
   } else {
      usage(argv[0]);
   }
   return 0;
}

void usage(char* arg0) {
   fprintf(stderr, "usage: %s [-o <outfile>] <infile>\n", arg0);
}
