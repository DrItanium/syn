/* dbg.c - the iris debugger */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "iris.h"
#include "util.h"

enum {
   FileWrapperInput = 0,

   FileWrapperCount,
};

FileWrapper files[] = {
   /* input */
   { 0, 0, "r", 0},
};
#define infile (files[FileWrapperInput])
#define infile_ptr (&infile)
static void usage(char* arg0);
static int execute(FILE* file);
static void startup(void);
static void shutdown(void);
static void installprogram(FILE* file);
static core proc;
static byte breakpoints[MemorySize];


int main(int argc, char* argv[]) {
   char* line;
   char* tmpline;
   int last, i, errorfree;
   line = 0;
   last = argc - 1;
   tmpline = 0;
   errorfree = 1;
   if(argc > 1) {
      for(i = 1; errorfree && (i < last); ++i) {
         tmpline = argv[i];
         if(strlen(tmpline) == 2 && tmpline[0] == '-') {
            switch(tmpline[1]) {
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
            line = argv[last];
            if(strlen(line) == 1 && line[0] == '-') {
               infile.fptr = stdin;
            } else if(strlen(line) >= 1 && line[0] != '-') {
               infile.line = line;
               openfw(infile_ptr);
            }
         } else {
            fprintf(stderr, "no file provided\n");
         }
      }
   }

   if(infile.fptr) {
      startup();
      execute(infile.fptr);
      shutdown();
      for(i = 0; i < FileWrapperCount; i++) {
         closefw(&(files[i]));
      }
   } else {
      usage(argv[0]);
   }
   return 0;
}

void usage(char* arg0) {
   fprintf(stderr, "usage: %s -h | [<file> | -]\n", arg0);
}
int execute(FILE* file) {
   int i, c;
   char buffer[80];
   c = 0;
   for(i = 0; i < 80; i++) {
      buffer[i] = 0;
   }
   /* install the program to memory */
   installprogram(file); 
   /* pause execution at this point */
   do {
      if(i != 'c') {
         iris_unparse(buffer, &(proc.code[proc.pc]));
         printf("%s\n", buffer);
         i = getc(stdin); 
      }
      if(((char)i) == 'q') {
         break;
      } else {
         iris_decode(&proc, &proc.code[proc.pc]);
         if(proc.advancepc) {
            proc.pc++;
         }
         c++;
      }
   } while(!proc.terminateexecution);
   printf("instruction count: %d\n", c);
   return 0;
}
void startup() {
   int i;
   proc.pc = 0;
   proc.terminateexecution = 0;
   proc.advancepc = 1;
   for(i = 0; i < RegisterCount; i++) {
      proc.gpr[i] = 0;
   }
   for(i = 0; i < ImplicitRegisterPredicate; i++) {
      proc.impliedregisters[i] = 0;
   }
   for(i = 0; i < MemorySize; i++) {
      proc.data[i] = 0;
      proc.code[i].full = 0;
   }
}

void shutdown() {
   /* nothing to do at this point */
}

void installprogram(FILE* file) {
   /* read up to 64k of program information */
   int a, b, c, d, i;
   instruction tmp;
   for(i = 0; i < MemorySize; i++) {
      a = fgetc(file);
      b = fgetc(file);
      c = fgetc(file);
      d = fgetc(file);
      if(a == EOF || b == EOF || c == EOF || d == EOF) {
         break;
      } else {
         tmp.full = (((d & 0x000000FF) << 24) + ((c & 0x000000FF) << 16) + 
               ((b & 0x000000FF) << 8) + (a & 0x000000FF));
         proc.code[i] = tmp;
      }
   }
   if(i < MemorySize) {
      if(feof(file)) {
         fprintf(stderr,"End of the file was reached\nNo data section!\n");
      }
      if(ferror(file)) {
         fprintf(stderr, "Error occurred during code section load: %d\n", errno);
         exit(errno);
      }
   } else {
      for(i = 0; i < MemorySize; i++) {
         a = fgetc(file);
         b = fgetc(file);
         if(a == EOF || b == EOF) {
            break;
         } else {
            proc.data[i] = ((b & 0x000000FF) << 8) + (a & 0x000000FF);
         }
      }
      if(i < MemorySize) {
         if(feof(file)) {
            fprintf(stderr, "End of file was reached before end of data section\n");
         }
         if(ferror(file)) {
            fprintf(stderr, "Error occurred during data section load: %d\n", errno);
            exit(errno);
         }
      } else {
         if(fgetc(file) != EOF) {
            fprintf(stderr, "warning: image contains more data than is loadable\n");
         }
      }
   }
}
