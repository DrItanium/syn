/* img.c - the iris image creator */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "iris.h"

typedef struct FileWrapper {
   FILE* fptr;
   char* line;
   char* permissions;
   int needsclosing;
} FileWrapper;

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
static int execute(FILE* file);
static void startup(void);
static void shutdown(FILE* output);
static void installprogram(FILE* file);
static core proc;
static void openfw(FileWrapper* fw);
static void closefw(FileWrapper* fw);

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
            /* open the output */
            if(!(outfile.line)) {
               outfile.line = "v.obj";
            }
            if(strlen(outfile.line) == 1 && (outfile.line)[0] == '-') {
               outfile.fptr = stdout; 
            } else {
               openfw(outfile_ptr);
            }
            /* open the output file */
            tmpline = argv[i];
            if(strlen(tmpline) == 1 && tmpline[0] == '-') {
               infile.fptr = stdin;
            } else if(strlen(tmpline) >= 1 && tmpline[0] != '-') {
               infile.line = tmpline;
               openfw(infile_ptr);
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
   fprintf(stderr, "usage: %s [-o <file>] <file>\n", arg0);
}
int execute(FILE* file) {
   /* install the program to memory */
   /*
      installprogram(file); 
      do {
      decode(&proc, &proc.code[proc.pc]);
      if(proc.advancepc) {
      proc.pc++;
      }
      } while(!proc.terminateexecution);
      */
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

void shutdown(FILE* output) {
   fwrite(&(proc.code), sizeof(instruction), MemorySize, output);
   fwrite(&(proc.data), sizeof(datum), MemorySize, output);
}

void installprogram(FILE* file) {
   /* read up to 64k of program information */
   int count, i;
   datum cell;
   instruction contents; /* theres a reason for this */
   i = 0;
   cell = 0;
   count = fread(&contents, sizeof(contents), 1, file);
   while(count > 0) {
      if(i < MemorySize) {
         proc.code[i] = contents;
         i++; 
         count = fread(&contents, sizeof(contents), 1, file);
      } else {
         break;
      }
   }
   count = fread(&cell, sizeof(cell), 1, file);
   while(count > 0) {
      if(i < MemorySize) {
         proc.data[i] = cell;
         i++;
         count = fread(&contents, sizeof(contents), 1, file);
      } else {
         printf("warning: Input file still has: %d cells left.\n", count);
         break;
      }
   }
}

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

