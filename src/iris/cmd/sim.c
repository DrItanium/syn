/* sim.c - the iris simulator */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "iris.h"

static void usage(char* arg0);
static int execute(FILE* file);
static void startup(void);
static void shutdown(void);
static void installprogram(FILE* file);
static core proc;

int main(int argc, char* argv[]) {
   char* line;
   char* tmpline;
   FILE* file;
   int needsclosing, last, i, errorfree;
   line = 0;
   file = 0;
   last = argc - 1;
   tmpline = 0;
   needsclosing = 0;
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
               file = stdin;
            } else if(strlen(line) >= 1 && line[0] != '-') {
               file = fopen(line, "r");
               if(!file) {
                  fprintf(stderr, "couldn't open %s\n", line);
                  exit(errno);
               }
               needsclosing = 1;
            }
         } else {
            fprintf(stderr, "no file provided\n");
         }
      }
   }

   if(file) {
      startup();
      execute(file);
      shutdown();
      if(needsclosing && fclose(file) != 0) {
         fprintf(stderr, "couldn't close %s\n", line); 
         exit(errno);
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
   /* install the program to memory */
   installprogram(file); 
   do {
      decode(&proc, &proc.code[proc.pc]);
      if(proc.advancepc) {
         proc.pc++;
      }
   } while(!proc.terminateexecution);
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
