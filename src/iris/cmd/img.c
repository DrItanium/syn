/* img.c - the iris image creator */
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

enum {
   SegmentCode = 0,
   SegmentData,

   SegmentCount,
};
void installcode(FILE* file, int lineno);
void installdata(FILE* file, int lineno);

static void usage(char* arg0);
static int execute(FILE* file);
static void startup(void);
static void shutdown(FILE* output);
static core proc;
static int translateline(FILE* file, int lineno);

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
   fprintf(stderr, "usage: %s [-o <file>] <file>\n", arg0);
}
int execute(FILE* file) {
   int status, lineno;
   lineno = 1; 
   status = translateline(file, lineno);
   while(status == 0) {
      lineno++;
      status = translateline(file, lineno);
   }
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


int translateline(FILE* file, int lineno) {
   int curr;
   byte segment;
   /* install the program to memory */
   /* each line in the map looks like this 
    * <c|d><addr-in-hex><value-in-hex> 
    *
    * Anything else is an error even empty lines
    */
   curr = fgetc(file);
   if(curr == EOF) {
      return 1;
   } else {
      segment = (byte)curr;
      switch(curr) {
         case SegmentCode:
            /* read the address */
            installcode(file, lineno);
            break;
         case SegmentData:
            installdata(file, lineno);
            break;
         default:
            fprintf(stderr, "error: line %d, unknown segment %d\n", lineno, segment);
            exit(1);
            break;
      }
   }
   return 0;
}

void installcode(FILE* f, int ln) {
   ushort addr;
   uint value;
   if(fread(&addr, sizeof(addr), 1, f) == 1) {
      if(fread(&value, sizeof(value), 1, f) == 1) {
         proc.code[addr].full = value;
      } else {
         fprintf(stderr, "error: line %d, invalid value\n", ln);
         exit(1);
      }
   } else {
      fprintf(stderr, "error: line %d, invalid address\n", ln);
      exit(1);
   }
}

void installdata(FILE* f, int ln) {
   ushort addr;
   ushort value;
   if(fread(&addr, sizeof(addr), 1, f) == 1) {
      if(fread(&value, sizeof(value), 1, f) == 1) {
         proc.data[addr] = value;
      } else {
         fprintf(stderr, "error: line %d, invalid value\n", ln);
         exit(1);
      }
   } else {
      fprintf(stderr, "error: line %d, invalid address\n", ln);
      exit(1);
   }
}
