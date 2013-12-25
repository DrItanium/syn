#include <u.h>
#include <libc.h>
#include <stdio.h>
#include "interpreter.h"
void main() {
   
   processor proc;
   setupprocessor(&proc);
   while(cycle(&proc));
   shutdownprocessor(&proc);
   exits(0);
}
void shutdownprocessor(processor* proc) {
   free(proc->memory);
}
instruction retrieveinstruction(processor* proc) {
   return (instruction)proc->memory[proc->gpr[ProgramCounter]];
}
void incrementprogramcounter(processor* proc) {
   add(proc, ProgramCounter, ProgramCounter, TrueRegister);
   modop(proc, ProgramCounter, ProgramCounter, CellCountRegister);
}
int instructionexecutable(processor* proc, instruction inst) {
   return proc->gpr[inst.predicate];
}
int cycle(processor* proc) {
   int shouldincrementprogramcounter;
   instruction a, b;
   shouldincrementprogramcounter = 1;
   a = retrieveinstruction(proc);
   /* check to see if we should execute */
   if(instructionexecutable(proc, a)) {
      switch(a.id) {
         case NopInstruction:
            nop(proc);
            break;
         case AddInstruction:
            add(proc, a.destination0, a.source0, a.source1);
            break;
         case SubInstruction:
            sub(proc, a.destination0, a.source0, a.source1);
            break;
         case MulInstruction:
            mul(proc, a.destination0, a.source0, a.source1);
            break;
         case DivInstruction:
            divop(proc, a.destination0, a.source0, a.source1);
            break;
         case RightShiftInstruction:
            rightshift(proc, a.destination0, a.source0, a.source1);
            break;
         case LeftShiftInstruction:
            leftshift(proc, a.destination0, a.source0, a.source1);
            break;
         case BinaryOrInstruction:
            binaryor(proc, a.destination0, a.source0, a.source1);
            break;
         case BinaryAndInstruction:
            binaryand(proc, a.destination0, a.source0, a.source1);
            break;
         case BinaryNotInstruction:
            binarynot(proc, a.destination0, a.source0);
            break;
         case EqualsInstruction:
            equals(proc, a.destination0, a.destination1, a.source0, a.source1);
            break;
         case NotEqualsInstruction:
            notequals(proc, a.destination0, a.destination1, a.source0, a.source1);
            break;
         case GreaterThanInstruction:
            greaterthan(proc, a.destination0, a.destination1, a.source0, a.source1);
            break;
         case LessThanInstruction:
            lessthan(proc, a.destination0, a.destination1, a.source0, a.source1);
            break;
         case LoadInstruction:
            load(proc, a.destination0, a.source0);
            break;
         case StoreInstruction:
            store(proc, a.destination0, a.source0);
            break;
         case BranchInstruction:
            /* we need to grab the next cell */
            incrementprogramcounter(proc);
            b = retrieveinstruction(proc);
            branch(proc, b.value);
            shouldincrementprogramcounter = 0;
            break;
         case SetInstruction:
            /* this is an interesting case */
            incrementprogramcounter(proc);
            b = retrieveinstruction(proc);
            set(proc, a.destination0, b.value);
            shouldincrementprogramcounter = 0;
            break;
         case ModInstruction:
            modop(proc, a.destination0, a.source0, a.source1);
            break;
         case CallInstruction:
            call(proc, a.source0);
            shouldincrementprogramcounter = 0;
            break;
         case RetInstruction:
            ret(proc);
            break;
         case PlatformCallInstruction:
            platformcall(proc);
            break;
         case TerminateInstruction:
         default:
            return 0;
      }
   } 
   if(shouldincrementprogramcounter) {
      incrementprogramcounter(proc);
      switch(a.id) {
         /* make sure that we don't interpret the second byte */
         case BranchInstruction:
         case SetInstruction:
            incrementprogramcounter(proc);
         default:
            break;
      }
   }
   return 1;
}
void add(processor* proc, uchar dest, uchar src0, uchar src1) {
   proc->gpr[dest] = proc->gpr[src0] + proc->gpr[src1];	
}

void sub(processor* proc, uchar dest, uchar src0, uchar src1) {
   proc->gpr[dest] = proc->gpr[src0] - proc->gpr[src1];	
}
void mul(processor* proc, uchar dest, uchar src0, uchar src1) {
   proc->gpr[dest] = proc->gpr[src0] * proc->gpr[src1];	
}
void divop(processor* proc, uchar dest, uchar src0, uchar src1) {
   proc->gpr[dest] = proc->gpr[src0] / proc->gpr[src1];	
}
void rightshift(processor* proc, uchar dest, uchar src0, uchar src1) {
   proc->gpr[dest] = proc->gpr[src0] >> proc->gpr[src1];	
}
void leftshift(processor* proc, uchar dest, uchar src0, uchar src1) {
   proc->gpr[dest] = proc->gpr[src0] << proc->gpr[src1];	
}
void binaryor(processor* proc, uchar dest, uchar src0, uchar src1) {
   proc->gpr[dest] = proc->gpr[src0] | proc->gpr[src1];	
}
void binaryand(processor* proc, uchar dest, uchar src0, uchar src1) {
   proc->gpr[dest] = proc->gpr[src0] & proc->gpr[src1];	
}
void binaryxor(processor* proc, uchar dest, uchar src0, uchar src1) {
   proc->gpr[dest] = proc->gpr[src0] ^ proc->gpr[src1];	
}
void binarynot(processor* proc, uchar dest, uchar src0) {
   proc->gpr[dest] = !(proc->gpr[src0]);	
}
void equals(processor* proc, uchar desttrue, uchar destfalse, uchar src0, uchar src1) {
   uvlong tmp;
   tmp = proc->gpr[src0] == proc->gpr[src1];	
   proc->gpr[desttrue] = tmp;
   proc->gpr[destfalse] = !tmp;
}
void notequals(processor* proc, uchar desttrue, uchar destfalse, uchar src0, uchar src1) {
   equals(proc, destfalse, desttrue, src0, src1);
}
void greaterthan(processor* proc, uchar desttrue, uchar destfalse, uchar src0, uchar src1) {
   uvlong tmp;
   tmp = proc->gpr[src0] > proc->gpr[src1];
   proc->gpr[desttrue] = tmp;
   proc->gpr[destfalse] = !tmp;
}
void lessthan(processor* proc, uchar desttrue, uchar destfalse, uchar src0, uchar src1) {
   uvlong tmp;
   tmp = proc->gpr[src0] < proc->gpr[src1];
   proc->gpr[desttrue] = tmp;
   proc->gpr[destfalse] = !tmp;
}

void load(processor* proc, uchar dest, uchar src) {
   proc->gpr[dest] = proc->memory[proc->gpr[src]];
}
void store(processor* proc, uchar dest, uchar src) {
   proc->memory[proc->gpr[dest]] = proc->gpr[src];
}

void branch(processor* proc, uvlong dest) {
   set(proc, ProgramCounter, dest % proc->gpr[CellCountRegister]);
}

void set(processor* proc, uchar dest, uvlong value) {
   proc->gpr[dest] = value;
}
void nop(processor* proc) { }

void modop(processor* proc, uchar dest, uchar src0, uchar src1) {
   proc->gpr[dest] = proc->gpr[src0] % proc->gpr[src1];	
}

void call(processor* proc, uchar dest) {
   add(proc, ReturnRegister, ProgramCounter, FalseRegister);
   branch(proc, proc->gpr[dest]);
}

void ret(processor* proc) {
   add(proc, ProgramCounter, ReturnRegister, FalseRegister);
}
void setupprocessor(processor* proc) {

   /* Upper registers have a special purpose */
   proc->gpr[FalseRegister] = 0;
   proc->gpr[TrueRegister] = 1;
   proc->gpr[ProcessorIdRegister] = processor_count++;
   proc->gpr[RegisterCountRegister] = RegisterCount;
   proc->gpr[CellCountRegister] = MemorySize;
   proc->gpr[ReturnRegister] = 0;
   /* program-counter */
   proc->gpr[ProgramCounter] = 2048;
   proc->memory = malloc(MemorySize * sizeof(uvlong));
   installexitcall(proc);
   installplatformcallhandler(proc);
   installprocessorloop(proc);
}
void installprocessorloop(processor* proc) {
   int offset;

   offset = proc->gpr[ProgramCounter];
   /* just print a and exit */
   encodesetinstruction(proc, offset, TrueRegister, PlatformInputRegister0, (uvlong)'i');
   encodebranchinstruction(proc, offset + 2, TrueRegister, offset);
}
void installplatformcallhandler(processor* proc) {
   encodesetinstruction(proc, PlatformHandlerLocation, TrueRegister, PlatformScratch0, platformexit);
   encodeeqinstruction(proc, PlatformHandlerLocation + 2, TrueRegister, PlatformTrue, PlatformFalse, PlatformFunctionCallIndex, PlatformScratch0);
   encodebranchinstruction(proc, PlatformHandlerLocation + 3, PlatformTrue, TerminateLocation);
   encodesetinstruction(proc, PlatformHandlerLocation + 5, PlatformFalse, PlatformScratch0, platformputc);
   encodeeqinstruction(proc, PlatformHandlerLocation + 7, PlatformFalse, PlatformTrue, PlatformFalse, PlatformFunctionCallIndex, PlatformScratch0);
   encodesetinstruction(proc, PlatformHandlerLocation + 8, PlatformFalse, PlatformScratch0, platformgetc);
   encodeeqinstruction(proc, PlatformHandlerLocation + 10, PlatformFalse, PlatformTrue, PlatformFalse, PlatformFunctionCallIndex, PlatformScratch0);
   encodesetinstruction(proc, PlatformHandlerLocation + 11, PlatformFalse, PlatformScratch0, platformerror);
   encodeplatforminstruction(proc, PlatformHandlerLocation + 13, TrueRegister);
   encoderetinstruction(proc, PlatformHandlerLocation + 14, TrueRegister);
}
void installexitcall(processor* proc) {
   instruction terminate;
   terminate.value = 0;
   terminate.id = TerminateInstruction;
   terminate.predicate = TrueRegister;
   proc->memory[TerminateLocation] = terminate.value;
}

void encodesetinstruction(processor* proc, int offset, uchar pred, uchar reg, uvlong value) {
   instruction a;	
   a.value = 0;
   a.predicate = pred;
   a.id = SetInstruction;
   a.destination0 = reg;
   proc->memory[offset] = a.value;
   proc->memory[offset + 1] = value;
}
void encodebranchinstruction(processor* proc, int offset, uchar pred, uvlong value) {
   instruction a;	
   a.value = 0;
   a.predicate = pred;
   a.id = BranchInstruction;
   proc->memory[offset] = a.value;
   proc->memory[offset + 1] = value;
}

void encodeeqinstruction(processor* proc, int offset, uchar pred, uchar d0, uchar d1, uchar s0, uchar s1) {
   instruction a;	
   a.value = 0;
   a.predicate = pred;
   a.id = EqualsInstruction;
   a.destination0 = d0;
   a.destination1 = d1;
   a.source0 = s0;
   a.source1 = s1;
   proc->memory[offset] = a.value;
}
void encodecallinstruction(processor* proc, int offset, uchar pred, uchar dest) {
   instruction a;
   a.value = 0;
   a.predicate = pred;
   a.id = CallInstruction;
   a.source0 = dest;
   proc->memory[offset] = a.value;
}
void encodeplatforminstruction(processor* proc, int offset, uchar pred) {
   instruction a;	
   a.value = 0;
   a.predicate = pred;
   a.id = PlatformCallInstruction;
   proc->memory[offset] = a.value;
}


void encoderetinstruction(processor* proc, int offset, uchar pred) {
   instruction a;	
   a.value = 0;
   a.predicate = pred;
   a.id = RetInstruction;
   proc->memory[offset] = a.value;
}

void platformcall(processor* proc) {
   switch(proc->gpr[PlatformFunctionCallIndex]) {
      case platformputc:
         proc->gpr[PlatformOutputRegister0] = putchar((char)proc->gpr[PlatformInputRegister0]);
         break;
      case platformgetc:
         proc->gpr[PlatformOutputRegister0] = getchar();
         break;
      case platformexit:
         sysfatal("Somehow platformexit was called!");
      case platformerror:
         sysfatal("A platform error occurred!");
      default:
         sysfatal("Invalid platform call occurred");
   }
}
