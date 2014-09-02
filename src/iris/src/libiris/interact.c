#include <stdint.h>
#include <stdio.h>
#include "clips.h"
#include "iris.h"
#define NO_ARGS "00"
#define DeclareEnumTransferFunction(name, func) EnvDefineFunction2(theEnv, name, 'i', PTIEF func, #func, NO_ARGS)
static word iris_interact_getregister(void*);
static void iris_interact_putregister(void*);

static word iris_interact_registercount(void*);
static dword iris_interact_memorysize(void*);
static word iris_interact_predicateregisterindex(void*);
static word iris_interact_stackpointerindex(void*);

static word iris_interact_getpc(void*);
static void iris_interact_setpc(void*);

static void iris_interact_setcodememory(void*);
static dword iris_interact_getcodememory(void*);

static void iris_interact_setdatamemory(void*);
static word iris_interact_getdatamemory(void*);

static void iris_interact_setstackmemory(void*);
static word iris_interact_getstackmemory(void*);

static int iris_interact_getadvancepc(void*);
static void iris_interact_setadvancepc(void*);

static int iris_interact_terminateexecution(void*);
static void iris_interact_setterminateexecution(void*);

static void iris_interact_dispatch(void*);
static void iris_interact_cycle(void*);

static int iris_interact_load_memory_image(void*);
static int iris_interact_save_memory_image(void*);
static void iris_interact_encode_word_immediate(void*, DATA_OBJECT_PTR);
static dword iris_interact_encode_instruction(void*);
//static void iris_interact_decode_instruction(void*, DATA_OBJECT_PTR);

static void iris_interact_deallocate(void*);
void iris_declarations(void* theEnv) {
   iris_core* curr;
   if (! AllocateEnvironmentData(theEnv, IRIS_CORE_DATA,
            sizeof(iris_core), iris_interact_deallocate)) {
      iris_error("Error allocating environment data for iris_core", ErrorUnableToAllocateCore);
   }
   curr = GetIrisCoreData(theEnv);
   iris_rom_init(curr);
   EnvDefineFunction2(theEnv, "get-register", 'i', PTIEF iris_interact_getregister, "iris_interact_getregister", "11i");
   EnvDefineFunction2(theEnv, "put-register", 'v', PTIEF iris_interact_putregister, "iris_interact_putregister", "22i");
   DeclareEnumTransferFunction("register-count", iris_interact_registercount);
   DeclareEnumTransferFunction("memory-size", iris_interact_memorysize);
   DeclareEnumTransferFunction("predicate-register", iris_interact_predicateregisterindex);
   DeclareEnumTransferFunction("stack-pointer", iris_interact_stackpointerindex);
   EnvDefineFunction2(theEnv, "get-pc", 'i', PTIEF iris_interact_getpc, "iris_interact_getpc", "11i");
   EnvDefineFunction2(theEnv, "put-pc", 'v', PTIEF iris_interact_setpc, "iris_interact_setpc", "22i");
   EnvDefineFunction2(theEnv, "set-code-memory", 'v', PTIEF iris_interact_setcodememory, "iris_interact_setcodememory ","22i");
   EnvDefineFunction2(theEnv, "get-code-memory", 'i', PTIEF iris_interact_getcodememory, "iris_interact_getcodememory ","11i");
   EnvDefineFunction2(theEnv, "set-data-memory", 'v', PTIEF iris_interact_setdatamemory, "iris_interact_setdatamemory ","22i");
   EnvDefineFunction2(theEnv, "get-data-memory", 'i', PTIEF iris_interact_getdatamemory, "iris_interact_getdatamemory ","11i");
   EnvDefineFunction2(theEnv, "set-stack-memory", 'v', PTIEF iris_interact_setstackmemory, "iris_interact_setstackmemory ","22i");
   EnvDefineFunction2(theEnv, "get-stack-memory", 'i', PTIEF iris_interact_getstackmemory, "iris_interact_getstackmemory ","11i");
   EnvDefineFunction2(theEnv, "set-advancepc", 'v', PTIEF iris_interact_setadvancepc, "iris_interact_setadvancepc", "11");
   EnvDefineFunction2(theEnv, "get-advancepc", 'b', PTIEF iris_interact_getadvancepc, "iris_interact_getadvancepc", NO_ARGS);
   EnvDefineFunction2(theEnv, "set-terminate-execution", 'v', PTIEF iris_interact_setterminateexecution, "iris_interact_setterminateexecution", "11");
   EnvDefineFunction2(theEnv, "terminate-executionp", 'b', PTIEF iris_interact_terminateexecution, "iris_interact_terminateexecution", NO_ARGS);
   EnvDefineFunction2(theEnv, "dispatch", 'v', PTIEF iris_interact_dispatch, "iris_interact_dispatch", "11i");
   EnvDefineFunction2(theEnv, "cycle", 'v', PTIEF iris_interact_cycle, "iris_interact_cycle", NO_ARGS);
   EnvDefineFunction2(theEnv, "load-memory-image", 'b', PTIEF iris_interact_load_memory_image, "iris_interact_load_memory_image", "*1");
   EnvDefineFunction2(theEnv, "save-memory-image", 'b', PTIEF iris_interact_save_memory_image, "iris_interact_save_memory_image", "11k");
   EnvDefineFunction2(theEnv, "encode-instruction", 'i', PTIEF iris_interact_encode_instruction, "iris_interact_encode_instruction", "44i");
   EnvDefineFunction2(theEnv, "encode-word-immediate", 'm', PTIEF iris_interact_encode_word_immediate, "iris_interact_encode_word_immediate", "11i");
}

void iris_interact_putregister(void* theEnv) {
   iris_put_register(GetIrisCoreData(theEnv), 
         (byte)EnvRtnLong(theEnv, 1),
         (word)EnvRtnLong(theEnv, 2));
}

word iris_interact_getregister(void* theEnv) {
   return iris_get_register(GetIrisCoreData(theEnv), 
         (byte)EnvRtnLong(theEnv, 1));
}

word iris_interact_registercount(void* theEnv) { return RegisterCount; }
dword iris_interact_memorysize(void* theEnv) { return MemorySize; }
word iris_interact_predicateregisterindex(void* theEnv) { return PredicateRegisterIndex; }
word iris_interact_stackpointerindex(void* theEnv) { return StackPointerRegisterIndex; }

word iris_interact_getpc(void* theEnv) {
   return GetIrisCoreData(theEnv)->pc;
}

void iris_interact_setpc(void* theEnv) {
   iris_core* c;
   c = GetIrisCoreData(theEnv);
   c->pc = (word)EnvRtnLong(theEnv, 1);
}

void iris_interact_setcodememory(void* theEnv) {
   iris_core* c;
   instruction q;
   word addr;
   c = GetIrisCoreData(theEnv);
   addr = (word)EnvRtnLong(theEnv, 1);
   q = (dword)EnvRtnLong(theEnv, 2);
   c->code[addr] = q;
}

dword iris_interact_getcodememory(void* theEnv) {
   iris_core* c;
   word addr;
   c = GetIrisCoreData(theEnv);
   addr = (word)EnvRtnLong(theEnv, 1);
   return c->code[addr];
}

void iris_interact_setdatamemory(void* theEnv) {
   iris_core* c;
   word addr, value;
   c = GetIrisCoreData(theEnv);
   addr = (word)EnvRtnLong(theEnv, 1);
   value = (word)EnvRtnLong(theEnv, 2);
   c->data[addr] = value;
}

word iris_interact_getdatamemory(void* theEnv) {
   iris_core* c;
   word addr;
   c = GetIrisCoreData(theEnv);
   addr = (word)EnvRtnLong(theEnv, 1);
   return c->data[addr];
}

void iris_interact_setstackmemory(void* theEnv) {
   iris_core* c;
   word addr, value;
   c = GetIrisCoreData(theEnv);
   addr = (word)EnvRtnLong(theEnv, 1);
   value = (word)EnvRtnLong(theEnv, 2);
   c->stack[addr] = value;
}

word iris_interact_getstackmemory(void* theEnv) {
   iris_core* c;
   word addr;
   c = GetIrisCoreData(theEnv);
   addr = (word)EnvRtnLong(theEnv, 1);
   return c->stack[addr];
}

int iris_interact_getadvancepc(void* theEnv) {
   return GetIrisCoreData(theEnv)->advancepc;
}

void iris_interact_setadvancepc(void* theEnv) {
   iris_core* c;
   DATA_OBJECT obj;
   c = GetIrisCoreData(theEnv);

   EnvRtnUnknown(theEnv,1,&obj);
   if (obj.type == SYMBOL) {
      if ((obj.value == EnvFalseSymbol(theEnv))) {
         c->advancepc = FALSE;
      } else {
         c->advancepc = TRUE;
      }
   } else {
      iris_error("Invalid type to set advance-pc to", ErrorTriedToSetAdvancePcToIllegalValue);
   }
}

int iris_interact_terminateexecution(void* theEnv) {
   return GetIrisCoreData(theEnv)->terminateexecution;
}

void iris_interact_setterminateexecution(void* theEnv) {
   iris_core* c;
   DATA_OBJECT obj;
   c = GetIrisCoreData(theEnv);

   EnvRtnUnknown(theEnv,1,&obj);
   if (obj.type == SYMBOL) {
      if ((obj.value == EnvFalseSymbol(theEnv))) {
         c->terminateexecution = FALSE;
      } else {
         c->terminateexecution = TRUE;
      }
   } else {
      iris_error("Invalid type to set terminateexecution to", ErrorTriedToSetShouldTerminateToIllegalValue);
   }
}

void iris_interact_dispatch(void* theEnv) {
   /* takes in an encoded instruction */
   iris_core* c;
   instruction q;
   q = (dword)EnvRtnLong(theEnv, 1);
   c = GetIrisCoreData(theEnv);
   iris_dispatch(c, &q);
}

void iris_interact_cycle(void* theEnv) {
   iris_core* c;
   c = GetIrisCoreData(theEnv);
   iris_dispatch(c, &c->code[c->pc]);
   if(c->advancepc) {
      c->pc++;
   }
}

int iris_interact_load_memory_image(void* theEnv) {
   iris_core* core;
   int argCount;
   char *logicalName;
   int a, b, c, d, i;
   instruction tmp;

   if ((argCount = EnvArgCountCheck(theEnv, "load-memory-image", NO_MORE_THAN, 1)) == -1) {
      return FALSE;
   }

   if (argCount == 0) {
      logicalName = "stdin";
   } else {
      logicalName = GetLogicalName(theEnv,1,"stdin");
      if (logicalName == NULL) {
         IllegalLogicalNameMessage(theEnv, "load-memory-image");
         SetHaltExecution(theEnv, TRUE);
         SetEvaluationError(theEnv, TRUE);
         return FALSE;
      }
   }
   if (QueryRouters(theEnv, logicalName) == FALSE) {
      UnrecognizedRouterMessage(theEnv, logicalName);
      SetHaltExecution(theEnv, TRUE);
      SetEvaluationError(theEnv, TRUE);
      return FALSE;
   }
#define EncodeWord(a, b) ((word)(((b & 0x000000FF) << 8) + (a & 0x000000FF)))
#define LoadWords(count, array, msg) \
   for (i = 0; i < count; i++) { \
      a = EnvGetcRouter(theEnv, logicalName); \
      b = EnvGetcRouter(theEnv, logicalName); \
      if (a == EOF || b == EOF) { \
         break; \
      } else { \
         core->array[i] = EncodeWord(a, b); \
      } \
   } \
   if (i < count) { \
      EnvPrintRouter(theEnv, WERROR, msg); \
      return FALSE; \
   }


   core = GetIrisCoreData(theEnv);
   /* restore the PC */
   a = EnvGetcRouter(theEnv, logicalName);
   b = EnvGetcRouter(theEnv, logicalName);
   core->pc = EncodeWord(a, b);
   /* load the registers */
   LoadWords(RegisterCount, gpr, "End of stream was reached\nNo data section or an error occurred!\n")
      /* Load the code section */
      for(i = 0; i < MemorySize; i++) {
         a = EnvGetcRouter(theEnv, logicalName);
         b = EnvGetcRouter(theEnv, logicalName);
         c = EnvGetcRouter(theEnv, logicalName);
         d = EnvGetcRouter(theEnv, logicalName);
         if(a == EOF || b == EOF || c == EOF || d == EOF) {
            break;
         } else {
            tmp = (((d & 0x000000FF) << 24) | ((c & 0x000000FF) << 16) | 
                  ((b & 0x000000FF) << 8) | (a & 0x000000FF));
            core->code[i] = tmp;
         }
      }
   if(i < MemorySize) {
      EnvPrintRouter(theEnv, WERROR, "End of stream was reached\nNo data section or an error occurred!\n");
      return FALSE;
   } 
   /* Load the data section */
   LoadWords(MemorySize, data, "End of stream was reached before end of data section or an error occurred!\nStack was not loaded!")
      /* Load the stack */
      LoadWords(MemorySize, stack, "End of stream was reached before end of stack section or an error occurred!\n")

      if(EnvGetcRouter(theEnv, logicalName) != EOF) {
         EnvPrintRouter(theEnv, WERROR, "Warning: image contains more data than is loadable\n");
      }
   return TRUE;
#undef EncodeWord
#undef LoadWords
}

int iris_interact_save_memory_image(void* theEnv) {
   iris_core* core;
   int argCount, i;
   char* logicalName;
   dword tmp;
   word currData;
   char dataValue[2];
   char instValue[4];
   FILE* fptr;
   currData = 0;
   tmp = 0;

   if ((argCount = EnvArgCountCheck(theEnv, "save-memory-image", NO_MORE_THAN, 1)) == -1) {
      return FALSE;
   }

   if (argCount == 0) {
      logicalName = "stdin";
   } else {
      logicalName = GetLogicalName(theEnv,1,"stdin");
      if (logicalName == NULL) {
         IllegalLogicalNameMessage(theEnv, "save-memory-image");
         SetHaltExecution(theEnv, TRUE);
         SetEvaluationError(theEnv, TRUE);
         return FALSE;
      }
   }
   if (QueryRouters(theEnv, logicalName) == FALSE) {
      UnrecognizedRouterMessage(theEnv, logicalName);
      SetHaltExecution(theEnv, TRUE);
      SetEvaluationError(theEnv, TRUE);
      return FALSE;
   }
#define SaveWords(count, array) \
   if (fptr != NULL) { \
      for (i = 0; i < count; i++) { \
         currData = core->array[i]; \
         dataValue[0] = (char)(currData & 0x00FF); \
         dataValue[1] = (char)((currData & 0xFF00) >> 8); \
         putc((int)dataValue[0], fptr); \
         putc((int)dataValue[1], fptr); \
      } \
   } else { \
      /* custom router */ \
      for (i = 0; i < count; i++) { \
         currData = core->array[i]; \
         dataValue[0] = (char)(currData & 0x00FF); \
         dataValue[1] = (char)((currData & 0xFF00) >> 8); \
         EnvPrintRouter(theEnv, logicalName, dataValue); \
      } \
   } \


   core = GetIrisCoreData(theEnv);
   /* save the PC */
   fptr = FindFptr(theEnv, logicalName);
   dataValue[0] = (char)(currData & 0x00FF);
   dataValue[1] = (char)((currData & 0xFF00) >> 8);
   if (fptr != NULL) {
      putc((int)dataValue[0], fptr);
      putc((int)dataValue[1], fptr);
   } else {
      EnvPrintRouter(theEnv, logicalName, dataValue);
   }
   /* save the registers */
   SaveWords(RegisterCount, gpr)
      /* Load the code section */
      if (fptr != NULL) {
         for(i = 0; i < MemorySize; i++) {
            tmp = core->code[i];
            instValue[0] = (char)(tmp & 0x000000FF);
            instValue[1] = (char)((tmp & 0x0000FF00) >> 8);
            instValue[2] = (char)((tmp & 0x00FF0000) >> 16);
            instValue[3] = (char)((tmp & 0xFF000000) >> 24);
            putc((int)instValue[0], fptr);
            putc((int)instValue[1], fptr);
            putc((int)instValue[2], fptr);
            putc((int)instValue[3], fptr);
         }
      } else {
         // custom router, not a file
         for(i = 0; i < MemorySize; i++) {
            tmp = core->code[i];
            instValue[0] = (char)(tmp & 0x000000FF);
            instValue[1] = (char)((tmp & 0x0000FF00) >> 8);
            instValue[2] = (char)((tmp & 0x00FF0000) >> 16);
            instValue[3] = (char)((tmp & 0xFF000000) >> 24);
            EnvPrintRouter(theEnv, logicalName, instValue);
         }
      }
   /* Load the data section */
   SaveWords(MemorySize, data)
      /* Load the stack */
      SaveWords(MemorySize, stack)

      return TRUE;
#undef SaveWords 
}

void iris_interact_deallocate(void* theEnv) {
   iris_shutdown(GetIrisCoreData(theEnv));
}
dword iris_interact_encode_instruction(void* theEnv) {
   dword control, r0, r1, r2;
   control = (dword)(byte)EnvRtnLong(theEnv, 1L);
   r0 = (dword)(byte)EnvRtnLong(theEnv, 2L);
   r1 = (dword)(byte)EnvRtnLong(theEnv, 3L);
   r2 = (dword)(byte)EnvRtnLong(theEnv, 4L);
   return ((r2 & 0x000000FF) << 24) |
      ((r1 & 0x000000FF) << 16) |
      ((r0 & 0x000000FF) << 8) |
      ((control & 0x000000FF));
}

void iris_interact_encode_word_immediate(void* theEnv, DATA_OBJECT_PTR ret) {
   void* multifield;
   word value;

   value = EnvRtnLong(theEnv, 1);
   multifield = EnvCreateMultifield(theEnv, 2);
   SetMFType(multifield, 1, INTEGER);
   SetMFType(multifield, 2, INTEGER);
   SetMFValue(multifield, 1, EnvAddLong(theEnv, value & 0x00FF));
   SetMFValue(multifield, 2, EnvAddLong(theEnv, ((value & 0xFF00) >> 8)));

   SetpType(ret, MULTIFIELD);
   SetpValue(ret, multifield);

   SetpDOBegin(ret, 1);
   SetpDOEnd(ret, 2);
}

/* vim: set expandtab tabstop=3 shiftwidth=3: */
