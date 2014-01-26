/* microcoded processor */
/* an mimd array */
typedef unsigned char byte;
enum {
   ExecutionUnitOperationCount = 32,
   CoreRegisterCount = 64,
   GlobalRegisterCount = 1024,
   CacheSize = 1024,
   CoreCount = 8,
   MemorySize = 10132768,
};
typedef void (*coreoperation) (uint dest, uint src0, uint src1);
typedef struct executionunit {
     union {
        uvlong ivalue;
        double fvalue;
     } registers[CoreRegisterCount];
     coreoperation operations[ExecutionUnitOperationCount];
     uvlong cache[CacheSize];
} executionunit;

executionunit units[CoreCount];
uvlong memory[MemorySize];
uvlong globalregisters[GlobalRegisterCount];

uvlong getregistercount(void);
void setregister(uint dest, uvlong value);
uvlong getregister(uint index);
