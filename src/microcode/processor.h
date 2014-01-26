/* microcoded processor */
/* an mimd array */
typedef unsigned char byte;
typedef unsigned char bit;
typedef void (*coreoperation) (uint dest, uint src0, uint src1);


enum {
    ExecutionUnitOperationCount = 32,
    GlobalRegisterCount = 1024,
    CoreRegisterCount = 64,
    CacheSize = 1024,
    UnitCount = 4,
    CoreCount = UnitCount + 1,
    MemorySize = 10132768,
    MemoryLoadWidth = CoreCount * sizeof(uvlong),
};
typedef union sysregister {
        uvlong ivalue;
        double fvalue;
        bit pvalue : 1;
} sysregister;

typedef struct unit {
    uint index;
} unit;

typedef struct executionunit {
    uint index;
    sysregister registers[CoreRegisterCount];
    uint executionlength;
} executionunit;

typedef struct schedulerunit {
    uint index;
    sysregister registers[GlobalRegisterCount];
} schedulerunit;

uint coreindex = 0;
void* cores[CoreCount];

void setexecutionlength(uint core, uint length);
uvlong getcorecount(void);
uvlong getregistercount(uint core);
void setiregister(uint core, uint dest, uvlong value);
void setfregister(uint core, uint dest, double value);
void setpregister(uint core, uint dest, bit value);
uvlong getiregister(uint core, uint dest);
double getfregister(uint core, uint dest);
bit getpregister(uint core, uint dest);
