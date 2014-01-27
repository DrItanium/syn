/* microcoded processor */
/* an mimd array */
typedef unsigned char byte;
typedef unsigned char bit;

enum {
    CoreRegisterCount = 128,
    UnitCount = 0,
    CoreCount = UnitCount + 1,
    DefaultExeuctionLength = 1024,
};
typedef union sysregister {
        uvlong ivalue;
        double fvalue;
        bit pvalue : 1;
} sysregister;

typedef struct executionunit {
    uint index;
    sysregister registers[CoreRegisterCount];
    uint executionlength;
} executionunit;


uint coreindex = 0;
void* cores[CoreCount];

