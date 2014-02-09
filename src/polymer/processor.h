/* microcoded processor with no registers! */
typedef unsigned char byte;
typedef unsigned char bit;
/* divided into four parts */

/* dispatch multiplexer */
void* mux;
/* input provider */
void* input;
/* panels that are connected to the multiplexer */
void* panels;
/* graphics processor */
void* gpu;

typedef struct terminatordata {
    bit shouldterminate : 1;
} terminatordata;

enum {
    RegisterByteLength = sizeof(uvlong),
};

typedef union gpuregister {
    uvlong ivalue;
    double fvalue;
    byte bytes[RegisterByteLength];
} gpuregister;

typedef struct gpudata {
    gpuregister registers[128];
    /* read from using getc and putc */
    gpuregister output;
    int index;
} gpudata;

/* router names */
char* tomux = "mux";
char* togpu = "gpu";
char* topanels = "panels";
char* tomicrocode = "microcode";
