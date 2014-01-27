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

/* router names */
char* inputtomux = "mux";
