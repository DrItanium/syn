#include <u.h>
#include <libc.h>
#include<draw.h>
#include<event.h>
#include <clips.h>
#include "processor.h"

#define MULTIPLEXER_TERMINATOR_DATA USER_ENVIRONMENT_DATA + 0 
#define GPU_DATA USER_ENVIRONMENT_DATA + 1

#define GetMultiplexerTerminatorData(theEnv) ((terminatordata*) GetEnvironmentData(theEnv, MULTIPLEXER_TERMINATOR_DATA))
#define GetGPUData(theEnv) ((gpudata*) GetEnvironmentData(theEnv, GPU_DATA))
static void setupmux(void);
static void setupgpu(void);
static void setupinput(void);
static void setuppanels(void);
static void markterminate(void*);
/* routers */
/* generic mux interface functions */
static int validcommunicationchannel(void*, char*, char*);
static int usecommunicationchannel(void*, void*, char*, char*);
static int findtomux(void*, char*);
static int exittomux(void*, int);
static int printtomux(void*, char*, char*);
static void createchanneltomux(void*);

/* gpu */
static int findgpu(void*, char*);
static int exitgpu(void*, int);
static int printgpu(void*, char*, char*);
static int getcgpu(void*, char*);
static int ungetcgpu(void*, int, char*);
static void interpret(void*, char*);
static int putgpuregister(void*);
static void getgpuregister(void*, DATA_OBJECT_PTR);

static int findmuxtogpu(void*, char*);
static int exitmuxtogpu(void*, int); 
static int printmuxtogpu(void*, char*, char*);

static int findmuxtopanels(void*, char*);
static int exitmuxtopanels(void*, int); 
static int printmuxtopanels(void*, char*, char*);

void 
main(int argc, char *argv[]) {
    int key;
    Event evt;
    Mouse m;
    char buf[128];
    /* Do this ahead of anything else so we don't have issues with
     * initialization across multiple environments */
    if(initdraw(0,0, "polymer") < 0) {
        sysfatal("initdraw failed: %r");
        exits("initdraw");
    }
    einit(Ekeyboard|Emouse);
    setupmux();
    setupinput();
    setupgpu();
    setuppanels();
    eresized(0);
    for(;;) {
        key = event(&evt);
        if(key == Emouse) {
            m = evt.mouse;
            sprintf(buf, "(mouse %d %d %d %ld)", m.buttons, m.xy.x, m.xy.y, m.msec);
            EnvAssertString(input, buf);
        } else if (key == Ekeyboard) {
            sprintf(buf, "(key %d)", evt.kbdc);
            EnvAssertString(input, buf);
        }
        /* mux is executed three times per cycle */
        EnvRun(input, -1L);
        EnvRun(mux, -1L);
        EnvRun(panels, -1L);
        EnvRun(mux, -1L);
        EnvRun(gpu, -1L);
        EnvRun(mux, -1L);
        if(GetMultiplexerTerminatorData(mux)->shouldterminate == 1) {
            DeallocateEnvironmentData();
            exits(0);
        }
    }
}
void UserFunctions() {

}
void EnvUserFunctions(void* theEnv) {


}
void 
eresized(int new) {
    /* need to jump into the gpu environment and perform graphical resizing */
    char buf[1024];
    if(new && getwindow(display, Refnone) < 0) {
        fprint(2, "can't reattach to window: %r\n");
        exits("resized");
    }
    sprintf(buf, "(message (to panels) (from gpu) (action resize) (values %d %d %d))", 
            new,
            screen->r.max.x,
            screen->r.max.y);
    EnvAssertString(panels, buf);
    EnvRun(panels, -1);
    EnvRun(gpu, -1);
    flushimage(display, 1);
}
void 
setupgpu(void) {
    gpu = CreateEnvironment();
    if (! AllocateEnvironmentData(gpu, GPU_DATA,
                sizeof(gpudata),NULL))
    {
        sysfatal("Error allocating environment data for GPU_DATA: %r");
        exits("allocgpudatafailure");
    }
    createchanneltomux(gpu);
    EnvAddRouter(gpu, tomicrocode, 40, findgpu, printgpu, getcgpu, ungetcgpu, exitgpu);
    EnvDefineFunction2(gpu, "put-register", 'b', PTIEF putgpuregister, "putgpuregister", "22nin");
    EnvDefineFunction2(gpu, "get-register", 'u', PTIEF getgpuregister, "putgpuregister", "22i");
    EnvBatchStar(gpu, "microcode/gpu.clp");
}

void 
setupmux(void) {
    mux = CreateEnvironment();
    if (! AllocateEnvironmentData(mux, MULTIPLEXER_TERMINATOR_DATA,
                sizeof(terminatordata),NULL))
    {
        sysfatal("Error allocating environment data for MULTIPLEXER_TERMINATOR_DATA: %r");
        exits("allocmuxdatafailure");
    }
    GetMultiplexerTerminatorData(mux)->shouldterminate = 0;
    EnvDefineFunction2(mux, "exit", 'v', PTIEF markterminate, "markterminate", "00a");
    EnvAddRouter(mux, togpu, 40, findmuxtogpu, printmuxtogpu, NULL, NULL, exitmuxtogpu);
    EnvAddRouter(mux, topanels, 40, findmuxtopanels, printmuxtopanels, NULL, NULL, exitmuxtopanels);
    EnvBatchStar(mux, "microcode/mux.clp");
}

void
setuppanels(void) {
    panels = CreateEnvironment();
    createchanneltomux(panels);
    EnvBatchStar(panels, "microcode/panels.clp");
}

void 
setupinput(void) {
    input = CreateEnvironment();
    createchanneltomux(input);
    EnvBatchStar(input, "microcode/input.clp");
}

void
markterminate(void* theEnv) {
    GetMultiplexerTerminatorData(theEnv)->shouldterminate = 1;
}

int 
validcommunicationchannel(void* theEnv, char* logicalName, char* target) {
    if((strcmp(logicalName, target) == 0)) {
        return TRUE;
    } else {
        return FALSE;
    }
}
int
usecommunicationchannel(void* theEnv, void* target, char* logicalName, char* str) {
    EnvAssertString(target, str);
    return 1;
}
/* generic to mux finder */
int 
findtomux(void* theEnv, char* logicalName) {
    return validcommunicationchannel(theEnv, logicalName, tomux);
}

int 
exittomux(void* theEnv, int num) {
    return 1;
}

int
printtomux(void* theEnv, char* logicalName, char* str) {
    return usecommunicationchannel(theEnv, mux, logicalName, str); 
}

void
createchanneltomux(void* theEnv) {
    EnvAddRouter(theEnv, tomux, 40, findtomux, printtomux, NULL, NULL, exittomux);
}

/* mux to gpu */
int 
findmuxtogpu(void* theEnv, char* logicalName) {
    return validcommunicationchannel(theEnv, logicalName, togpu);
}

int
exitmuxtogpu(void* theEnv, int num) {
    return 1;
}

int
printmuxtogpu(void* theEnv, char* logicalName, char* str) {
    return usecommunicationchannel(theEnv, gpu, logicalName, str); 
}

/* mux to panels */
int 
findmuxtopanels(void* theEnv, char* logicalName) {
    return validcommunicationchannel(theEnv, logicalName, topanels);
}

int
exitmuxtopanels(void* theEnv, int num) {
    return 1;
}

int
printmuxtopanels(void* theEnv, char* logicalName, char* str) {
    return usecommunicationchannel(theEnv, panels, logicalName, str); 
}

/* gpu operations */
int
findgpu(void* theEnv, char* logicalName) {
    return validcommunicationchannel(theEnv, logicalName, tomicrocode);
}

int
printgpu(void* theEnv, char* logicalName, char* str) {
    interpret(theEnv, str);
    return 1;
}

void
interpret(void* theEnv, char* stream) {
    /* do nothing right now */
}

int
getcgpu(void* theEnv, char* logicalName) {
    int index, result;
    index = GetGPUData(theEnv)->index;
    if(index < RegisterByteLength) {
        result = GetGPUData(theEnv)->output.bytes[index];
        GetGPUData(theEnv)->index++;
    } else {
        result = EOF;
    }
    return result;
}

int
ungetcgpu(void* theEnv, int ch, char* logicalName) {
    int index, result;
    index = GetGPUData(theEnv)->index;
    /* we can't ungetc if we haven't actually looked at it */
    if(index > 0) {
        result = ch; 
        GetGPUData(theEnv)->index--;
    } else {
        /* can't decrement any more */
        result = EOF;
    }
    return result;
}

int 
exitgpu(void* theEnv, int num) {
    /* clean up all of the different functions */
    return 1;
}

/*
   EnvDefineFunction2(gpu, "put-register", 'b', PTIEF putgpuregister, "putgpuregister", "22nin");
   EnvDefineFunction2(gpu, "get-register", 'u', PTIEF getgpuregister, "putgpuregister", "12i");
   */

int
putgpuregister(void* theEnv) {
    uint index;
    DATA_OBJECT arg0;
    index = EnvRtnLong(theEnv, 1);
    if(index >= 0 && index < GPURegisterCount &&
            EnvArgTypeCheck(theEnv, "put-register", 2, INTEGER_OR_FLOAT, &arg0) == TRUE) {

        if(GetType(arg0) == INTEGER) {
            GetGPUData(theEnv)->registers[index].ivalue = DOToLong(arg0);
        } else {
            GetGPUData(theEnv)->registers[index].fvalue = DOToLong(arg0);
        }
        return TRUE;
    } else {
        return FALSE;
    }
}
enum {
    RegisterIntType = 0,
    RegisterFloatType,
};
void
getgpuregister(void* theEnv, DATA_OBJECT_PTR ret) {
    uint index, type;
    index = EnvRtnLong(theEnv, 1);
    type = EnvRtnLong(theEnv, 2);
    if(index >= 0 && index < GPURegisterCount) {
        if(type == RegisterIntType) {
            ret->type = INTEGER;
            ret->value = EnvAddLong(theEnv, GetGPUData(theEnv)->registers[index].ivalue);
        } else if(type == RegisterFloatType) {
            ret->type = FLOAT;
            ret->value = EnvAddDouble(theEnv, GetGPUData(theEnv)->registers[index].fvalue);
        } else {
            ret->type = SYMBOL;
            ret->value = EnvFalseSymbol(theEnv);
        }
    } else {
        ret->type = SYMBOL;
        ret->value = EnvFalseSymbol(theEnv);
    }
}
