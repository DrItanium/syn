#include <u.h>
#include <libc.h>
#include<draw.h>
#include<event.h>
#include <clips.h>
#include "processor.h"

#define MULTIPLEXER_TERMINATOR_DATA USER_ENVIRONMENT_DATA + 0 

#define GetMultiplexerTerminatorData(theEnv) ((terminatordata*) GetEnvironmentData(theEnv, MULTIPLEXER_TERMINATOR_DATA))
static void setupmux(void);
static void setupgpu(void);
static void setupinput(void);
static void setuppanels(void);
static void markterminate(void*);
/* routers */
/* input to mux */
static int findinputtomux(void*, char*);
static int exitinputtomux(void*, int);
static int printinputtomux(void*, char*, char*);

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
        EnvRun(input, -1L);
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

}
void 
setupgpu(void) {
    gpu = CreateEnvironment();
    EnvBatchStar(gpu, "microcode/gpu.clp");
}

void 
setupmux(void) {
    mux = CreateEnvironment();
    if (! AllocateEnvironmentData(mux, MULTIPLEXER_TERMINATOR_DATA,
                sizeof(terminatordata),NULL))
    {
        printf("Error allocating environment data for MULTIPLEXER_TERMINATOR_DATA\n");
        exit(EXIT_FAILURE);
    }
    GetMultiplexerTerminatorData(mux)->shouldterminate = 0;
    EnvDefineFunction2(mux, "exit", 'v', PTIEF markterminate, "markterminate", "00a");
    EnvBatchStar(mux, "microcode/mux.clp");
}

void
setuppanels(void) {
    panels = CreateEnvironment();
    EnvBatchStar(panels, "microcode/panels.clp");
}

void 
setupinput(void) {
    input = CreateEnvironment();
    EnvAddRouter(input, inputtomux, 40, findinputtomux, printinputtomux, NULL, NULL, exitinputtomux);
    EnvBatchStar(input, "microcode/input.clp");
}

void
markterminate(void* theEnv) {
    GetMultiplexerTerminatorData(theEnv)->shouldterminate = 1;
}

/* input to mux */
int 
findinputtomux(void* theEnv, char* logicalName) {
    if((strcmp(logicalName, inputtomux) == 0)) {
        return TRUE;
    } else {
        return FALSE;
    }
}
int 
exitinputtomux(void* theEnv, int num) {
    /* nothing needed */
    return 1;
}
int 
printinputtomux(void* theEnv, char* logicalName, char* str) {
    EnvAssertString(mux, str);
    return 1;
}
