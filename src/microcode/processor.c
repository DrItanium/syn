#include <u.h>
#include <libc.h>
#include <stdio.h>
#include <clips.h>
#include "processor.h"

#define UNIT_DATA USER_ENVIRONMENT_DATA + 0
/* unit 0 */
#define EXECUTION_UNIT_DATA UNIT_DATA
/* every other unit */
#define SCHEDULER_UNIT_DATA UNIT_DATA

#define GetUnit(theEnv) ((unit*) GetEnvironmentData(theEnv, UNIT_DATA))
#define GetExecutionUnit(theEnv) ((executionunit*) GetEnvironmentData(theEnv, EXECUTION_UNIT_DATA))
#define GetSchedulerUnit(theEnv) ((schedulerunit*) GetEnvironmentData(theEnv, SCHEDULER_UNIT_DATA))

static void GetExecutionRegisterFunc(void* theEnv, DATA_OBJECT_PTR ret);
static int SetExecutionRegisterFunc(void* theEnv);
static uvlong GetExecutionRegisterCountFunc(void* theEnv);
static void SetExecutionLength(void* theEnv);

static void GetSchedulerRegisterFunc(void* theEnv, DATA_OBJECT_PTR ret);
static int SetSchedulerRegisterFunc(void* theEnv);
static uvlong GetSchedulerRegisterCountFunc(void* theEnv);
int main(int argc, char *argv[]) {
    void *theEnv;

    /* Do this ahead of anything else so we don't have issues with
     * initialization across multiple environments */
    /*
       if(initdraw(0,0, "neutron") < 0) {
       sysfatal("initdraw failed: %r");
       exits("initdraw");
       }
       einit(Ekeyboard|Emouse);
       */

    theEnv = CreateEnvironment();
    RerouteStdin(theEnv,argc,argv);
    CommandLoop(theEnv);

    return(-1);
}
void UserFunctions() {

}
void EnvUserFunctions(void* theEnv) {

    if(coreindex == 0) {
        if (! AllocateEnvironmentData(theEnv,,SCHEDULER_UNIT_DATA,
                    sizeof(schedulerunit),NULL))
        {
            printf("Error allocating environment data for SCHEDULER_UNIT_DATA\n");
            exit(EXIT_FAILURE);
        }
        EnvDefineFunction2(theEnv, "get-register", 'u', PTIEF GetSchedulerRegisterFunc, "GetSchedulerRegisterFunc", "11ii");
        EnvDefineFunction2(theEnv, "set-register", 'b', PTIEF SetSchedulerRegisterFunc, "SetSchedulerRegisterFunc", "22iii");
        EnvDefineFunction2(theEnv, "register-count", 'g', PTIEF GetSchedulerRegisterCountFunc, "GetSchedulerRegisterCountFunc", "00a");
    } else {
        if (! AllocateEnvironmentData(theEnv,,EXECUTION_UNIT_DATA,
                    sizeof(executionunit),NULL))
        {
            printf("Error allocating environment data for EXECUTION_UNIT_DATA\n");
            exit(EXIT_FAILURE);
        }

        GetExecutionUnit(theEnv)->executionLength = 128;
        EnvDefineFunction2(theEnv, "get-register", 'u', PTIEF GetExecutionRegisterFunc, "GetExecutionRegisterFunc", "11ii");
        EnvDefineFunction2(theEnv, "set-register", 'b', PTIEF SetExecutionRegisterFunc, "SetExecutionRegisterFunc", "22iii");
        EnvDefineFunction2(theEnv, "register-count", 'g', PTIEF GetExecutionRegisterCountFunc, "GetExecutionRegisterCountFunc", "00a");
        EnvDefineFunction2(theEnv, "set-execution-length", 'g', PTIEF SetExecutionLength, "SetExecutionLength", "11ii");
    }
    EnvDefineFunction2(theEnv,"get-index",'l',PTIEF GetIndex, "GetIndex", "00");
    GetUnit(theEnv)->index = coreindex;
    cores[coreindex] = theEnv;
    coreindex++;
}
