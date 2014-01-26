#include <u.h>
#include <libc.h>
#include <stdio.h>
#include <clips.h>
#include "processor.h"

#define EXECUTION_UNIT_DATA USER_ENVIRONMENT_DATA + 0 

#define GetExecutionUnit(theEnv) ((executionunit*) GetEnvironmentData(theEnv, EXECUTION_UNIT_DATA))

static void GetFloatRegister(void* theEnv, DATA_OBJECT_PTR ret);
static void GetPredicateRegister(void* theEnv, DATA_OBJECT_PTR ret);
static void GetRegister(void* theEnv, DATA_OBJECT_PTR ret);
static int SetRegister(void* theEnv);
static uvlong GetRegisterCountFunc(void* theEnv);
static void SetExecutionLength(void* theEnv, DATA_OBJECT_PTR ret);
static uint GetExecutionLength(void* theEnv);
static uint GetIndex(void* theEnv);

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

    if (! AllocateEnvironmentData(theEnv,EXECUTION_UNIT_DATA,
                sizeof(executionunit),NULL))
    {
        printf("Error allocating environment data for EXECUTION_UNIT_DATA\n");
        exit(EXIT_FAILURE);
    }

    GetExecutionUnit(theEnv)->index = coreindex;
    GetExecutionUnit(theEnv)->executionlength = DefaultExeuctionLength;
    EnvDefineFunction2(theEnv, "get-register:predicate", 'u', PTIEF GetPredicateRegister, "GetPredicateRegister", "11ii");
    EnvDefineFunction2(theEnv, "get-register:float", 'u', PTIEF GetFloatRegister, "GetFloatRegister", "11ii");
    EnvDefineFunction2(theEnv, "get-register", 'u', PTIEF GetRegister, "GetExecutionRegisterFunc", "11ii");
    EnvDefineFunction2(theEnv, "set-register", 'b', PTIEF SetRegister, "SetExecutionRegisterFunc", "22iin");
    EnvDefineFunction2(theEnv, "register-count", 'g', PTIEF GetRegisterCountFunc, "GetExecutionRegisterCountFunc", "00a");
    EnvDefineFunction2(theEnv, "set-execution-length", 'u', PTIEF SetExecutionLength, "SetExecutionLength", "11ii");
    EnvDefineFunction2(theEnv, "get-execution-length", 'l', PTIEF GetExecutionLength, "GetExecutionLength", "00a");
    EnvDefineFunction2(theEnv,"get-index",'l',PTIEF GetIndex, "GetIndex", "00a");
    cores[coreindex] = theEnv;
    coreindex++;
}

void GetPredicateRegister(void* theEnv, DATA_OBJECT_PTR ret) {
    uint index;
    index = EnvRtnLong(theEnv, 1);
    ret->type = SYMBOL;
    if(index >= 0 && index < CoreRegisterCount) {
        if(GetExecutionUnit(theEnv)->registers[index].pvalue == 0) {
            ret->value = EnvFalseSymbol(theEnv);
        } else {
            ret->value = EnvTrueSymbol(theEnv);
        }
    } else {
        ret->value = EnvAddSymbol(theEnv, "nil");
    }
}

void GetFloatRegister(void* theEnv, DATA_OBJECT_PTR ret) {
    uint index;
    index = EnvRtnLong(theEnv, 1);
    if(index >= 0 && index < CoreRegisterCount) {
        ret->type = FLOAT;
        ret->value = EnvAddDouble(theEnv, GetExecutionUnit(theEnv)->registers[index].fvalue);
    } else {
        ret->type = SYMBOL;
        ret->value = EnvFalseSymbol(theEnv);
    }
}
void GetRegister(void* theEnv, DATA_OBJECT_PTR ret) {
    uint index;
    index = EnvRtnLong(theEnv, 1);
    if(index >= 0 && index < CoreRegisterCount) {
        ret->type = FLOAT;
        ret->value = EnvAddLong(theEnv, GetExecutionUnit(theEnv)->registers[index].ivalue);
    } else {
        ret->type = SYMBOL;
        ret->value = EnvFalseSymbol(theEnv);
    }
}

int SetRegister(void* theEnv) {
    uint index;
    DATA_OBJECT arg0;
    index = EnvRtnLong(theEnv, 1);
    if(index >= 0 && index < CoreRegisterCount && 
            EnvArgTypeCheck(theEnv, "merge", 2, INTEGER_OR_FLOAT, &arg0) == TRUE) {
        if(GetType(arg0) == INTEGER) {
            GetExecutionUnit(theEnv)->registers[index].ivalue = DOToLong(arg0);
        } else {
            GetExecutionUnit(theEnv)->registers[index].fvalue = DOToDouble(arg0);
        }
        return 1;
    } else {
        return 0;
    }

}

uvlong GetRegisterCountFunc(void* theEnv) {
    return CoreRegisterCount;
}

void SetExecutionLength(void* theEnv, DATA_OBJECT_PTR ret) {
    uint count;
    uint oldcount;
    count = EnvRtnLong(theEnv, 1);
    if(count >= 0) {
        oldcount = GetExecutionUnit(theEnv)->executionlength;
        GetExecutionUnit(theEnv)->executionlength = count;
        ret->type = INTEGER;
        ret->value =EnvAddLong(theEnv, oldcount);
    } else {
        ret->type = SYMBOL;
        ret->value = EnvFalseSymbol(theEnv);
    }
}

uint GetExecutionLength(void* theEnv) {
    return GetExecutionUnit(theEnv)->executionlength;
}

uint GetIndex(void* theEnv) {
    return GetExecutionUnit(theEnv)->index;
}
