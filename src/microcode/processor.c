#include <u.h>
#include <libc.h>
#include <stdio.h>
#include <clips.h>
#include "processor.h"

static uvlong GetRegisterFunc(void* theEnv);
static int SetRegisterFunc(void* theEnv);
static uvlong GetRegisterCountFunc(void* theEnv);
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
uvlong registercount(void) {
    return RegisterCount;
}

void setregister(uint dest, uvlong value) {
    registers[dest] = value;
}

uvlong getregister(uint dest) {
    return registers[dest];
}

uvlong GetRegisterFunc(void* theEnv) {
    uint dest;
    dest = EnvRtnLong(theEnv, 1);
    if(dest < RegisterCount) {
        return getregister(dest);
    } else {
        return -1;
    }
}
int SetRegisterFunc(void* theEnv) {
    uint dest;
    uvlong value;
    dest = EnvRtnLong(theEnv, 1);
    value = EnvRtnLong(theEnv, 2);

    if(dest < RegisterCount) {
        setregister(dest, value);
        return TRUE;
    } else {
        return FALSE;
    }
}

uvlong GetRegisterCountFunc(void* theEnv) {
    return registercount();
}
void UserFunctions() {

}

void EnvUserFunctions(void* theEnv) {
   EnvDefineFunction2(theEnv, 
           "get-register",
           'g',
           PTIEF GetRegisterFunc,
           "GetRegisterFunc",
           (char*)"11ii");
   EnvDefineFunction2(theEnv, 
           "set-register",
           'b',
           PTIEF SetRegisterFunc,
           "SetRegisterFunc",
           "22iii");
   EnvDefineFunction2(theEnv,
           "register-count",
           'g',
           PTIEF GetRegisterCountFunc,
           "GetRegisterCountFunc",
           "00a");
}
