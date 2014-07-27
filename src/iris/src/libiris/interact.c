#include <stdint.h>
#include "clips.h"
#include "iris.h"
#define NO_ARGS "00a"
#define DeclareEnumTransferFunction(name, func) \
	EnvDefineFunction2(theEnv, name, 'i', PTIEF func, #func, NO_ARGS)
static word iris_interact_getregister(void*);
static void iris_interact_putregister(void*);

static word iris_interact_registercount(void*);
static dword iris_interact_memorysize(void*);
static word iris_interact_predicateregisterindex(void*);
static word iris_interact_stackpointerindex(void*);

static word iris_interact_getpc(void*);
static void iris_interact_setpc(void*);

static void iris_interact_setcodememory(void*);
static dword iris_interact_getcodememory(void*);

static void iris_interact_setdatamemory(void*);
static word iris_interact_getdatamemory(void*);


void iris_declarations(void* theEnv) {
	iris_core* curr;
	if (! AllocateEnvironmentData(theEnv, IRIS_CORE_DATA,
				sizeof(iris_core), NULL)) {
		iris_error("Error allocating environment data for iris_core", ErrorUnableToAllocateCore);
	}
	curr = GetIrisCoreData(theEnv);
	iris_rom_init(curr);
	EnvDefineFunction2(theEnv, "get-register", 'i', PTIEF iris_interact_getregister, "iris_interact_getregister", "11i");
	EnvDefineFunction2(theEnv, "put-register", 'v', PTIEF iris_interact_putregister, "iris_interact_putregister", "22i");
	DeclareEnumTransferFunction("register-count", iris_interact_registercount);
	DeclareEnumTransferFunction("memory-size", iris_interact_memorysize);
	DeclareEnumTransferFunction("predicate-register", iris_interact_predicateregisterindex);
	DeclareEnumTransferFunction("stack-pointer", iris_interact_stackpointerindex);
	EnvDefineFunction2(theEnv, "get-pc", 'i', PTIEF iris_interact_getpc, "iris_interact_getpc", "11i");
	EnvDefineFunction2(theEnv, "put-pc", 'v', PTIEF iris_interact_setpc, "iris_interact_setpc", "22i");
	EnvDefineFunction2(theEnv, "set-code-memory", 'v', PTIEF iris_interact_setcodememory, "iris_interact_setcodememory ","22i");
	EnvDefineFunction2(theEnv, "get-code-memory", 'i', PTIEF iris_interact_getcodememory, "iris_interact_getcodememory ","11i");
	EnvDefineFunction2(theEnv, "set-data-memory", 'v', PTIEF iris_interact_setdatamemory, "iris_interact_setdatamemory ","22i");
	EnvDefineFunction2(theEnv, "get-data-memory", 'i', PTIEF iris_interact_getdatamemory, "iris_interact_getdatamemory ","11i");
}

void iris_interact_putregister(void* theEnv) {
	iris_put_register(GetIrisCoreData(theEnv), 
			(byte)EnvRtnLong(theEnv, 1),
			(word)EnvRtnLong(theEnv, 2));
}

word iris_interact_getregister(void* theEnv) {
	return iris_get_register(GetIrisCoreData(theEnv), 
			                 (byte)EnvRtnLong(theEnv, 1));
}

word iris_interact_registercount(void* theEnv) { return RegisterCount; }
dword iris_interact_memorysize(void* theEnv) { return MemorySize; }
word iris_interact_predicateregisterindex(void* theEnv) { return PredicateRegisterIndex; }
word iris_interact_stackpointerindex(void* theEnv) { return StackPointerRegisterIndex; }

word iris_interact_getpc(void* theEnv) {
	iris_core* c;
	c = GetIrisCoreData(theEnv);
	return c->pc;
}

void iris_interact_setpc(void* theEnv) {
	iris_core* c;
	c = GetIrisCoreData(theEnv);
	c->pc = (word)EnvRtnLong(theEnv, 1);
}

void iris_interact_setcodememory(void* theEnv) {
	iris_core* c;
	instruction q;
	word addr;
	c = GetIrisCoreData(theEnv);
	addr = (word)EnvRtnLong(theEnv, 1);
	q.full = (dword)EnvRtnLong(theEnv, 2);
	c->code[addr] = q;
}

dword iris_interact_getcodememory(void* theEnv) {
	iris_core* c;
	word addr;
	c = GetIrisCoreData(theEnv);
	addr = (word)EnvRtnLong(theEnv, 1);
	return c->code[addr].full;
}

void iris_interact_setdatamemory(void* theEnv) {
	iris_core* c;
	word addr, value;
	c = GetIrisCoreData(theEnv);
	addr = (word)EnvRtnLong(theEnv, 1);
	value = (word)EnvRtnLong(theEnv, 2);
	c->data[addr] = value;
}

word iris_interact_getdatamemory(void* theEnv) {
	iris_core* c;
	word addr;
	c = GetIrisCoreData(theEnv);
	addr = (word)EnvRtnLong(theEnv, 1);
	return c->data[addr];
}

