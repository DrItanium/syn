// register all of the machine cores here since the cores should not be aware
// of these kinds of registrations
#include "Problem.h"
#include "RegisterEntry.h"
#include "CoreRegistrar.h"
#include "iris.h"
#include "cisc0.h"
#include "molecule.h"
#include "syn_machines.h"

template<typename T>
using RegisterCore = syn::RegisterEntry<syn::CoreRegistrar, T>;
static RegisterCore<iris::Core> iris16Core(syn::registry, "iris", iris::newCore);
static RegisterCore<cisc0::Core> cisc0Core(syn::registry, "cisc0", cisc0::newCore);
static RegisterCore<molecule::Core> moleculeCore(syn::registry, "molecule", molecule::newCore);
static RegisterCore<machine::LockStepMachine<8>> lockStepMachine_8(syn::registry, "lsm0", machine::LockStepMachine<8>::newCore);
