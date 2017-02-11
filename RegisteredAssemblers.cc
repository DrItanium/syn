// register all of the machine cores here since the cores should not be aware
// of these kinds of registrations
#include "Problem.h"
#include "RegisterEntry.h"
#include "AssemblerRegistrar.h"
#include "IrisCore.h"
#include "Cisc0Core.h"

template<typename T>
using RegisterAssembler = syn::RegisterEntry<syn::AssemblerRegistrar, T>;
static RegisterAssembler<iris::Core> iris16Core(syn::assemblerRegistry, "iris", iris::assemble);
static RegisterAssembler<cisc0::Core> cisc0Core(syn::assemblerRegistry, "cisc0", cisc0::assemble);
