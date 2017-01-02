// register all of the machine cores here since the cores should not be aware
// of these kinds of registrations
#include "Problem.h"
#include "RegisterEntry.h"
#include "asm_interact.h"
#include "iris.h"
#include "cisc0.h"
#include "hybrid0.h"

template<typename T>
using RegisterAssembler = syn::RegisterEntry<syn::AssemblerRegistrar, T>;
static RegisterAssembler<iris::Core> iris16Core(syn::assemblerRegistry, "iris", iris::assemble);
static RegisterAssembler<cisc0::Core> cisc0Core(syn::assemblerRegistry, "cisc0", cisc0::assemble);
static RegisterAssembler<hybrid0::Core> hybrid0Core(syn::assemblerRegistry, "hybrid0", [](auto a, auto b) { throw syn::Problem("Assembler is done through clips!"); });
