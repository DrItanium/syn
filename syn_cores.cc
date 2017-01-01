// register all of the machine cores here since the cores should not be aware
// of these kinds of registrations
#include "Problem.h"
#include "sim_registration.h"
#include "iris.h"
#include "cisc0.h"
#include "hybrid0.h"

static syn::RegisterCore<iris::Core> iris16Core(syn::registry, "iris", iris::newCore);
static syn::RegisterCore<cisc0::Core> cisc0Core(syn::registry, "cisc0", cisc0::newCore);
static syn::RegisterCore<hybrid0::Core> hybrid0Core(syn::registry, "hybrid0", hybrid0::newCore);
