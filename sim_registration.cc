#include "sim_registration.h"
#include "Problem.h"
namespace iris {
	Core* getCore(Architecture arch);
	Core* getCore(const std::string& value) {
		return getCore(getArchitectureFromString(value));
	}

	Core* getCore(Architecture arch) {
		switch(arch) {
#define X(en, str, __) \
			case Architecture:: en: \
									return newCore<Architecture:: en>();
#include "architecture_registrations.def"
#undef X
			default:
				throw Problem("Unknown architecture provided!");
		}
	}
}
