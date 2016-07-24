#include "sim_registration.h"
#include "Problem.h"
#include "targets.h"
namespace iris {
	Core* getCore(const std::string& value) {
		switch (getArchitectureFromString(value)) {
#define X(en, str, __) \
			case Architecture:: en: \
									return en :: newCore();
#include "architecture_registrations.def"
#undef X
			default:
				throw Problem("Unknown architecture provided!");
		}
	}
}
