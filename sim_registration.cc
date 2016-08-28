#include "sim_registration.h"
#include "Problem.h"
#include "targets.h"
#include "Core.h"
#include "architecture.h"
namespace iris {
	Core* getCore(const std::string& value) {
		auto arch = getArchitectureFromString(value);
#define X(en, str, __) if (arch == Architecture:: en) { return en :: newCore(); }
#include "def/architecture_registrations.def"
#undef X
		throw Problem("Unknown architecture provided!");
	}
}
