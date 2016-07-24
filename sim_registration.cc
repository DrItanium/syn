#include "sim_registration.h"
#include "Problem.h"
#include <map>
namespace iris {
	Architecture getArchitecture(const std::string& value);
	Core* getCore(Architecture arch);
	Core* getCore(const std::string& value) {
		return getCore(getArchitecture(value));
	}

	Architecture getArchitecture(const std::string& value) {
		static std::map<std::string, Architecture> translationTable = {
#define X(en, str, __) { str, Architecture:: e },
#include "architecture_registrations.def"
#undef X
		};
		if (translationTable.count(value) == 0) {
			throw Problem("Unknown target selected!");
		} else {
			return translationTable[value];
		}
		
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
