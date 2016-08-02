#include "architecture.h"
#include "Problem.h"
#include <map>

namespace iris {
	Architecture getArchitectureFromString(const std::string& value) {
		static std::map<std::string, Architecture> translationTable = {
#define X(en, str, __) { str, Architecture:: en },
#include "def/architecture_registrations.def"
#undef X
		};
		if (translationTable.count(value) == 0) {
			throw Problem("Unknown target '" + value + "' !");
		} else {
			return translationTable[value];
		}
		
	}
	std::string getStringFromArchitecture(Architecture arch) {
		static std::map<Architecture, std::string> translationTable = {
#define X(en, str, __) { Architecture:: en, str },
#include "def/architecture_registrations.def"
#undef X
		};
		if (translationTable.count(arch) == 0) {
			throw Problem("Unknown target architecture provided!");
		} else {
			return translationTable[arch];
		}
	}
}
