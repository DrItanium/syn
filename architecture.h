// architecture related definitions

#ifndef IRIS_ARCHITECTURE
#define IRIS_ARCHITECTURE
#include <string>
namespace iris {
	enum class Architecture {
#define X(name, str, cl) name, 
	#include "def/architecture_registrations.def"
#undef X
	};
	Architecture getArchitectureFromString(const std::string& value);
	std::string getStringFromArchitecture(Architecture arch);
}

#endif // end IRIS_ARCHITECTURE
