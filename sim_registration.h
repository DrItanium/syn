// Registration of different iris targets
//
#ifndef IRIS_SIM_REGISTRATION
#define IRIS_SIM_REGISTRATION
#include "Core.h"

#include <map>
#include <functional>

namespace iris {
	enum class Architecture {
#define X(name, str, cl) name, 
	#include "architecture_registrations.def"
#undef X
	};
	template<Architecture arch>
	Core* getCore() {
		return nullptr;
	}
	
	
}

#endif // end IRIS_SIM_REGISTRATION
