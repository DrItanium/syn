// Registration of different iris targets
//
#ifndef IRIS_SIM_REGISTRATION
#define IRIS_SIM_REGISTRATION
#include "Core.h"
#include "architecture.h"

#include <map>
#include <functional>

namespace iris {
	template<Architecture arch>
	Core* getCore() {
		return nullptr;
	}
}

#endif // end IRIS_SIM_REGISTRATION
