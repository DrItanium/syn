// Registration of different iris targets
//
#ifndef IRIS_SIM_REGISTRATION
#define IRIS_SIM_REGISTRATION
#include "Core.h"
#include "architecture.h"

#include <string>


namespace iris {
	Core* getCore(const std::string& value);
	template<Architecture arch>
	Core* newCore() {
		return nullptr;
	}

}

#endif // end IRIS_SIM_REGISTRATION
