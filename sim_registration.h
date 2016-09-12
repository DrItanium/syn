// Registration of different iris targets
//
#ifndef IRIS_SIM_REGISTRATION
#define IRIS_SIM_REGISTRATION
#include <string>
#include "Factory.h"


namespace iris {
	class Core;
	Core* getCore(const std::string& value) {
		return Factory<Core>::get(value);
	}
}

#endif // end IRIS_SIM_REGISTRATION
