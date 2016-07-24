// Registration of different iris targets
//
#ifndef IRIS_SIM_REGISTRATION
#define IRIS_SIM_REGISTRATION
#include "Core.h"
#include "architecture.h"
#include "Problem.h"

#include <string>


namespace iris {
	Core* getCore(const std::string& value);
}

#endif // end IRIS_SIM_REGISTRATION
