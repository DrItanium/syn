// Registration of different iris targets
//
#ifndef IRIS_SIM_REGISTRATION
#define IRIS_SIM_REGISTRATION
#include <string>


namespace iris {
	class Core;
	Core* getCore(const std::string& value);
}

#endif // end IRIS_SIM_REGISTRATION
