// Registration of different iris targets
//
#ifndef IRIS_SIM_REGISTRATION
#define IRIS_SIM_REGISTRATION
#include <string>
#include <functional>

namespace iris {
	class Core;
	Core* getCore(const std::string& name);
	void forEachCoreName(std::function<void(const std::string&)> fn);
}

#endif // end IRIS_SIM_REGISTRATION
