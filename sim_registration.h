// Registration of different stdiris targets
//
#ifndef IRIS_SIM_REGISTRATION
#define IRIS_SIM_REGISTRATION
#include <string>
#include <functional>

namespace stdiris {
	class Core;
	Core* getCore(const std::string& name);
	void forEachCoreName(std::function<void(const std::string&)> fn);
}

#endif // end IRIS_SIM_REGISTRATION
