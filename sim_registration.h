// Registration of different iris targets
//
#ifndef IRIS_SIM_REGISTRATION
#define IRIS_SIM_REGISTRATION
#include <string>
#include "Singleton.h"
#include "Factory.h"


namespace iris {
	class Core;
	using CoreFactory = Factory<Core>;
	static Singleton<CoreFactory> cores;
	Core* getCore(const std::string& name) {
		return cores->get(name)();
	}
	using RegisterCore = RegisterAction<CoreFactory>;
}

#endif // end IRIS_SIM_REGISTRATION
