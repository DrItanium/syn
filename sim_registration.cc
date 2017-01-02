#include "Problem.h"
#include "sim_registration.h"
#include "Core.h"
#include <map>
#include <sstream>
//#include "iris.h"
//#include "cisc0.h"
//#include "hybrid0.h"
//#include "iris_machine.h"

namespace syn {
	CoreRegistrar registry;
//	static std::map<std::string, std::function<Core*()>> cores = {
//        { "hybrid0", hybrid0::newCore },
//		{ "cisc0", cisc0::newCore },
//		{ "iris", iris::newCore },
//		{ "LockStepMachine-type0", machine::LockStepMachine<8>::newCore },
//	};
	Core* CoreRegistrar::getCore(const std::string& name) {
		auto loc = cores.find(name);
		if (loc != cores.end()) {
			return loc->second();
		} else {
			std::stringstream stream;
			stream << "Tried to create a non-existent core: " << name << "!!!";
			throw syn::Problem(stream.str());
		}
    }
    void CoreRegistrar::forEachCoreName(std::function<void(const std::string&)> fn) {
        for (auto const& entry : cores) {
            fn(entry.first);
        }
    }
	void CoreRegistrar::addToRegistry(const std::string& name, CoreRegistrar::Operation make) {
		cores.emplace(name, make);
	}

	CoreRegistrar::CoreRegistrar() { }
	CoreRegistrar::~CoreRegistrar() { }
}
