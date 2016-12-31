#include "sim_registration.h"
#include "Core.h"
#include <map>
#include "iris.h"
#include "iris18.h"
#include "iris20.h"
#include "iris_machine.h"

namespace stdiris {
	static std::map<std::string, std::function<Core*()>> cores = {
        { "iris20", iris20::newCore },
		{ "iris18", iris18::newCore },
		{ "iris", iris::newCore },
		{ "LockStepMachine-type0", machine::LockStepMachine<8>::newCore },
	};
    Core* getCore(const std::string& name) {
		auto loc = cores.find(name);
		if (loc != cores.end()) {
			return loc->second();
		} else {
			std::stringstream stream;
			stream << "Tried to create a non-existent core: " << name << "!!!";
			throw stdiris::Problem(stream.str());
		}
    }
    void forEachCoreName(std::function<void(const std::string&)> fn) {
        for (auto const& entry : cores) {
            fn(entry.first);
        }
    }
}
