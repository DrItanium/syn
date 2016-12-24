#include "sim_registration.h"
#include "Core.h"
#include <map>
#include "iris18.h"
#include "iris16.h"
#include "iris17.h"
#include "iris19.h"
#include "iris20.h"
#include "iris_machine.h"

namespace iris {
	static std::map<std::string, std::function<Core*()>> cores = {
        { "iris20", iris20::newCore },
		{ "iris19", iris19::newCore },
		{ "iris18", iris18::newCore },
		{ "iris17", iris17::newCore },
		{ "iris16", iris16::newCore },
		{ "LockStepMachine-type0-1", machine::LockStepMachine<1>::newCore },
		{ "LockStepMachine-type0-2", machine::LockStepMachine<2>::newCore },
		{ "LockStepMachine-type0-3", machine::LockStepMachine<3>::newCore },
		{ "LockStepMachine-type0-4", machine::LockStepMachine<4>::newCore },
		{ "LockStepMachine-type0-5", machine::LockStepMachine<5>::newCore },
		{ "LockStepMachine-type0-6", machine::LockStepMachine<6>::newCore },
		{ "LockStepMachine-type0-7", machine::LockStepMachine<7>::newCore },
		{ "LockStepMachine-type0-8", machine::LockStepMachine<8>::newCore },

	};
    Core* getCore(const std::string& name) {
		auto loc = cores.find(name);
		if (loc != cores.end()) {
			return loc->second();
		} else {
			std::stringstream stream;
			stream << "Tried to create a non-existent core: " << name << "!!!";
			throw iris::Problem(stream.str());
		}
    }
    void forEachCoreName(std::function<void(const std::string&)> fn) {
        for (auto const& entry : cores) {
            fn(entry.first);
        }
    }
}
