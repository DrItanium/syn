#include "sim_registration.h"
#include "Core.h"
#include <map>
#include "iris17.h"
#include "iris16.h"
#include "iris32.h"

namespace iris {
	static std::map<std::string, std::function<Core*()>> cores = {
		{ "iris17", iris17::newCore },
		{ "iris16", iris16::newCore },
		{ "iris32", iris32::newCore },
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
