#include "Problem.h"
#include "CoreRegistrar.h"
#include "Core.h"
#include <map>
#include <sstream>
#include <iostream>

namespace syn {
	CoreRegistrar registry;
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
    void CoreRegistrar::printEachCoreName(std::ostream& out) {
        return forEachCoreName([&out](const std::string& str) { out << "\t" << str << std::endl; });
    }
}
