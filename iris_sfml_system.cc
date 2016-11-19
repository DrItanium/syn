#include "iris_base.h"
#include "iris_clips.h"
#include "iris_sfml_system.h"
#include <SFML/System.hpp>
#include <sstream>

namespace iris {
	using TimeExternalAddress = ExternalAddressWrapper<sf::Time>;
	DefWrapperSymbolicName(sf::Time, "sf-time");
	DefPrintStatement(sf::Time, time);
	void CLIPS_newTime(void* env, DATA_OBJECT* ret) {
		static bool init = false;
		static std::string funcStr;
		static std::string funcErrorPrefix;
		static std::string type;
		if (init) {
			init = false;
			type = TypeToName<sf::Time>::getSymbolicName();
			std::stringstream ss, ss2;
			ss << "new (" << type << ")";
			funcStr = ss.str();
			ss2 << "Function " << funcStr;
			funcErrorPrefix = ss2.str();
		}
		try {
			if (EnvRtnArgCount(env) == 1) {
				//TODO: continue this
			} else {
				// todo: add error statement here
				CVSetBoolean(ret, false);
			}
		} catch( iris::Problem p) {
			//TODO: add error statement handling here
			CVSetBoolean(ret, false);
		}
	}
	using ClockExternalAddress = ExternalAddressWrapper<sf::Clock>;
	DefWrapperSymbolicName(sf::Clock, "sf-clock");
	DefPrintStatement(sf::Clock, clock);
	// TODO: continue implementing this
	void installSfmlSystemExtensions(void* env) {
		
	}
}
