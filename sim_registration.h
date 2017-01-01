// Registration of different syn targets
//
#ifndef IRIS_SIM_REGISTRATION
#define IRIS_SIM_REGISTRATION
#include <string>
#include <functional>
#include <map>

namespace syn {
	class Core;
	class CoreRegistrar {
		public:
			using CoreNameOperator = std::function<void(const std::string&)>;
			using CoreInstantiator = std::function<Core*()>;
			using CoreRegistry = std::map<std::string, CoreInstantiator>;
		public:
			CoreRegistrar();
			virtual ~CoreRegistrar();
			Core* getCore(const std::string& name);
			void forEachCoreName(CoreNameOperator fn);
			void registerCore(const std::string& name, CoreInstantiator make);
		private:
			CoreRegistry cores;
	};
	template<typename T>
	class RegisterCore {
		public:
			RegisterCore(CoreRegistrar& reg, const std::string& name, typename CoreRegistrar::CoreInstantiator make) {
				reg.registerCore(name, make);
			}
	};
	extern CoreRegistrar registry;
}

#endif // end IRIS_SIM_REGISTRATION
