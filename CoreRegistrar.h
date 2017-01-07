// Registration of different syn targets
//
#ifndef IRIS_SIM_REGISTRATION
#define IRIS_SIM_REGISTRATION
#include <string>
#include <functional>
#include <map>
#include <iostream>

namespace syn {
	class Core;
	class CoreRegistrar {
		public:
			using CoreNameOperator = std::function<void(const std::string&)>;
			using Operation = std::function<Core*()>;
			using CoreRegistry = std::map<std::string, Operation>;
		public:
			CoreRegistrar();
			virtual ~CoreRegistrar();
			Core* getCore(const std::string& name);
			void forEachCoreName(CoreNameOperator fn);
            void printEachCoreName(std::ostream& out);
			void addToRegistry(const std::string& name, Operation make);
		private:
			CoreRegistry cores;
	};
	extern CoreRegistrar registry;
}

#endif // end IRIS_SIM_REGISTRATION
