#ifndef _IRIS_CORE_H
#define _IRIS_CORE_H
#include <iostream>
namespace iris {
	 // Generic iris core interface
	 class Core {
		public:
			virtual void initialize() = 0;
			virtual void installprogram(std::istream& stream) = 0;
			virtual void shutdown() = 0;
			virtual void dump(std::ostream& stream) = 0;
			virtual void run() = 0;
	 };
}
#endif
