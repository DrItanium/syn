#ifndef _IRIS_CORE_H
#define _IRIS_CORE_H
#include <iostream>
#include <typeinfo>
#include "Device.h"
namespace iris {
	 // Generic iris core interface
	 class Core : public Device {
		public:
			virtual void installprogram(std::istream& stream) = 0;
			virtual void dump(std::ostream& stream) = 0;
			virtual void run() = 0;
			virtual void link(std::istream& input) = 0;

	 };
}
#endif
