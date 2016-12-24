#ifndef _IRIS_CORE_H
#define _IRIS_CORE_H
#include <iostream>
#include <typeinfo>
#include "Device.h"
namespace iris {
	 // Generic iris core interface
	 class Core : public Device {
		public:
			virtual void dump(std::ostream& stream) = 0;
			virtual void installprogram(std::istream& stream) = 0;
			virtual void run() {
				do {
					execute = cycle();
				} while(execute);
			}

			virtual void link(std::istream& input) = 0;
			virtual bool cycle() = 0;
			inline bool shouldExecute() const { return execute; }
		protected:
			bool execute = true;
	 };
}
#endif
