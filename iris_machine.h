// this is a default 
#ifndef _IRIS_MACHINE_H
#define _IRIS_MACHINE_H
#include "iris_base.h"
#include "iris_xunits.h"
#include "Core.h"
#include <cstdint>
#include <memory>
#include "IODevice.h"
#include "iris16.h"
#include "iris20.h"
namespace machine {
	using MemoryWord = int64_t;
	using MemoryAddress = uint64_t;
	class Machine : public iris::Core {
		public:
			Machine()
		private:
	};
} // end namespace machine


#endif // end _IRIS_MACHINE_H
