#ifndef _TARGET_IRIS16_IRIS_H
#define _TARGET_IRIS16_IRIS_H
#include <iris_base.h>
#include <cstdint>
namespace iris16 {
	typedef uint16_t word;
	typedef uint32_t dword;
	typedef dword raw_instruction;
	typedef word immediate;
	enum ArchitectureConstants  {
		RegisterCount = 256,
		AddressMax = 65535,
	};
	class Core {
		Core();
		~Core();
		private:
			word gpr[RegisterCount] = {0};
			word data[AddressMax] = { 0 };
			dword instruction[AddressMax] = { 0 };
			word stack[AddressMax] = { 0 };
			
	};
}
#endif
