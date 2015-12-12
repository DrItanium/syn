#ifndef _TARGET_IRIS16_IRIS_H
#define _TARGET_IRIS16_IRIS_H
#include <iris_base.h>
#include <Core.h>
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
	class Core : public iris::Core {
		public:
			Core();
			~Core();
			virtual void initialize();
			virtual void installprogram(std::istream& stream);
			virtual void shutdown();
			virtual void dump(std::ostream& stream);
			virtual void run();
		private:
			bool execute = true;
			word gpr[RegisterCount] = {0};
			word data[AddressMax] = { 0 };
			dword instruction[AddressMax] = { 0 };
			word stack[AddressMax] = { 0 };
			
	};
}
#endif
