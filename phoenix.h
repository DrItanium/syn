#ifndef _TARGET_PHOENIX
#define _TARGET_PHOENIX
#include <cstdint>
#include "Core.h"
#include <memory>
#include "iris16.h"
#include "iris17.h"
namespace phoenix {
	using word = uint16_t;
	using Address = uint32_t;

	// memory controller chip which merges many different memory spaces
	// together
	class Core : public iris::Core {
		using MemorySpace = std::shared_ptr<word>;
		public:	
			Core();
			virtual ~Core();
			virtual void initialize() override;
			virtual void installprogram(std::istream& stream) override;
			virtual void shutdown() override;
			virtual void dump(std::ostream& stream) override;
			virtual void run() override;
			virtual void link(std::istream& stream) override;
			void mapMemorySpace(Address start, Address end, MemorySpace space);
		private:
			iris17::Core core;
	};

} // end namespace phoenix
#endif // end _TARGET_PHOENIX
