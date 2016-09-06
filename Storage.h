#ifndef _TARGET_STORAGE
#define _TARGET_STORAGE
#include <cstdint>
#include "Core.h"
#include <memory>
#include <list>
#include "iris16.h"
namespace Hardware {
	using word = uint16_t;
	using Address = uint32_t;

	class Storage : public iris::Core {
		public:	
			Storage(Address size);
			virtual ~Storage();
			virtual void initialize() override;
			virtual void installprogram(std::istream& stream) override;
			virtual void shutdown() override;
			virtual void dump(std::ostream& stream) override;
			virtual void run() override;
			virtual void link(std::istream& input) override;
		private:
			Address _size;
			std::shared_ptr<word> _backingStore;
			iris16::Core _controller;
	};

} // end namespace Hardware
#endif // end _TARGET_STORAGE
