#ifndef _TARGET_PHOENIX
#define _TARGET_PHOENIX
#include <cstdint>
#include "Core.h"
#include <memory>
#include <list>
#include "iris16.h"
#include "iris17.h"
namespace phoenix {
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
		private:
			Address _size;
			std::shared_ptr<word> _backingStore;
			iris16::Core _controller;
	};
	class Machine : public iris::Core {
		public:
			Machine();
			virtual ~Machine(); 
			virtual void initialize() override;
			virtual void installprogram(std::istream& stream) override;
			virtual void shutdown() override;
			virtual void dump(std::ostream& stream) override;
			virtual void run() override;
		private:
			std::unique_ptr<iris17::Core> _primaryCPU;
			std::unique_ptr<iris16::Core> _ioController, _memoryController;
			std::shared_ptr<word> _cpuMemory;
			std::shared_ptr<word> _hddStorage;
	};

} // end namespace phoenix
#endif // end _TARGET_PHOENIX
