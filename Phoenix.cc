#include "phoenix.h"

namespace phoenix {
	Machine::Machine() : 
		_hddStorage(new word[DiskSize]), // 1 gb disk
		_primaryCPU(new iris17::Core),
		_ioController(new iris16::Core),
		_memoryController(new iris16::Core),
		_hdd(new Storage(DiskSize)),
    {
		// need to setup a memory map :)
	}
} // end namespace phoenix
