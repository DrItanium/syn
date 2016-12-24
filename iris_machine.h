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
	class LockStepMachine : public iris::Core {
		public:
			static LockStepMachine* newCore() noexcept {
				return new LockStepMachine();
			}
			LockStepMachine() noexcept;
			virtual ~LockStepMachine();
			virtual void initialize() override;
			virtual void installprogram(std::istream& stream) override;
			virtual void dump(std::ostream& stream) override;
			virtual void link(std::istream& input) override;
			virtual bool cycle() override;
			virtual void shutdown() override;
		private:
			iris20::Core _primary;
			iris16::Core _secondary;
	};
} // end namespace machine


#endif // end _IRIS_MACHINE_H
