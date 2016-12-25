// this is a default 
#ifndef _IRIS_LOCKSTEP_MACHINE_H
#define _IRIS_LOCKSTEP_MACHINE_H
#include "iris_base.h"
#include "iris_xunits.h"
#include "Core.h"
#include <cstdint>
#include <memory>
#include "IODevice.h"
#include "iris16.h"
#include "iris20.h"
namespace machine {
	template<byte secondaryCoreCount = 1>
	class LockStepMachine : public iris::Core {
		public:
			static LockStepMachine* newCore() noexcept {
				return new LockStepMachine();
			}
			static_assert(secondaryCoreCount != 0, "Can't have zero secondary cores!");
			static_assert(secondaryCoreCount <= 8, "Too many cores declared, max is 8");
			LockStepMachine() noexcept { }
			virtual ~LockStepMachine() { }
			virtual void initialize() override;
			virtual void installprogram(std::istream& stream) override;
			virtual void dump(std::ostream& stream) override;
			virtual void link(std::istream& input) override { }
			virtual bool cycle() override;
			virtual void shutdown() override;
		private:
			iris20::Core _primary;
			iris16::Core _secondary[secondaryCoreCount];
	};

	template<byte count>
	void LockStepMachine<count>::initialize() {
		_primary.initialize();
		auto addressStart = iris20::ArchitectureConstants::AddressMax + 1;
		auto register0Id = 15;
		auto register1Id = 16;
		auto memoryLength = iris16::ExposedCoreDataMemory<iris20::word, iris20::word>::computeDataLength();
		auto makeRegisterMapping = [this](auto registerIndex, auto base) -> auto {
			return iris::makeLambdaDevice<iris16::word, iris16::word>(base, 4,
					[this, base, registerIndex](auto addr) -> auto {
						auto reg = _primary.getRegister(registerIndex);
						switch(addr - base) {
							case 0:
								return iris::decodeBits<iris20::word, iris16::word, 0x000000000000FFFF, 0>(reg);
							case 1:
								return iris::decodeBits<iris20::word, iris16::word, 0x00000000FFFF0000, 16>(reg);
							case 2:
								return iris::decodeBits<iris20::word, iris16::word, 0x0000FFFF00000000, 32>(reg);
							case 3:
								return iris::decodeBits<iris20::word, iris16::word, static_cast<iris20::word>(0xFFFF000000000000), 48>(reg);
							default:
								throw iris::Problem("Illegal address provided!");
						}
					},
					[this, base, registerIndex](auto addr, auto value) {
						auto & reg = _primary.getRegister(registerIndex);
						switch(addr - base) {
							case 0:
								reg = iris::encodeBits<iris20::word, iris16::word, 0x000000000000FFFF, 0>(reg, value);
							case 1:
								reg = iris::encodeBits<iris20::word, iris16::word, 0x00000000FFFF0000, 16>(reg, value);
							case 2:
								reg = iris::encodeBits<iris20::word, iris16::word, 0x0000FFFF00000000, 32>(reg, value);
							case 3:
								reg = iris::encodeBits<iris20::word, iris16::word, static_cast<iris20::word>(0xFFFF000000000000), 48>(reg, value);
							default:
								throw iris::Problem("Illegal address provided!");
						}

					});
		};
		// now we need to install stuff into the primary and secondary so they
		// can communicate with one another
		for (auto &c : _secondary) {
			c.initialize();
			_primary.installDevice(iris16::mapData<iris20::word, iris20::word>(&c, addressStart));
			c.installIODevice(makeRegisterMapping(register0Id, 0x3));
			c.installIODevice(makeRegisterMapping(register1Id, 0x7));
			register0Id++;
			register1Id++;
			addressStart += memoryLength;
		}
	}
	template<byte count>
	void LockStepMachine<count>::shutdown() {
		for (auto & _c : _secondary) {
			_c.shutdown();
		}
		_primary.shutdown();
	}

	template<byte count>
	bool LockStepMachine<count>::cycle() {
		static constexpr int secondaryCoreCyclesPerMachineCycle = 4;
		auto tryInvokeCore = [this](auto& core) {
			if (core.shouldExecute()) {
				core.cycle();
				execute = true;
			}
		};
		execute = false;
		tryInvokeCore(_primary);
		// probably should run at least 4 - 8 cycles before coming back to
		// the iris20 core!
		for (auto i = 0; i < count; ++i) {
			auto & _c = _secondary[i];
			for (int j = 0; j < secondaryCoreCyclesPerMachineCycle; ++i) {
				tryInvokeCore(_c);
			}
		}
		return execute;
	}

	template<byte count>
	void LockStepMachine<count>::dump(std::ostream& output) {
		_primary.dump(output);
		for (auto & c : _secondary) {
			c.dump(output);
		}
	}

	template<byte count>
	void LockStepMachine<count>::installprogram(std::istream& stream) {
		_primary.installprogram(stream);
		for (auto & c : _secondary) {
			c.installprogram(stream);
		}
	}
} // end namespace machine


#endif // end _IRIS_LOCKSTEP_MACHINE_H
