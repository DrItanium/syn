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
	template<byte secondaryCoreCount = 1, byte secondaryCycleCount = 4>
	class LockStepMachine : public iris::Core {
		public:
			static constexpr byte MaxSecondaryCoreCount = 16;
			static LockStepMachine* newCore() noexcept {
				return new LockStepMachine();
			}
			static_assert(secondaryCoreCount != 0, "Can't have zero secondary cores!");
			static_assert(secondaryCoreCount <= MaxSecondaryCoreCount, "Too many cores declared, max is 8");
			static_assert(secondaryCycleCount != 0, "The secondary cores can't be disabled by setting the core counts to zero");
			LockStepMachine() noexcept { }
			virtual ~LockStepMachine() { }
			virtual void initialize() override;
			virtual void installprogram(std::istream& stream) override;
			virtual void dump(std::ostream& stream) override;
			virtual void link(std::istream& input) override { }
			virtual bool cycle() override;
			virtual void shutdown() override;
			byte getSecondaryCycleCount() const {
				return secondaryCycleCount;
			}
		private:
			iris20::Core _primary;
			iris16::Core _secondary[secondaryCoreCount];
	};

	template<byte count, byte cycles>
	void LockStepMachine<count, cycles>::initialize() {
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
		int index = 0;
		for (auto &c : _secondary) {
			c.initialize();
			_primary.installDevice(iris16::mapData<iris20::word, iris20::word>(&c, addressStart));
			c.installIODevice(makeRegisterMapping(register0Id, 0x3));
			c.installIODevice(makeRegisterMapping(register1Id, 0x7));
			register0Id+=2;
			register1Id+=2;
			addressStart += memoryLength;
			int innerIndex = 0;
			auto baseCRegister = 128;
			auto baseDRegister = 129;
			for (auto &other : _secondary) {
				auto base = 0xB + (2 * innerIndex);
				auto creg = static_cast<byte>(baseCRegister);
				auto dreg = static_cast<byte>(baseDRegister);
				if (index != innerIndex) {
					// start the registration process
					c.installIODevice(iris::makeLambdaDevice<iris16::word, iris16::word>(base, 2, 
								[this, base, &other, creg, dreg ](auto addr) -> auto {
									// address 0 is generally the command port
									return other.readRegister(((addr - base) == 0) ? creg : dreg);
								},
								[this, base, &other, creg, dreg] (auto addr, auto value) {
									other.writeRegister(((addr - base) == 0) ? creg : dreg, value);
								}));
				} else {
					c.installIODevice(iris::makeLambdaDevice<iris16::word, iris16::word>(base, 2, 
								[this, base, &c, creg, dreg] (auto addr) -> auto {
									// address 0 is generally the command port
									return c.readRegister(((addr - base) == 0) ? creg : dreg);
								},
								[this, base, &c, creg, dreg] (auto addr, auto value) {
									c.writeRegister(((addr - base) == 0) ? creg : dreg, value);
								}));
				}
				++innerIndex;
				baseCRegister += 2;
				baseDRegister += 2;
			}
			
			++index;
		}
	}
	template<byte count, byte cycle>
	void LockStepMachine<count, cycle>::shutdown() {
		for (auto & _c : _secondary) {
			_c.shutdown();
		}
		_primary.shutdown();
	}

	template<byte count, byte cycleCount>
	bool LockStepMachine<count, cycleCount>::cycle() {
		auto tryInvokeCore = [this](auto& core) {
			if (core.shouldExecute()) {
				core.cycle();
				execute = true;
			}
		};
		execute = false;
		tryInvokeCore(_primary);
		for (auto i = 0; i < count; ++i) {
			auto & _c = _secondary[i];
			for (byte j = 0; j < cycleCount ; ++i) {
				tryInvokeCore(_c);
			}
		}
		return execute;
	}

	template<byte count, byte cycleCount>
	void LockStepMachine<count, cycleCount>::dump(std::ostream& output) {
		_primary.dump(output);
		for (auto & c : _secondary) {
			c.dump(output);
		}
	}

	template<byte count, byte cycleCount>
	void LockStepMachine<count, cycleCount>::installprogram(std::istream& stream) {
		_primary.installprogram(stream);
		for (auto & c : _secondary) {
			c.installprogram(stream);
		}
	}
} // end namespace machine


#endif // end _IRIS_LOCKSTEP_MACHINE_H
