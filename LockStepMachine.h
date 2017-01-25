// this is a default 
#ifndef _IRIS_LOCKSTEP_MACHINE_H
#define _IRIS_LOCKSTEP_MACHINE_H
#include "Base.h"
#include "ExecutionUnits.h"
#include "Core.h"
#include <cstdint>
#include <memory>
#include "IODevice.h"
#include "iris.h"
#include "MoleculeCore.h"
namespace machine {
	template<byte secondaryCoreCount, byte secondaryCycleCount = 4, byte primaryCycleCount = secondaryCoreCount>
	class LockStepMachine : public syn::Core {
		public:
			static constexpr byte MaxSecondaryCoreCount = 16;
			static LockStepMachine* newCore() noexcept {
				return new LockStepMachine();
			}
			static_assert(secondaryCoreCount != 0, "Can't have zero secondary cores!");
			static_assert(secondaryCoreCount <= MaxSecondaryCoreCount, "Too many cores declared, max is 8");
			static_assert(secondaryCycleCount != 0, "The secondary cores can't be disabled by setting the core counts to zero");
			static_assert(primaryCycleCount != 0, "The number of cycles per machine cycle for the primary molecule core must be greater than zero!");
			LockStepMachine() noexcept { }
			virtual ~LockStepMachine() { }
			virtual void initialize() override;
			virtual void installprogram(std::istream& stream) override;
			virtual void dump(std::ostream& stream) override;
			virtual void link(std::istream& input) override { }
			virtual bool cycle() override;
			virtual void shutdown() override;
			virtual void toggleDebug() override {
				syn::Core::toggleDebug();
				_primary.toggleDebug();
				for(auto &c : _secondary) {
					c.toggleDebug();
				}
			}
			byte getSecondaryCycleCount() const {
				return secondaryCycleCount;
			}
		private:
			molecule::Core _primary;
			iris::Core _secondary[secondaryCoreCount];
	};

	template<byte count, byte cycles, byte pcycle>
	void LockStepMachine<count, cycles, pcycle>::initialize() {
		_primary.initialize();
		auto addressStart = molecule::ArchitectureConstants::AddressMax + 1;
		auto register0Id = 15;
		auto register1Id = 16;
		auto memoryLength = iris::ExposedCoreDataMemory<molecule::word, molecule::word>::computeDataLength();
		auto makeRegisterMapping = [this](auto registerIndex, auto base) -> auto {
			return syn::makeLambdaDevice<iris::word, iris::word>(base, 4,
					[this, base, registerIndex](auto addr) -> auto {
						auto reg = _primary.getRegister(registerIndex);
						switch(addr - base) {
							case 0:
								return syn::decodeBits<molecule::word, iris::word, 0x000000000000FFFF, 0>(reg);
							case 1:
								return syn::decodeBits<molecule::word, iris::word, 0x00000000FFFF0000, 16>(reg);
							case 2:
								return syn::decodeBits<molecule::word, iris::word, 0x0000FFFF00000000, 32>(reg);
							case 3:
								return syn::decodeBits<molecule::word, iris::word, static_cast<molecule::word>(0xFFFF000000000000), 48>(reg);
							default:
								throw syn::Problem("Illegal address provided!");
						}
					},
					[this, base, registerIndex](auto addr, auto value) {
						auto & reg = _primary.getRegister(registerIndex);
						switch(addr - base) {
							case 0:
								reg = syn::encodeBits<molecule::word, iris::word, 0x000000000000FFFF, 0>(reg, value);
							case 1:
								reg = syn::encodeBits<molecule::word, iris::word, 0x00000000FFFF0000, 16>(reg, value);
							case 2:
								reg = syn::encodeBits<molecule::word, iris::word, 0x0000FFFF00000000, 32>(reg, value);
							case 3:
								reg = syn::encodeBits<molecule::word, iris::word, static_cast<molecule::word>(0xFFFF000000000000), 48>(reg, value);
							default:
								throw syn::Problem("Illegal address provided!");
						}

					});
		};
		// now we need to install stuff into the primary and secondary so they
		// can communicate with one another
		int index = 0;
		constexpr auto iris16BaseCRegisterIndex = 128;
		constexpr auto iris16BaseDRegisterIndex = 129;
		for (auto &c : _secondary) {
			auto properBaseAddress = static_cast<iris::word>(0x3);
			c.initialize();
			_primary.installDevice(iris::mapData<molecule::word, molecule::word>(&c, addressStart));
			c.installIODevice(makeRegisterMapping(register0Id, properBaseAddress));
			properBaseAddress += 0x4;
			c.installIODevice(makeRegisterMapping(register1Id, properBaseAddress));
			properBaseAddress += 0x4;
			register0Id+=2;
			register1Id+=2;
			addressStart += memoryLength;
			int innerIndex = 0;
			auto baseCRegister = iris16BaseCRegisterIndex;
			auto baseDRegister = iris16BaseDRegisterIndex;
			for (auto &other : _secondary) {
				auto base = properBaseAddress;
				auto creg = static_cast<byte>(baseCRegister);
				auto dreg = static_cast<byte>(baseDRegister);
				if (index != innerIndex) {
					// start the registration process
					c.installIODevice(syn::makeLambdaDevice<iris::word, iris::word>(base, 2, 
								[this, base, &other, creg, dreg ](auto addr) -> auto {
									// address 0 is generally the command port
									return other.readRegister(((addr - base) == 0) ? creg : dreg);
								},
								[this, base, &other, creg, dreg] (auto addr, auto value) {
									other.writeRegister(((addr - base) == 0) ? creg : dreg, value);
								}));
				} else {
					c.installIODevice(syn::makeLambdaDevice<iris::word, iris::word>(base, 2, 
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
				properBaseAddress += 0x2;
			}
			c.installIODevice(syn::makeLambdaDevice<iris::word, iris::word>(properBaseAddress, 1, 
						[this](auto addr) -> auto { return static_cast<iris::word>(cycles); },
						[](auto addr, auto value) { }));
			properBaseAddress++;
			c.installIODevice(syn::makeLambdaDevice<iris::word, iris::word>(properBaseAddress, 1,
						[this, index](auto addr) -> auto { return static_cast<iris::word>(index); },
						[](auto addr, auto value) { }));
			properBaseAddress++;
			++index;
		}
		_primary.installDevice(syn::makeLambdaDevice<molecule::word, molecule::word>(addressStart, 1, 
					[this](auto address) -> auto { return pcycle; },
					[this](auto a, auto v) { }));
		addressStart++;
	}
	template<byte count, byte cycle, byte pcycle>
	void LockStepMachine<count, cycle, pcycle>::shutdown() {
		for (auto & _c : _secondary) {
			_c.shutdown();
		}
		_primary.shutdown();
	}

	template<byte count, byte cycleCount, byte pcycle>
	bool LockStepMachine<count, cycleCount, pcycle>::cycle() {
		auto tryInvokeCore = [this](auto& core) {
			if (core.shouldExecute()) {
				core.cycle();
				execute = true;
			}
		};
		execute = false;
		for (auto i = 0; i < pcycle; ++i) {
			tryInvokeCore(_primary);
		}
		for (auto i = 0; i < count; ++i) {
			auto & _c = _secondary[i];
			for (byte j = 0; j < cycleCount ; ++i) {
				tryInvokeCore(_c);
			}
		}
		return execute;
	}

	template<byte count, byte cycleCount, byte pcycle>
	void LockStepMachine<count, cycleCount, pcycle>::dump(std::ostream& output) {
		_primary.dump(output);
		for (auto & c : _secondary) {
			c.dump(output);
		}
	}

	template<byte count, byte cycleCount, byte pcycle>
	void LockStepMachine<count, cycleCount, pcycle>::installprogram(std::istream& stream) {
		_primary.installprogram(stream);
		for (auto & c : _secondary) {
			c.installprogram(stream);
		}
	}
} // end namespace machine


#endif // end _IRIS_LOCKSTEP_MACHINE_H
