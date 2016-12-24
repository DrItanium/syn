#include "iris_machine.h"

namespace machine {
	LockStepMachine::LockStepMachine() noexcept { }
	LockStepMachine::~LockStepMachine() { }
	void LockStepMachine::initialize() {
		_primary.initialize();
		_secondary.initialize();
		// now we need to install stuff into the primary and secondary so they
		// can communicate with one another
		_primary.installDevice(iris16::mapData<iris20::word, iris20::word>(&_secondary, iris20::ArchitectureConstants::AddressMax + 1));
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
		_secondary.installIODevice(makeRegisterMapping(15, 0x3));
		_secondary.installIODevice(makeRegisterMapping(16, 0x7));
	}
	void LockStepMachine::shutdown() {
		_secondary.shutdown();
		_primary.shutdown();
	}

	bool LockStepMachine::cycle() {
		execute = false;
		if (_primary.shouldExecute()) {
			_primary.cycle();
			execute = true;
		}
		if (_secondary.shouldExecute()) {
			_secondary.cycle();
			execute = true;
		}
		return execute;
	}

	void LockStepMachine::link(std::istream& input) {

	}

	void LockStepMachine::dump(std::ostream& output) {

	}

	void LockStepMachine::installprogram(std::istream& stream) {

	}

} // end namespace machine
