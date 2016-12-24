#include "iris_machine.h"

namespace machine {
	LockStepMachine::LockStepMachine() noexcept { }
	LockStepMachine::~LockStepMachine() { }
	void LockStepMachine::initialize() {
		_primary.initialize();
		_secondary.initialize();
		// now we need to install stuff into the primary and secondary so they
		// can communicate with one another
		//_primary.installDevice(iris16::mapData<iris20::word>(&_secondary, iris20::ArchitectureConstants::AddressMax + 1));
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
