#include "target/iris16/iris.h"

namespace iris16 {
	Core::Core() { }

	void Core::initialize() {

	}

	void Core::installprogram(std::istream& stream) {

	}

	void Core::shutdown() {

	}
	void Core::dump(std::ostream& stream) {

	}
	void Core::run() {
		while(execute) {
			current.decode(instruction[gpr[ArchitectureConstants::InstructionPointerIndex]]);
			dispatch();
			if (advanceIp) {
				gpr[ArchitectureConstants::InstructionPointerIndex]++;
			}
		}
	}
	void Core::dispatch() {
		switch(static_cast<InstructionGroup>(current.getGroup())) {
#define X(_, operation, tag) case tag: operation(); break; 
#include "target/iris16/groups.def"
#undef X
			default:
				std::cerr << "Illegal instruction group " << current.getGroup() << std::endl;
				execute = false;
				break;
		}
	}

	void Core::jump() {

	}
	void Core::misc() {

	}
	void Core::move() {

	}
	void Core::compare() {

	}
}
