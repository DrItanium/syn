#include "iris16.h"
#include <functional>
#include <sstream>

namespace iris16 {
	Core* newCore() noexcept {
		return new iris16::Core();
	}


	Core::~Core() { }

	void Core::installprogram(std::istream& stream) {
		char wordBuf[sizeof(word)] = { 0 };
		char dwordBuf[sizeof(dword)] = { 0 };
		for (auto i = 0; i < ArchitectureConstants::RegisterCount; ++i) {
			stream.read(wordBuf, sizeof(word));
			gpr[i] = iris16::encodeWord(wordBuf[0], wordBuf[1]);
		}
		for (auto i = 0; i < ArchitectureConstants::AddressMax; ++i) {
			stream.read(wordBuf, sizeof(word));
			setDataMemory(i, iris16::encodeWord(wordBuf[0], wordBuf[1]));
		}
		for (auto i = 0; i < ArchitectureConstants::AddressMax; ++i) {
			stream.read(dwordBuf, sizeof(dword));
			setInstructionMemory(i, iris16::encodeDword(dwordBuf[0], dwordBuf[1], dwordBuf[2], dwordBuf[3]));
		}
		for (auto i = 0; i < ArchitectureConstants::AddressMax; ++i) {
			stream.read(wordBuf, sizeof(word));
			stack[i] = iris16::encodeWord(wordBuf[0], wordBuf[1]);
		}
	}

	void Core::dump(std::ostream& stream) {
		char wordBuf[sizeof(word)] = { 0 };
		char dwordBuf[sizeof(dword)] = { 0 };
		for (auto i = 0; i < ArchitectureConstants::RegisterCount; ++i) {
			iris::decodeUint16LE(gpr[i], (byte*)wordBuf);
			stream.write(wordBuf, sizeof(word));
		}
		for (auto i = 0; i < ArchitectureConstants::AddressMax; ++i) {
			iris::decodeUint16LE(getDataMemory(i), (byte*)wordBuf);
			stream.write(wordBuf, sizeof(word));
		}
		for (auto i = 0; i < ArchitectureConstants::AddressMax; ++i) {
			iris::decodeUint32LE(instruction[i], (byte*)dwordBuf);
			stream.write(dwordBuf, sizeof(dword));
		}
		for (auto i = 0; i < ArchitectureConstants::AddressMax; ++i) {
			iris::decodeUint16LE(stack[i], (byte*)wordBuf);
			stream.write(wordBuf, sizeof(word));
		}
	}
	void Core::run() {
		while(execute) {
			if (!advanceIp) {
				advanceIp = true;
			}
			current = instruction[gpr[ArchitectureConstants::InstructionPointerIndex]];
			dispatch();
			if (advanceIp) {
				++gpr[ArchitectureConstants::InstructionPointerIndex];
			}
		}
	}
	void Core::dispatch() {
		auto group = static_cast<InstructionGroup>(getGroup());
#define X(name, operation) \
		if (group == InstructionGroup:: name) { \
			operation(); \
			return; \
		}
#include "def/iris16/groups.def"
#undef X
		std::stringstream stream;
		stream << "Illegal instruction group " << getGroup();
		execute = false;
		throw iris::Problem(stream.str());
	}

	void Core::compare() {
		auto cop = static_cast<CompareOp>(getOperation());
#define X(type, compare, mod) \
			if (cop == CompareOp:: type) { \
				gpr[getDestination()] = (gpr[getSource0()] compare gpr[getSource1()]); \
				return; \
			}
#define Y(type, compare, mod) \
			if (cop == CompareOp:: type) { \
				gpr[getDestination()] = (gpr[getSource0()] compare static_cast<word>(getSource1())); \
				return; \
			}
#include "def/iris16/compare.def"
#undef X
#undef Y
		std::stringstream stream;
		stream << "Illegal compare code " << getOperation();
		execute = false;
		advanceIp = false;
		throw iris::Problem(stream.str());
	}

	void Core::arithmetic() {
		switch(static_cast<ArithmeticOp>(getOperation())) {

#define XNone(n) (gpr[getSource0()], gpr[getSource1()])
#define XImmediate(n) (gpr[getSource0()], static_cast<word>(getSource1()))
#define XUnary(n) (gpr[getSource0()])
#define X(name, op, desc) \
			case ArithmeticOp:: name: \
									   gpr[getDestination()] = op INDIRECTOR(X, desc)(name); \
			break;
#include "def/iris16/arithmetic.def"
#undef X
#undef XNone
#undef XDenominator
#undef XUnary
			default: {
		std::stringstream stream;
		stream << "Illegal arithmetic operation " << getOperation();
		execute = false;
		throw iris::Problem(stream.str());
					 }
		}
	}
	void Core::jump() {
		auto jop = static_cast<JumpOp>(getOperation());
		auto ifthenelse = false, conditional = false, iffalse = false, immediate = false,  link = false;
		switch(jop) {
#define X(name, _ifthenelse, _conditional, _iffalse, _immediate, _link) \
			case JumpOp:: name : { \
									 ifthenelse = _ifthenelse; \
									 conditional = _conditional; \
									 iffalse = _iffalse; \
									 immediate = _immediate; \
									 link = _link; \
									 break;\
								 }
#include "def/iris16/jump.def"
#undef X
			
			default: {
						 std::stringstream ss;
						 ss << "Illegal jump code " << std::hex << static_cast<int>(getOperation());
						 execute =  false;
						 throw iris::Problem(ss.str());
					 }
		}
		auto newAddr = static_cast<word>(0);
		auto cond = true;
		advanceIp = false;
		auto ip = gpr[ArchitectureConstants::InstructionPointerIndex];
		if (conditional) {
			auto dest = gpr[getDestination()];
			cond = (iffalse ? (dest == 0) : (dest != 0));
			if (ifthenelse) {
				newAddr = gpr[cond ? getSource0() : getSource1()];
			} else {
				newAddr = cond ? (immediate ? getImmediate() : gpr[getSource0()]) : ip + 1;
			}
		} else {
			newAddr = immediate ? getImmediate() : gpr[getDestination()];
		}
		gpr[ArchitectureConstants::InstructionPointerIndex] = newAddr;
		if (link && cond) {
			gpr[ArchitectureConstants::LinkRegisterIndex] = ip + 1;
		}
	}

	void Core::misc() {
		auto op = static_cast<MiscOp>(getOperation());
#define X(name, func) \
		if (op == MiscOp:: name) { \
			func () ; \
			return; \
		}
#include "def/iris16/misc.def"
#undef X
		std::stringstream ss;
		ss << "Illegal misc code " << getOperation();
		execute = false;
		advanceIp = false;
		throw iris::Problem(ss.str());
	}
	void Core::systemCall() {
		auto target = static_cast<SystemCalls>(getDestination());
		if (target == SystemCalls::Terminate) {
			execute = false;
			advanceIp = false;
		} else if (target == SystemCalls::PutC) {
			// read register 0 and register 1
			std::cout.put(static_cast<char>(gpr[getSource0()]));
		} else if (target == SystemCalls::GetC) {
			auto value = static_cast<byte>(0);
			std::cin >> std::noskipws >> value;
			gpr[getSource0()] = static_cast<word>(value);
		} else if (target == SystemCalls::InitializeXMem) {
			// just load the given storage size into r0 and r1
			gpr[getSource0()] = iris::decodeBits<dword, word, 0x0000FFFF, 0>(extendedMemorySize);
			gpr[getSource1()] = iris::decodeBits<dword, word, 0xFFFF0000, 16>(extendedMemorySize);
		} else {
			std::stringstream stream;
			stream << "Illegal system call " << std::hex << getDestination();
			execute = false;
			advanceIp = false;
			throw iris::Problem(stream.str());
		}
	}
	void Core::move() {
		auto op = static_cast<MoveOp>(getOperation());
		if (op == MoveOp::Move) {
			gpr[getDestination()] = gpr[getSource0()];
		} else if (op == MoveOp::Set) {
			gpr[getDestination()] = static_cast<word>(getImmediate());
		} else if (op == MoveOp::Swap) {
			auto result = gpr[getDestination()];
			gpr[getDestination()] = gpr[getSource0()];
			gpr[getSource0()] = result;
		} else if (op == MoveOp::Load) {
			gpr[getDestination()] = getDataMemory(gpr[getSource0()]);
		} else if (op == MoveOp::LoadImmediate) {
			gpr[getDestination()] = getDataMemory(getImmediate());
		} else if (op == MoveOp::Store) {
			setDataMemory(gpr[getDestination()], gpr[getSource0()]);
		} else if (op == MoveOp::Memset) {
			setDataMemory(gpr[getDestination()], getImmediate());
		} else if (op == MoveOp::Push) {
			++gpr[ArchitectureConstants::StackPointerIndex];
			stack[gpr[ArchitectureConstants::StackPointerIndex]] = gpr[getDestination()];
		} else if (op == MoveOp::PushImmediate) {
			++gpr[ArchitectureConstants::StackPointerIndex];
			stack[gpr[ArchitectureConstants::StackPointerIndex]] = getImmediate();
		} else if (op == MoveOp::Pop) {
			gpr[getDestination()] = stack[gpr[ArchitectureConstants::StackPointerIndex]];
			--gpr[ArchitectureConstants::StackPointerIndex];
		} else if (op == MoveOp::LoadCode) {
			auto result = instruction[gpr[getDestination()]];
			gpr[getSource0()] = iris::getLowerHalf(result);
			gpr[getSource1()] = iris::getUpperHalf(result);
		} else if (op == MoveOp::StoreCode) {
			instruction[getDestination()] = encodeDword(gpr[getSource0()], gpr[getSource1()]);
		} else if (op == MoveOp::ExtendedMemoryWrite) {
			// store destination in the address described by source0 and source1
			auto result = gpr[getDestination()];
			auto lower = gpr[getSource0()];
			auto upper = gpr[getSource1()];
			// build an address out of this
			setExtendedDataMemory(iris::encodeUint32LE(lower, upper), result);
		} else if (op == MoveOp::ExtendedMemoryRead) {
			auto lower = gpr[getSource0()];
			auto upper = gpr[getSource1()];
			// build an address out of this
			gpr[getDestination()] = getExtendedDataMemory(iris::encodeUint32LE(lower, upper));
		} else {
			std::stringstream ss;
			ss << "Illegal move code " << getOperation();
			execute = false;
			advanceIp = false;
			throw iris::Problem(ss.str());
		}
	}

	enum class Segment  {
		Code,
		Data,
		Count,
	};
	void Core::link(std::istream& input) {
		char buf[8] = {0};
		for(auto lineNumber = static_cast<int>(0); input.good(); ++lineNumber) {
			input.read(buf, 8);
			if (input.gcount() < 8 && input.gcount() > 0) {
				throw iris::Problem("unaligned object file found!");
			} else if (input.gcount() == 0) {
				if (input.eof()) {
					break;
				} else {
					throw iris::Problem("Something bad happened while reading input file!");
				}
			}
			//ignore the first byte, it is always zero
			auto target = static_cast<Segment>(buf[1]);
			auto address = iris16::encodeWord(buf[2], buf[3]);
			if (debugEnabled()) {
				std::cerr << "current target = " << static_cast<int>(target) << "\tcurrent address = 0x" << std::hex << address << std::endl;
			}
			if (target == Segment::Code) {
				auto result = iris16::encodeDword(buf[4], buf[5], buf[6], buf[7]);
				if (debugEnabled()) {
					std::cerr << " code result: 0x" << std::hex << result << std::endl;
				}
				setInstructionMemory(address, result);
			} else if (target == Segment::Data) {
				auto result = iris16::encodeWord(buf[4], buf[5]);
				if (debugEnabled()) {
					std::cerr << " data result: 0x" << std::hex << result << std::endl;
				}
				setDataMemory(address, result);
			} else {
				std::stringstream str;
				str << "error: line " << lineNumber << ", unknown segment " << static_cast<int>(target) << "/" << static_cast<int>(buf[1]) << std::endl;
				str << "current address: " << std::hex << address << std::endl;
				throw iris::Problem(str.str());
			}
		}
	}

	Core::Core() noexcept { }
	Core::Core(std::shared_ptr<word> extendedMemory, dword size) noexcept : extendedData(extendedMemory), extendedMemorySize(size) { }

	word* Core::dataMapping(word address) {
		return &data[address];
	}
	word* Core::registerMapping(byte index) {
		return &gpr[index];
	}
	void Core::setExtendedDataMemory(dword address, word value) {
		if (extendedMemorySize != 0) {
			if (address < extendedMemorySize) {
				extendedData.get()[address] = value;
			} else {
				throw iris::Problem("Attempted to write to an address outside the mapped extended memory range");
			}
		} else {
			throw iris::Problem("Attempted to write to non existent extended memory!");
		}
	}

	word Core::getExtendedDataMemory(dword address) {
		if (extendedMemorySize != 0) {
			if (address < extendedMemorySize) {
				return extendedData.get()[address];
			} else {
				throw iris::Problem("Attempted to read from an address outside the mapped extended memory range");
			}
		} else {
			throw iris::Problem("Attempted to read from non existent extended memory!");
		}
	}
}
