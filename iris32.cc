#include "iris32.h"
#include <functional>
#include "Problem.h"
#include "iris_base.h"
#include <sstream>

namespace iris32 {
	Core* newCore() noexcept {
		return new iris32::Core(iris32::ArchitectureConstants::AddressMax, 8);
	}
	constexpr word encodeWord(byte a, byte b, byte c, byte d)  noexcept {
		return iris::encodeInt32LE(a, b, c, d);
	}
	DecodedInstruction::DecodedInstruction(word inst) : raw(inst) { }
#define X(title, mask, shift, type, isreg, field) \
	type DecodedInstruction:: get ## title ( )  const { \
		return iris::decodeBits<word, type, mask, shift>(raw); \
	} \
	void DecodedInstruction:: set ## title ( type value ) { \
		raw = iris::encodeBits<word, type, mask, shift>(raw, value); \
	}
#include "def/iris32/instruction.def"
#undef X

	void Core::write(word addr, word value) {
		if (addr >= 0 && addr < memorySize) {
			memory.get()[addr] = value;
		} else {
			std::stringstream ss;
			ss << "write: Address " << std::hex << addr << " is out of range of" << std::hex << memorySize;
			execute = false;
			throw iris::Problem(ss.str());
		}
	}
	word Core::read(word address) {
		if (address >= 0 && address < memorySize) {
			return memory.get()[address];
		} else {
			std::stringstream ss;
			ss << "read: Address " << std::hex << address << " is out of range of " << std::hex << memorySize;
			execute = false;
			throw iris::Problem(ss.str());
		}
	}

	Core::Core(word msize, byte numThreads) : memorySize(msize), memory(new word[msize]), threads(numThreads) { }

	Core::~Core() { }

	void Core::installprogram(std::istream& stream) {
		// read directly from the assembler's output
		char a[sizeof(word)] = { 0 };
		char b[sizeof(word)] = { 0 };
		while(stream.good()) {
			stream.read(a, sizeof(word));
			if (!stream.good()) {
				if (stream.gcount() > 0) {
					std::cerr << "panic: provided data is not valid iris32 encoded assembler" << std::endl;
					exit(1);
				} else {
					break;
				}
			}
			auto addr = word(a[0]);
			addr = (addr & 0xFFFF00FF) | (word(a[1]) << 8);
			addr = (addr & 0xFF00FFFF) | (word(a[2]) << 16);
			addr = (addr & 0x00FFFFFF) | (word(a[3]) << 24);
			stream.read(b, sizeof(word));
			if (stream.gcount() != sizeof(word)) {
				std::cerr << "panic: provided data is not valid iris32 encoded assembler" << std::endl;
				exit(1);
			}
			auto data = word(b[0]);
			data = (data & 0xFFFF00FF) | (word(b[1]) << 8);
			data = (data & 0xFF00FFFF) | (word(b[2]) << 16);
			data = (data & 0x00FFFFFF) | (word(b[3]) << 24);
			if (debugEnabled()) {
				std::cerr << "install 0x" << std::hex << data
					<< " @ 0x" << std::hex << addr << std::endl;
			}
			memory.get()[addr] = data;
		}
	}
	void Core::dump(std::ostream& stream) {
		char storage[sizeof(word)] = { 0 };
		for (word i = 0; i < memorySize; ++i) {
			auto cell = memory.get()[i];
			for (int j = 0; j < int(sizeof(word)); ++j) {
				storage[j] = byte(cell >> (8 * j));
			}
			stream.write(storage, sizeof(word));
		}
	}
	template<typename T, T value>
		void invoke(Core* c, DecodedInstruction&& inst) {
			throw "unimplemented invocation";
		}

	// jump operations
	template<bool ifthenelse, bool conditional, bool iffalse, bool immediate, bool link>
		void invokeJump(Core* core, DecodedInstruction&& inst) {
			const auto &thread = core->thread;
			auto newAddr = static_cast<word>(0);
			auto cond = false;
			thread->advanceIp = false;
			auto ip = core->thread->gpr[inst.getDestination()];
			if (conditional) {
				auto dest = thread->gpr[inst.getDestination()];
				cond = iffalse ? (dest == 0) : (dest != 0);
				if (ifthenelse) {
					newAddr = thread->gpr[cond ? inst.getSource0() : inst.getSource1()];
				} else {
					if (cond) {
						if(immediate) {
							newAddr = inst.getImmediate();
						} else {
							newAddr = thread->gpr[inst.getSource0()];
						}
					} else {
						newAddr = ip + 1;
					}
				}
			} else {
				if (immediate) {
					newAddr = inst.getImmediate();
				} else {
					newAddr = thread->gpr[inst.getSource0()];
				}
			}
			thread->gpr[ArchitectureConstants::InstructionPointerIndex] = newAddr;
			if (link) {
				if (conditional) {
					if (cond) {
						thread->gpr[ArchitectureConstants::LinkRegisterIndex] = ip + 1;
					}
				} else {
					thread->gpr[ArchitectureConstants::LinkRegisterIndex] = ip + 1;
				}
			}
		}


	// move operations
	template<MoveOp op>
		void invokeMove(Core* core, DecodedInstruction&& inst) {
			const auto &thread = core->thread;
			constexpr auto stackPointer = ArchitectureConstants::StackPointerIndex;
			switch (op) {
				case MoveOp::Store:
					core->write(thread->gpr[inst.getDestination()], thread->gpr[inst.getSource0()]);
					break;
				case MoveOp::Push:
					--thread->gpr[stackPointer];
					core->write(thread->gpr[stackPointer], thread->gpr[inst.getDestination()]);
					break;
				case MoveOp::Pop:
					thread->gpr[inst.getDestination()] = core->read(thread->gpr[stackPointer]);
					++thread->gpr[stackPointer];
					break;
				case MoveOp::Load:
					thread->gpr[inst.getDestination()] = core->read(thread->gpr[inst.getSource0()]);
					break;
				case MoveOp::Move:
					thread->gpr[inst.getDestination()] = thread->gpr[inst.getSource0()];
					break;
				case MoveOp::SetLower: 
					thread->gpr[inst.getDestination()] = iris::encodeBits<word, hword, static_cast<word>(0xFFFF0000), 0>(thread->gpr[inst.getDestination()], inst.getImmediate());
					break;
				case MoveOp::SetUpper: 
					thread->gpr[inst.getDestination()] = iris::encodeBits<word, hword, 0x0000FFFF, 16>(thread->gpr[inst.getDestination()], inst.getImmediate());
					break;
				case MoveOp::Swap: {
									   auto a = thread->gpr[inst.getDestination()];
									   thread->gpr[inst.getDestination()] = thread->gpr[inst.getSource0()];
									   thread->gpr[inst.getSource0()] = a;
									   break;
								   }
				default:
								   throw iris::Problem("Illegal move code!");
			}
		}

	template<CompareOp op>
		void invokeCompare(Core* core, DecodedInstruction&& current) {
			auto &thread = core->thread;
			switch (op) {
				case CompareOp::Eq:
					thread->gpr[current.getDestination()] = iris::eq(thread->gpr[current.getSource0()], thread->gpr[current.getSource1()]); 
					break;
				case CompareOp::Neq:
					thread->gpr[current.getDestination()] = iris::neq(thread->gpr[current.getSource0()],  thread->gpr[current.getSource1()]); 
					break;
				case CompareOp::LessThan:
					thread->gpr[current.getDestination()] = iris::lt(thread->gpr[current.getSource0()], thread->gpr[current.getSource1()]); 
					break;
				case CompareOp::GreaterThan:
					thread->gpr[current.getDestination()] = iris::gt(thread->gpr[current.getSource0()], thread->gpr[current.getSource1()]); 
					break;
				case CompareOp::LessThanOrEqualTo:
					thread->gpr[current.getDestination()] = iris::le(thread->gpr[current.getSource0()], thread->gpr[current.getSource1()]); 
					break;
				case CompareOp::GreaterThanOrEqualTo:
					thread->gpr[current.getDestination()] = iris::ge(thread->gpr[current.getSource0()], thread->gpr[current.getSource1()]); 
					break;
				case CompareOp::EqImm:
					thread->gpr[current.getDestination()] = iris::eq(thread->gpr[current.getSource0()], static_cast<word>(current.getSource1())); 
					break;
				case CompareOp::NeqImm:
					thread->gpr[current.getDestination()] = iris::neq(thread->gpr[current.getSource0()], static_cast<word>(current.getSource1())); 
					break;
				case CompareOp::LessThanImm:
					thread->gpr[current.getDestination()] = iris::lt(thread->gpr[current.getSource0()], static_cast<word>(current.getSource1())); 
					break;
				case CompareOp::GreaterThanImm:
					thread->gpr[current.getDestination()] = iris::gt(thread->gpr[current.getSource0()], static_cast<word>(current.getSource1())); 
					break;
				case CompareOp::LessThanOrEqualToImm:
					thread->gpr[current.getDestination()] = iris::le(thread->gpr[current.getSource0()], static_cast<word>(current.getSource1())); 
					break;
				case CompareOp::GreaterThanOrEqualToImm:
					thread->gpr[current.getDestination()] = iris::ge(thread->gpr[current.getSource0()], static_cast<word>(current.getSource1())); 
					break;
				default:
					throw iris::Problem("Illegal compare instruction!");
			}
		}

	template<ArithmeticOp op, bool checkDenominator, bool immediate>
		void invokeArithmetic(Core* core, DecodedInstruction&& inst) {
			auto &thread = core->thread;
			auto src1 = static_cast<word>(immediate ? inst.getSource1() : thread->gpr[inst.getSource1()]);
			if (checkDenominator) {
				if (src1 == 0) {
					throw iris::Problem("denominator is zero!");
				}
			} 
			auto src0 = thread->gpr[inst.getSource0()];
			auto &result = thread->gpr[inst.getDestination()];
			switch (op) {
				case ArithmeticOp::Add:
				case ArithmeticOp::AddImmediate:
					result = iris::add(src0, src1);
					break;
				case ArithmeticOp::Sub:
				case ArithmeticOp::SubImmediate:
					result = iris::sub(src0, src1);
					break;
				case ArithmeticOp::Mul:
				case ArithmeticOp::MulImmediate:
					result = iris::mul(src0, src1);
					break;
				case ArithmeticOp::Div:
				case ArithmeticOp::DivImmediate:
					result = iris::div(src0, src1);
					break;
				case ArithmeticOp::Rem:
				case ArithmeticOp::RemImmediate:
					result = iris::rem(src0, src1);
					break;
				case ArithmeticOp::ShiftLeft:
				case ArithmeticOp::ShiftLeftImmediate:
					result = iris::shiftLeft(src0, src1);
					break;
				case ArithmeticOp::ShiftRight:
				case ArithmeticOp::ShiftRightImmediate:
					result = iris::shiftRight(src0, src1);
					break;
				case ArithmeticOp::BinaryAnd:
					result = iris::binaryAnd(src0, src1);
					break;
				case ArithmeticOp::BinaryOr:
					result = iris::binaryOr(src0, src1);
					break;
				case ArithmeticOp::BinaryNot:
					result = iris::binaryNot(src0);
					break;
				case ArithmeticOp::BinaryXor:
					result = iris::binaryXor(src0, src1);
					break;
				default:
					throw iris::Problem("Illegal arithmetic operation!");
			}

		}

	template<MiscOp op>
		void invokeMisc(Core* core, DecodedInstruction&& inst) {
			const auto &thread = core->thread;
			switch (op) { 
				case MiscOp::SystemCall:
					switch(static_cast<SystemCalls>(inst.getDestination())) {
						case SystemCalls::Terminate: {
														 core->execute = false;
														 thread->advanceIp = false;
														 break;
													 }
						case SystemCalls::PutC: {
													// read register 0 and register 1
													std::cout << static_cast<char>(thread->gpr[inst.getSource0()]);
													break;
												}
						case SystemCalls::GetC: {
													byte value = 0;
													std::cin >> std::noskipws >> value;
													thread->gpr[inst.getSource0()] = static_cast<word>(value);
													break;
												}
						default: {
									 std::stringstream stream;
									 stream << "Illegal system call " << std::hex << inst.getDestination();
									 throw iris::Problem(stream.str());
								 }
					}
					break;
				default:
					throw iris::Problem("Illegal Misc Op!");
			}
		}


	void Core::dispatch() {
		// read a byte from the current instruction pointer address
		auto decoded = DecodedInstruction(read(thread->gpr[ArchitectureConstants::InstructionPointerIndex]));
		auto printInst = [this](const std::string& msg, DecodedInstruction& decoded) {
			std::cerr << msg << "\n"
				<< "\tdestination register index: r" << std::dec << (int)  decoded.getDestination()
				<< ", value: " << (thread->gpr[decoded.getDestination()]) << "\n"
				<< "\tsource0 register index: r" <<  std::dec << (int)  decoded.getSource0()
				<< ", value: " << (thread->gpr[decoded.getSource0()]) << "\n"
				<< "\tsource 1 register index: r" <<  std::dec << (int)  decoded.getSource1()
				<< ", value: " << (thread->gpr[decoded.getSource1()]) << std::endl;
			std::cerr << "ip: 0x" << std::hex << thread->gpr[ArchitectureConstants::InstructionPointerIndex]
			<< ", instruction raw: 0x" << std::hex << decoded.getRawValue() << std::endl;
			std::cerr << "group: " << std::hex << (int)decoded.getGroup() << std::endl;
			std::cerr << "op: " << std::hex << (int)decoded.getOperation() << std::endl;
		};
		if (debugEnabled()) {
			printInst("before", decoded);
		}
		switch (decoded.getControl()) {
#define X(title, operation, unused) \
			case DecodeCompareOp ## title :: value : \
													 invokeCompare<CompareOp :: title>(this, std::move(decoded));  \
			break;
#define Y(title, operation, unused) X(title, operation, unused)
#include "def/iris32/compare.def"
#undef X
#undef Y
#define X(title, immediate, checkDenominator) \
			case DecodeArithmeticOp ## title :: value : \
														invokeArithmetic<ArithmeticOp :: title, immediate, checkDenominator>(this, std::move(decoded));\
			break;
#include "def/iris32/arithmetic.def"
#undef X
#define X(title, operation, __, ___, ____) \
			case DecodeMoveOp ## title :: value : \
												  invokeMove<MoveOp :: title> (this, std::move(decoded)); \
			break;
#include "def/iris32/move.def"
#undef X
#define X(name, ifthenelse, conditional, iffalse, immediate, link) \
			case DecodeJumpOp ## name :: value : \
												 invokeJump<ifthenelse, conditional, iffalse, immediate, link>(this, std::move(decoded)); \
			break;
#include "def/iris32/jump.def"
#undef X
#define X(title, __) \
			case DecodeMiscOp ## title :: value: \
												 invokeMisc<MiscOp :: title>(this, std::move(decoded)); \
			break;
#include "def/iris32/misc.def"
#undef X
			default:
				throw "Undefined control!";
		}
	}
	void Core::initialize() {
		int threadIndex = 0;
		for (auto &cthread : threads) {
			cthread->gpr[iris32::ArchitectureConstants::ThreadIndex] = threadIndex;
			++threadIndex;
		}
	}
	void Core::shutdown() {

	}
	void Core::run() {
		while (execute) {
			for (auto cthread : threads) {
				if (!execute) {
					return;
				} else {
					thread = cthread;
					execBody();
				}
			}
		}
	}
	void Core::execBody() {
		if (debugEnabled()) {
			std::cerr << "{" << std::endl;
			std::cerr << "current thread " << std::hex << thread.get() << std::endl;
		}
		if (!thread->advanceIp) {
			thread->advanceIp = true;
		}
		dispatch();
		if (thread->advanceIp) {
			++thread->gpr[ArchitectureConstants::InstructionPointerIndex];
			if (thread->gpr[ArchitectureConstants::InstructionPointerIndex] >= memorySize) {
				thread->gpr[ArchitectureConstants::InstructionPointerIndex] = 0;
			}
		}
		if (debugEnabled()) {
			std::cerr << "}" << std::endl;
		}
	}

	void Core::link(std::istream& input) {
		throw iris::Problem("iris32 does not require explicit linking!");
	}

} // end namespace iris32
