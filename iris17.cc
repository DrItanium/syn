#include "iris17.h"
#include <functional>
#include "Problem.h"
#include "iris_base.h"
#include <sstream>
#include <map>

namespace iris17 {
	Core* newCore() noexcept {
		return new iris17::Core(iris17::ArchitectureConstants::AddressMax, 8);
	}
	constexpr word encodeWord(byte a, byte b, byte c, byte d)  noexcept {
		return stdiris::encodeInt32LE(a, b, c, d);
	}
	DecodedInstruction::DecodedInstruction(word inst) noexcept : raw(inst) { }
	DecodedInstruction::~DecodedInstruction() noexcept { }


	void Core::write(word addr, word value) {
		memory[addr] = value;
	}
	word Core::read(word address) {
		return memory[address];
	}

	Core::Core(word msize, byte numThreads) noexcept : memory(msize), threads(numThreads) { }

	Core::~Core() noexcept { }

	auto throwIfNotFound = [](auto result, auto& table, const std::string& msg) {
		if (result == table.end()) {
			throw stdiris::Problem(msg);
		}
	};
	void Core::installprogram(std::istream& stream) {
		// read directly from the assembler's output
		char a[sizeof(word)] = { 0 };
		char b[sizeof(word)] = { 0 };
		while(stream.good()) {
			stream.read(a, sizeof(word));
			if (!stream.good()) {
				if (stream.gcount() > 0) {
					std::cerr << "panic: provided data is not valid iris17 encoded assembler" << std::endl;
					exit(1);
				} else {
					break;
				}
			}
			auto addr = encodeWord(a[0], a[1], a[2], a[3]);
			stream.read(b, sizeof(word));
			if (stream.gcount() != sizeof(word)) {
				std::cerr << "panic: provided data is not valid iris17 encoded assembler" << std::endl;
				exit(1);
			}
			auto data = encodeWord(b[0], b[1], b[2], b[3]);
			if (debugEnabled()) {
				std::cerr << "install 0x" << std::hex << data
					<< " @ 0x" << std::hex << addr << std::endl;
			}
			memory[addr] = data;
		}
	}
	void Core::dump(std::ostream& stream) {
		memory.dump(stream, [](word value, char* buf) { stdiris::decodeInt32LE(value, (byte*)buf); });
	}
	// jump operations
	//template<bool ifthenelse, bool conditional, bool iffalse, bool immediate, bool link>
	void Core::jump(DecodedInstruction&& inst) {
		auto newAddr = static_cast<word>(0);
		auto cond = false;
		thread->advanceIp = false;
		auto ifthenelse = false;
		auto conditional = false;
		auto iffalse = false;
		auto immediate = false;
		auto link = false;
		static std::map<JumpOp, std::tuple<bool, bool, bool, bool, bool>> translationTable = {
			{ JumpOp:: UnconditionalRegister , std::make_tuple( false, false, false, false, false) } ,
			{ JumpOp:: UnconditionalRegisterLink , std::make_tuple( false, false, false, false, true) } ,
			{ JumpOp:: ConditionalTrueRegister , std::make_tuple( false, true, false, false, false) } ,
			{ JumpOp:: ConditionalTrueRegisterLink , std::make_tuple( false, true, false, false, true) } ,
			{ JumpOp:: ConditionalFalseRegister , std::make_tuple( false, true, true, false, false) } ,
			{ JumpOp:: ConditionalFalseRegisterLink , std::make_tuple( false, true, true, false, true) } ,
			{ JumpOp:: IfThenElseNormalPredTrue , std::make_tuple( true, true, false, false, false) } ,
			{ JumpOp:: IfThenElseNormalPredFalse , std::make_tuple( true, true, true, false, false) } ,
			{ JumpOp:: IfThenElseLinkPredTrue , std::make_tuple( true, true, false, false, true) } ,
			{ JumpOp:: IfThenElseLinkPredFalse , std::make_tuple( true, true, true, false, true) } ,
		};
		auto result = translationTable.find(inst.getSubtype<JumpOp>());
		throwIfNotFound(result, translationTable, "undefined jump op!");
		std::tie(ifthenelse, conditional, iffalse, immediate, link) = result->second;
		auto ip = thread->getInstructionPointer();
		if (conditional) {
			auto dest = thread->gpr[inst.getDestination()];
			cond = iffalse ? (dest == 0) : (dest != 0);
			if (ifthenelse) {
				newAddr = thread->gpr[cond ? inst.getSource0() : inst.getSource1()];
			} else {
				if (cond) {
					newAddr = immediate ? inst.getImmediate() : thread->gpr[inst.getSource0()];
				} else {
					newAddr = ip + 1;
				}
			}
		} else {
			newAddr = immediate ? inst.getImmediate() : thread->gpr[inst.getSource0()];
		}
		thread->getInstructionPointer() = newAddr;
		if (link) {
			if (conditional) {
				if (cond) {
					thread->getLinkRegister() = ip + 1;
				}
			} else {
				thread->getLinkRegister() = ip + 1;
			}
		}
	}

	// move operations
	void Core::move(DecodedInstruction&& inst) {
		auto op = inst.getSubtype<MoveOp>();
		auto &dest = thread->gpr[inst.getDestination()];
		if (op == MoveOp::Store) {
			write(dest, thread->gpr[inst.getSource0()]);
		} else if (op == MoveOp::Push) {
			--thread->getStackPointer();
			write(thread->getStackPointer(), dest);
		} else if (op == MoveOp::Pop) {
			dest = read(thread->getStackPointer());
			++thread->getStackPointer();
		} else if (op == MoveOp::Load) {
			dest = read(thread->gpr[inst.getSource0()]);
		} else if (op == MoveOp::Move) {
			dest = thread->gpr[inst.getSource0()];
		} else if (op == MoveOp::SetLower) {
			dest = stdiris::setLowerHalf(dest, inst.getImmediate());
		} else if (op == MoveOp::SetUpper) {
			dest = stdiris::setUpperHalf(dest, inst.getImmediate());
		} else if (op == MoveOp::Swap) {
			thread->gpr.swap(inst.getDestination(), inst.getSource0());
		} else {
			throw stdiris::Problem("Illegal move code!");
		}
	}
	template<typename Operation>
	using ImmediateTuple = std::tuple<Operation, bool>;
	template<typename Unit>
	inline void performBinaryOperation(SharedExecState& thread, Unit& unit, ImmediateTuple<typename Unit::Operation>& tup, DecodedInstruction&& inst) {
		using Op = typename Unit::Operation;
		using Word = typename Unit::WordType;
		Op op;
		bool immediate = false;
		std::tie(op, immediate) = tup;
		thread->gpr[inst.getDestination()] = unit.performOperation(op, thread->gpr[inst.getSource0()], immediate ? static_cast<Word>(inst.getSource1()) : thread->gpr[inst.getSource1()]);
	}
	template<typename T, typename Unit>
	using ImmediateLookupTable = std::map<T, ImmediateTuple<typename Unit::Operation>>;
	void Core::compare(DecodedInstruction&& current) {
		static ImmediateLookupTable<CompareOp, CompareUnit> translationTable = {
			{ CompareOp::LessThan, std::make_tuple(CompareUnit::Operation::LessThan, false) },
			{ CompareOp::LessThanImm, std::make_tuple(CompareUnit::Operation::LessThan, true) },
			{ CompareOp::LessThanOrEqualTo, std::make_tuple(CompareUnit::Operation::LessThanOrEqualTo, false) },
			{ CompareOp::LessThanOrEqualToImm, std::make_tuple(CompareUnit::Operation::LessThanOrEqualTo, true) },
			{ CompareOp::GreaterThan, std::make_tuple(CompareUnit::Operation::GreaterThan, false) },
			{ CompareOp::GreaterThanImm, std::make_tuple(CompareUnit::Operation::GreaterThan, true) },
			{ CompareOp::GreaterThanOrEqualTo, std::make_tuple(CompareUnit::Operation::GreaterThanOrEqualTo, false) },
			{ CompareOp::GreaterThanOrEqualToImm, std::make_tuple(CompareUnit::Operation::GreaterThanOrEqualTo, true) },
			{ CompareOp::Eq, std::make_tuple(CompareUnit::Operation::Eq, false) },
			{ CompareOp::EqImm, std::make_tuple(CompareUnit::Operation::Eq, true) },
			{ CompareOp::Neq, std::make_tuple(CompareUnit::Operation::Neq, false) },
			{ CompareOp::NeqImm, std::make_tuple(CompareUnit::Operation::Neq, true) },
		};
		auto result = translationTable.find(current.getSubtype<CompareOp>());
		throwIfNotFound(result, translationTable, "Illegal compare operation!");
		performBinaryOperation(thread, _compare, result->second, std::move(current));
	}

	void Core::arithmetic(DecodedInstruction&& inst) {
		static ImmediateLookupTable<ArithmeticOp, ALU> translationTable = {
			{ ArithmeticOp::Add, std::make_tuple(ALU::Operation::Add , false) },
			{ ArithmeticOp::Sub, std::make_tuple(ALU::Operation::Subtract , false ) },
			{ ArithmeticOp::Mul, std::make_tuple(ALU::Operation::Multiply , false ) } ,
			{ ArithmeticOp::Div, std::make_tuple(ALU::Operation::Divide , false ) },
			{ ArithmeticOp::Rem, std::make_tuple(ALU::Operation::Remainder , false ) },
			{ ArithmeticOp::ShiftLeft, std::make_tuple(ALU::Operation::ShiftLeft , false ) },
			{ ArithmeticOp::ShiftRight, std::make_tuple(ALU::Operation::ShiftRight , false ) },
			{ ArithmeticOp::BinaryAnd, std::make_tuple(ALU::Operation::BinaryAnd , false ) },
			{ ArithmeticOp::BinaryOr, std::make_tuple(ALU::Operation::BinaryOr , false ) },
			{ ArithmeticOp::BinaryNot, std::make_tuple(ALU::Operation::UnaryNot , false) },
			{ ArithmeticOp::BinaryXor, std::make_tuple(ALU::Operation::BinaryXor , false ) },
			{ ArithmeticOp::AddImmediate, std::make_tuple(ALU::Operation::Add , true  ) },
			{ ArithmeticOp::SubImmediate, std::make_tuple(ALU::Operation::Subtract , true  ) },
			{ ArithmeticOp::MulImmediate, std::make_tuple(ALU::Operation::Multiply , true  ) } ,
			{ ArithmeticOp::DivImmediate, std::make_tuple(ALU::Operation::Divide , true  ) },
			{ ArithmeticOp::RemImmediate, std::make_tuple(ALU::Operation::Remainder , true  ) },
			{ ArithmeticOp::ShiftLeftImmediate, std::make_tuple(ALU::Operation::ShiftLeft , true ) },
			{ ArithmeticOp::ShiftRightImmediate, std::make_tuple(ALU::Operation::ShiftRight , true ) },
		};
		auto result = translationTable.find(inst.getSubtype<ArithmeticOp>());
		throwIfNotFound(result, translationTable, "Illegal arithmetic operation!");
		performBinaryOperation(thread, _alu, result->second, std::move(inst));
	}

	void Core::misc(DecodedInstruction&& inst) {
		auto op = inst.getSubtype<MiscOp>();
		if (op == MiscOp::SystemCall) {
			auto sysop = static_cast<SystemCalls>(inst.getDestination());
			if (sysop == SystemCalls::Terminate) {
				execute = false;
				thread->advanceIp = false;
			} else if (sysop == SystemCalls::PutC) {
				std::cout << static_cast<char>(thread->gpr[inst.getSource0()]);
			} else if (sysop == SystemCalls::GetC) {
				byte value = 0u;
				std::cin >> std::noskipws >> value;
				thread->gpr[inst.getSource0()] = static_cast<word>(value);
			} else {
				std::stringstream stream;
				stream << "Illegal system call " << std::hex << inst.getDestination();
				throw stdiris::Problem(stream.str());
			}
		} else {
			throw stdiris::Problem("Illegal misc op!");
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
		switch (decoded.getGroup()) {
			case InstructionGroup::Compare:
				compare(std::move(decoded));
				break;
			case InstructionGroup::Arithmetic:
				arithmetic(std::move(decoded));
				break;
			case InstructionGroup::Jump:
				jump(std::move(decoded));
				break;
			case InstructionGroup::Misc:
				misc(std::move(decoded));
				break;
			case InstructionGroup::Move:
				move(std::move(decoded));
				break;
			default:
				throw stdiris::Problem("Undefined control!");
		}
	}
	void Core::initialize() {
		int threadIndex = 0;
		for (auto &cthread : threads) {
			cthread->getThreadIndexRegister() = threadIndex;
			++threadIndex;
		}
	}
	void Core::shutdown() {

	}
	bool Core::cycle() {
		// this will execute an instruction on each thread before terminating
		for (auto cthread : threads) {
			if (!execute) {
				break;
			} else {
				thread = cthread;
				execBody();
			}
		}
		return execute;
	}
	void Core::execBody() {
		if (debugEnabled()) {
			std::cerr << "{" << std::endl;
			std::cerr << "current thread " << std::hex << thread.get() << std::endl;
		}
		thread->advanceIp = true;
		dispatch();
		if (thread->advanceIp) {
			++thread->getInstructionPointer();
		}
		if (thread->getInstructionPointer() >= memory.getSize()) {
			thread->getInstructionPointer() = 0;
		}
		if (debugEnabled()) {
			std::cerr << "}" << std::endl;
		}
	}

	void Core::link(std::istream& input) {
		throw stdiris::Problem("iris17 does not require explicit linking!");
	}

} // end namespace iris17
