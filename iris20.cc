#include "iris20.h"
#include <functional>
#include <sstream>
#include <vector>

namespace iris20 {
	Core* newCore() noexcept {
		return new iris20::Core();
	}


	Core::~Core() { 
	}

	void Core::installprogram(std::istream& stream) {
		auto encodeWord = [](char* buf) { return iris20::encodeWord(buf[0], buf[1]); };
		auto encodeDword = [](char* buf) { return iris20::encodeDword(buf[0], buf[1], buf[2], buf[3]); };
		gpr.install(stream, encodeWord);
		data.install(stream, encodeWord);
		instruction.install(stream, encodeDword);
		stack.install(stream, encodeWord);
	}

	void Core::dump(std::ostream& stream) {
		auto decodeWord = [](word value, char* buf) { iris::decodeUint16LE(value, (byte*)buf); };
		auto decodeDword = [](dword value, char* buf) { iris::decodeUint32LE(value, (byte*)buf); };
		gpr.dump(stream, decodeWord);
		data.dump(stream, decodeWord);
		instruction.dump(stream, decodeDword);
		stack.dump(stream, decodeWord);
	}
	void Core::run() {
		while(execute) {
			if (!advanceIp) {
				advanceIp = true;
			}
			current = instruction[getInstructionPointer()];
			dispatch();
			if (advanceIp) {
				++getInstructionPointer();
			}
		}
	}
	void Core::dispatch() {
		auto group = static_cast<InstructionGroup>(getGroup());
		if (group == InstructionGroup::Arithmetic) {
			static std::map<ArithmeticOp, std::tuple<ALU::Operation, bool>> table = {
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
			auto result = table.find(static_cast<ArithmeticOp>(getOperation()));
			if (result == table.end()) {
				std::stringstream stream;
				stream << "Illegal arithmetic operation " << getOperation();
				execute = false;
				throw iris::Problem(stream.str());
			} else {
				performOperation(_alu, result->second);
			}
		} else if (group == InstructionGroup::Compare) {
			static std::map<CompareOp, std::tuple<CompareUnit::Operation, bool>> translationTable = {
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
			auto result = translationTable.find(static_cast<CompareOp>(getOperation()));
			if (result == translationTable.end()) {
				std::stringstream stream;
				stream << "Illegal compare code " << getOperation();
				execute = false;
				advanceIp = false;
				throw iris::Problem(stream.str());
			} else {
				performOperation(_compare, result->second);
			}
		} else if (group == InstructionGroup::Misc) {
			auto op = static_cast<MiscOp>(getOperation());
			if (op == MiscOp::SystemCall) {
				auto target = static_cast<SystemCalls>(getDestination());
				if (target == SystemCalls::Terminate) {
					execute = false;
					advanceIp = false;
				} else if (target == SystemCalls::PutC) {
					// read register 0 and register 1
					std::cout.put(static_cast<char>(source0Register()));
				} else if (target == SystemCalls::GetC) {
					auto value = static_cast<byte>(0);
					std::cin >> std::noskipws >> value;
					source0Register() = static_cast<word>(value);
				} else if (target == SystemCalls::InitializeXMem) {
					// just load the given storage size into r0 and r1
					auto xSize = extendedData->getSize();
					source0Register() = iris::getLowerHalf(xSize);
					source1Register() = iris::getUpperHalf(xSize);
				} else {
					std::stringstream stream;
					stream << "Illegal system call " << std::hex << getDestination();
					execute = false;
					advanceIp = false;
					throw iris::Problem(stream.str());
				}
			} else {
				std::stringstream ss;
				ss << "Illegal misc code " << getOperation();
				execute = false;
				advanceIp = false;
				throw iris::Problem(ss.str());
			}
		} else if (group == InstructionGroup::Jump) {
			// ifthenelse?, conditional?, iffalse?, immediate?, link?
			static std::map<JumpOp, std::tuple<bool, bool, bool, bool, bool>> translationTable = {
				{ JumpOp:: UnconditionalImmediate , std::make_tuple( false, false, false, true, false) } ,
				{ JumpOp:: UnconditionalImmediateLink , std::make_tuple( false, false, false, true, true) } ,
				{ JumpOp:: UnconditionalRegister , std::make_tuple( false, false, false, false, false) } ,
				{ JumpOp:: UnconditionalRegisterLink , std::make_tuple( false, false, false, false, true) } ,
				{ JumpOp:: ConditionalTrueImmediate , std::make_tuple( false, true, false, true, false) } ,
				{ JumpOp:: ConditionalTrueImmediateLink , std::make_tuple( false, true, false, true, true) } ,
				{ JumpOp:: ConditionalTrueRegister , std::make_tuple( false, true, false, false, false) } ,
				{ JumpOp:: ConditionalTrueRegisterLink , std::make_tuple( false, true, false, false, true) } ,
				{ JumpOp:: ConditionalFalseImmediate , std::make_tuple( false, true, true, true, false) } ,
				{ JumpOp:: ConditionalFalseImmediateLink , std::make_tuple( false, true, true, true, true) } ,
				{ JumpOp:: ConditionalFalseRegister , std::make_tuple( false, true, true, false, false) } ,
				{ JumpOp:: ConditionalFalseRegisterLink , std::make_tuple( false, true, true, false, true) } ,
				{ JumpOp:: IfThenElseNormalPredTrue , std::make_tuple( true, true, false, false, false) } ,
				{ JumpOp:: IfThenElseNormalPredFalse , std::make_tuple( true, true, true, false, false) } ,
				{ JumpOp:: IfThenElseLinkPredTrue , std::make_tuple( true, true, false, false, true) } ,
				{ JumpOp:: IfThenElseLinkPredFalse , std::make_tuple( true, true, true, false, true) } ,
			};
			auto ifthenelse = false, conditional = false, iffalse = false, immediate = false,  link = false;
			auto result = translationTable.find(static_cast<JumpOp>(getOperation()));
			if (result == translationTable.end()) {
				std::stringstream ss;
				ss << "Illegal jump code " << std::hex << static_cast<int>(getOperation());
				execute =  false;
				throw iris::Problem(ss.str());
			}
			std::tie(ifthenelse, conditional, iffalse, immediate, link) = result->second;
			auto newAddr = static_cast<word>(0);
			auto cond = true;
			advanceIp = false;
			auto ip = getInstructionPointer();
			if (conditional) {
				auto dest = destinationRegister();
				cond = (iffalse ? (dest == 0) : (dest != 0));
				if (ifthenelse) {
					newAddr = gpr[cond ? getSource0() : getSource1()];
				} else {
					newAddr = cond ? (immediate ? getImmediate() : source0Register()) : ip + 1;
				}
			} else {
				newAddr = immediate ? getImmediate() : destinationRegister();
			}
			getInstructionPointer() = newAddr;
			if (link && cond) {
				getLinkRegister() = ip + 1;
			}
		} else if (group == InstructionGroup::Move) {
			auto op = static_cast<MoveOp>(getOperation());
			if (op == MoveOp::Move) {
				gpr.copy(getDestination(), getSource0());
			} else if (op == MoveOp::Set) {
				gpr.set(getDestination(), getImmediate());
			} else if (op == MoveOp::Swap) {
				gpr.swap(getDestination(), getSource0());
			} else if (op == MoveOp::Load) {
				gpr.set(getDestination(), data[source0Register()]);
			} else if (op == MoveOp::LoadImmediate) {
				gpr.set(getDestination(), data[getImmediate()]);
			} else if (op == MoveOp::Store) {
				data.set(destinationRegister(), source0Register());
			} else if (op == MoveOp::Memset) {
				data.set(destinationRegister(), getImmediate());
			} else if (op == MoveOp::Push) {
				stack[++getStackPointer()] = destinationRegister();
			} else if (op == MoveOp::PushImmediate) {
				stack[++getStackPointer()] = getImmediate();
			} else if (op == MoveOp::Pop) {
				destinationRegister() = stack[getStackPointer()];
				--getStackPointer();
			} else if (op == MoveOp::LoadCode) {
				auto result = instruction[destinationRegister()];
				source0Register() = iris::getLowerHalf(result);
				source1Register() = iris::getUpperHalf(result);
			} else if (op == MoveOp::StoreCode) {
				genericSet(
				instruction[destinationRegister()] = encodeDword(source0Register(), source1Register());
			} else {
				std::stringstream ss;
				ss << "Illegal move code " << getOperation();
				execute = false;
				advanceIp = false;
				throw iris::Problem(ss.str());
			}
		} else {
			std::stringstream stream;
			stream << "Illegal instruction group " << getGroup();
			execute = false;
			throw iris::Problem(stream.str());
		}
	}

	void Core::link(std::istream& input) {
		constexpr bufSize = sizeof(word) * 2;
		char buf[bufSize] = { 0 };
		for(auto lineNumber = static_cast<int>(0); input.good(); ++lineNumber) {
			input.read(buf, bufSize);
			if (input.gcount() < bufSize && input.gcount() > 0) {
				throw iris::Problem("unaligned object file found!");
			} else if (input.gcount() == 0) {
				if (input.eof()) {
					break;
				} else {
					throw iris::Problem("Something bad happened while reading input file!");
				}
			}
			// first 8 bytes are an address, second 8 are a value
			auto address = iris20::encodeWord(buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7]);
			auto value = iris20::encodeWord(buf[8], buf[9], buf[10], buf[11], buf[12], buf[13], buf[14], buf[15]);
			if (debugEnabled()) {
				std::cerr << "addr: 0x " << std::hex << address << ": value: 0x" << std::hex << value << std::endl;
			}
			memory[address] = value;
		}
	}

	Core::Core() noexcept { }
	void Core::initialize() {
		_memory.zero();
	}
}
