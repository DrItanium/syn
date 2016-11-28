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
		auto encodeWord = [](char* buf) { return iris20::encodeWord(buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7]); };
		gpr.install(stream, encodeWord);
		memory.install(stream, encodeWord);
	}

	void Core::dump(std::ostream& stream) {

		auto decodeWord = [](word value, char* buf) { iris::decodeInt64LE(value, (byte*)buf); };
		gpr.dump(stream, decodeWord);
		memory.dump(stream, decodeWord);
	}
	void Core::run() {
		while(execute) {
			if (!advanceIp) {
				advanceIp = true;
			}
			dispatch();
			if (advanceIp) {
				++getInstructionPointer();
			}
		}
	}
	void Core::dispatch() {
		current = memory[getInstructionPointer()];
		executeAtom(getFirstAtom(current));
		executeAtom(getSecondAtom(current));
	}
	void Core::executeAtom(InstructionAtom atom) {
		auto genericOperation = getOperation(atom);
		auto group = static_cast<InstructionGroup>(getGroup(atom));
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
			auto result = table.find(getSubtype<ArithmeticOp>(atom));
			if (result == table.end()) {
				std::stringstream stream;
				stream << "Illegal arithmetic operation " << getOperation(atom);
				execute = false;
				throw iris::Problem(stream.str());
			} else {
				performOperation(_alu, result->second, atom);
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
			auto result = translationTable.find(getSubtype<CompareOp>(atom));
			if (result == translationTable.end()) {
				std::stringstream stream;
				stream << "Illegal compare code " << genericOperation;
				execute = false;
				advanceIp = false;
				throw iris::Problem(stream.str());
			} else {
				performOperation(_compare, result->second, atom);
			}
		} else if (group == InstructionGroup::Misc) {
			auto op = getSubtype<MiscOp>(atom);
			if (op == MiscOp::SystemCall) {
				auto target = static_cast<SystemCalls>(getDestinationRawValue(atom));
				if (target == SystemCalls::Terminate) {
					execute = false;
					advanceIp = false;
				} else if (target == SystemCalls::PutC) {
					// read register 0 and register 1
					std::cout.put(static_cast<char>(operandGet(getSource0RawValue(atom))));
				} else if (target == SystemCalls::GetC) {
					auto value = static_cast<byte>(0);
					std::cin >> std::noskipws >> value;
					operandSet(getSource0RawValue(atom), static_cast<word>(value));
				} else {
					std::stringstream stream;
					stream << "Illegal system call " << std::hex << getDestinationRawValue(atom);
					execute = false;
					advanceIp = false;
					throw iris::Problem(stream.str());
				}
			} else {
				std::stringstream ss;
				ss << "Illegal misc code " << genericOperation;
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
			auto result = translationTable.find(getSubtype<JumpOp>(atom));
			if (result == translationTable.end()) {
				std::stringstream ss;
				ss << "Illegal jump code " << std::hex << static_cast<int>(genericOperation);
				execute =  false;
				throw iris::Problem(ss.str());
			}
			std::tie(ifthenelse, conditional, iffalse, immediate, link) = result->second;
			auto newAddr = static_cast<word>(0);
			auto cond = true;
			advanceIp = false;
			auto ip = getInstructionPointer();
			auto dest = operandGet(getDestinationRawValue(atom));
			auto src0Ind = getSource0RawValue(atom);
			auto src1Ind = getSource0RawValue(atom);
			if (conditional) {
				cond = (iffalse ? (dest == 0) : (dest != 0));
				if (ifthenelse) {
					newAddr = operandGet(cond ? src0Ind : src1Ind);
				} else {
					newAddr = cond ? (immediate ? getImmediate(atom) : operandGet(src1Ind)) : ip + 1;
				}
			} else {
				newAddr = immediate ? getImmediate(atom) : dest;
			}
			getInstructionPointer() = newAddr;
			if (link && cond) {
				getLinkRegister() = ip + 1;
			}
		} else if (group == InstructionGroup::Move) {
			auto op = getSubtype<MoveOp>(atom);
			auto dest = getDestinationRawValue(atom);
			auto src = getSource0RawValue(atom);
			if (op == MoveOp::Move) {
				operandSet(dest, operandGet(src));
			} else if (op == MoveOp::Set16) {
				operandSet(dest, getImmediate(atom));
			} else if (op == MoveOp::Swap) {
				auto tmp = operandGet(dest);
				operandSet(dest, operandGet(src));
				operandSet(src, tmp);
			} else {
				std::stringstream ss;
				ss << "Illegal move code " << genericOperation;
				execute = false;
				advanceIp = false;
				throw iris::Problem(ss.str());
			}
		} else {
			std::stringstream stream;
			stream << "Illegal instruction group " << getGroup(atom);
			execute = false;
			throw iris::Problem(stream.str());
		}
	}

	void Core::link(std::istream& input) {
		constexpr auto bufSize = sizeof(word) * 2;
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
		memory.zero();
	}
}
