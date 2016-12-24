#include "iris16.h"
#include <functional>
#include <sstream>
#include <vector>

namespace iris16 {

	Core::Core() noexcept : _io(0, 0xFFFF) { }

	Core::~Core() { 
	}

	void Core::installprogram(std::istream& stream) {
		auto encodeWord = [](char* buf) { return iris16::encodeWord(buf[0], buf[1]); };
		auto encodeDword = [](char* buf) { return iris16::encodeDword(buf[0], buf[1], buf[2], buf[3]); };
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
	bool Core::cycle() {
		advanceIp = true;
		dispatch();
		if (advanceIp) {
			++getInstructionPointer();
		}
		return execute;
	}
	template<typename T>
	using UnitDescription = std::tuple<typename T::Operation, bool>;
	template<typename T>
	UnitDescription<T> makeDesc(typename T::Operation operation, bool immediate) noexcept {
		return std::make_tuple(operation, immediate);
	}
	void Core::dispatch() {
		current = instruction[getInstructionPointer()];
		auto group = static_cast<InstructionGroup>(getGroup());
		auto makeProblem = [this](const std::string& message, auto operation) {
			std::stringstream stream;
			stream << message << " 0x" << std::hex << operation;
			execute = false;
			advanceIp = false;
			throw iris::Problem(stream.str());
		};
		auto makeIllegalOperationMessage = [this, makeProblem](const std::string& type) {
			makeProblem("Illegal " + type, getOperation());
		};
		if (group == InstructionGroup::Arithmetic) {
			static std::map<ArithmeticOp, UnitDescription<ALU>> table = {
				{ ArithmeticOp::Add, makeDesc<ALU>(ALU::Operation::Add , false) },
				{ ArithmeticOp::Sub, makeDesc<ALU>(ALU::Operation::Subtract , false ) },
				{ ArithmeticOp::Mul, makeDesc<ALU>(ALU::Operation::Multiply , false ) } ,
				{ ArithmeticOp::Div, makeDesc<ALU>(ALU::Operation::Divide , false ) },
				{ ArithmeticOp::Rem, makeDesc<ALU>(ALU::Operation::Remainder , false ) },
				{ ArithmeticOp::ShiftLeft, makeDesc<ALU>(ALU::Operation::ShiftLeft , false ) },
				{ ArithmeticOp::ShiftRight, makeDesc<ALU>(ALU::Operation::ShiftRight , false ) },
				{ ArithmeticOp::BinaryAnd, makeDesc<ALU>(ALU::Operation::BinaryAnd , false ) },
				{ ArithmeticOp::BinaryOr, makeDesc<ALU>(ALU::Operation::BinaryOr , false ) },
				{ ArithmeticOp::BinaryNot, makeDesc<ALU>(ALU::Operation::UnaryNot , false) },
				{ ArithmeticOp::BinaryXor, makeDesc<ALU>(ALU::Operation::BinaryXor , false ) },
				{ ArithmeticOp::AddImmediate, makeDesc<ALU>(ALU::Operation::Add , true  ) },
				{ ArithmeticOp::SubImmediate, makeDesc<ALU>(ALU::Operation::Subtract , true  ) },
				{ ArithmeticOp::MulImmediate, makeDesc<ALU>(ALU::Operation::Multiply , true  ) } ,
				{ ArithmeticOp::DivImmediate, makeDesc<ALU>(ALU::Operation::Divide , true  ) },
				{ ArithmeticOp::RemImmediate, makeDesc<ALU>(ALU::Operation::Remainder , true  ) },
				{ ArithmeticOp::ShiftLeftImmediate, makeDesc<ALU>(ALU::Operation::ShiftLeft , true ) },
				{ ArithmeticOp::ShiftRightImmediate, makeDesc<ALU>(ALU::Operation::ShiftRight , true ) },
			};
			auto result = table.find(static_cast<ArithmeticOp>(getOperation()));
			if (result == table.end()) {
				makeIllegalOperationMessage("arithmetic operation");
			} else {
				performOperation(_alu, result->second);
			}
		} else if (group == InstructionGroup::Compare) {
			static std::map<CompareOp, UnitDescription<CompareUnit>> translationTable = {
				{ CompareOp::LessThan, makeDesc<CompareUnit>(CompareUnit::Operation::LessThan, false) },
				{ CompareOp::LessThanImm, makeDesc<CompareUnit>(CompareUnit::Operation::LessThan, true) },
				{ CompareOp::LessThanOrEqualTo, makeDesc<CompareUnit>(CompareUnit::Operation::LessThanOrEqualTo, false) },
				{ CompareOp::LessThanOrEqualToImm, makeDesc<CompareUnit>(CompareUnit::Operation::LessThanOrEqualTo, true) },
				{ CompareOp::GreaterThan, makeDesc<CompareUnit>(CompareUnit::Operation::GreaterThan, false) },
				{ CompareOp::GreaterThanImm, makeDesc<CompareUnit>(CompareUnit::Operation::GreaterThan, true) },
				{ CompareOp::GreaterThanOrEqualTo, makeDesc<CompareUnit>(CompareUnit::Operation::GreaterThanOrEqualTo, false) },
				{ CompareOp::GreaterThanOrEqualToImm, makeDesc<CompareUnit>(CompareUnit::Operation::GreaterThanOrEqualTo, true) },
				{ CompareOp::Eq, makeDesc<CompareUnit>(CompareUnit::Operation::Eq, false) },
				{ CompareOp::EqImm, makeDesc<CompareUnit>(CompareUnit::Operation::Eq, true) },
				{ CompareOp::Neq, makeDesc<CompareUnit>(CompareUnit::Operation::Neq, false) },
				{ CompareOp::NeqImm, makeDesc<CompareUnit>(CompareUnit::Operation::Neq, true) },
			};
			auto result = translationTable.find(static_cast<CompareOp>(getOperation()));
			if (result == translationTable.end()) {
				makeIllegalOperationMessage("compare code");
			} else {
				performOperation(_compare, result->second);
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
				makeIllegalOperationMessage("jump code");
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
			raw_instruction codeStorage = 0u;
			switch(op) {
				case MoveOp::Move:
					gpr.copy(getDestination(), getSource0());
					break;
				case MoveOp::Set:
					gpr.set(getDestination(), getImmediate());
					break;
				case MoveOp::Swap:
					gpr.swap(getDestination(), getSource0());
					break;
				case MoveOp::Load:
					gpr.set(getDestination(), data[source0Register()]);
					break;
				case MoveOp::LoadImmediate:
					gpr.set(getDestination(), data[getImmediate()]);
					break;
				case MoveOp::Store:
					data.set(destinationRegister(), source0Register());
					break;
				case MoveOp::Memset:
					data.set(destinationRegister(), getImmediate());
					break;
				case MoveOp::Push:
					stack[++getStackPointer()] = destinationRegister();
					break;
				case MoveOp::PushImmediate:
					stack[++getStackPointer()] = getImmediate();
					break;
				case MoveOp::Pop:
					destinationRegister() = stack[getStackPointer()];
					--getStackPointer();
					break;
				case MoveOp::LoadCode:
					codeStorage = instruction[destinationRegister()];
					source0Register() = iris::getLowerHalf(codeStorage);
					source1Register() = iris::getUpperHalf(codeStorage);
					break;
				case MoveOp::StoreCode:
					instruction[destinationRegister()] = encodeDword(source0Register(), source1Register());
					break;
				case MoveOp::IOWrite:
					_io.write(destinationRegister(), source0Register());
					break;
				case MoveOp::IORead:
					destinationRegister() = _io.read(source0Register());
					break;
				default:
					makeIllegalOperationMessage("move code");
					break;
			}
		} else {
			makeProblem("Illegal instruction group", getGroup());
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
				writeInstructionMemory(address, result);
			} else if (target == Segment::Data) {
				auto result = iris16::encodeWord(buf[4], buf[5]);
				if (debugEnabled()) {
					std::cerr << " data result: 0x" << std::hex << result << std::endl;
				}
				writeDataMemory(address, result);
			} else {
				std::stringstream str;
				str << "error: line " << lineNumber << ", unknown segment " << static_cast<int>(target) << "/" << static_cast<int>(buf[1]) << std::endl;
				str << "current address: " << std::hex << address << std::endl;
				throw iris::Problem(str.str());
			}
		}
	}


	void Core::initialize() {
		execute = true;
		advanceIp = true;
		gpr.initialize();
		data.initialize();
		instruction.initialize();
		stack.initialize();
		_io.initialize();
		auto readNothing = iris::readNothing<typename LambdaIODevice::DataType, typename LambdaIODevice::AddressType>;
		// terminate
		_io.install(std::make_shared<LambdaIODevice>(0, 1, readNothing, 
					[this](word address, word value) { 
						execute = false; 
						advanceIp = false; 
					}));
		// getc and putc
		_io.install(std::make_shared<iris::StandardInputOutputDevice<word>>(1));
	}

	void Core::shutdown() {
		gpr.shutdown();
		data.shutdown();
		instruction.shutdown();
		stack.shutdown();
		_io.shutdown();
	}

	void Core::installIODevice(std::shared_ptr<IODevice> dev) {
		_io.install(dev);
	}

	Core* newCore() noexcept {
		return new iris16::Core();
	}
}
