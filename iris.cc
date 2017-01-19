#include "iris.h"
#include <functional>
#include <sstream>
#include <vector>

namespace iris {

	Core::Core() noexcept : _io(0, 0xFFFF) { }

	Core::~Core() {
	}

	void Core::installprogram(std::istream& stream) {
		auto encodeWord = [](char* buf) { return iris::encodeWord(buf[0], buf[1]); };
		auto encodeDword = [](char* buf) { return iris::encodeDword(buf[0], buf[1], buf[2], buf[3]); };
		gpr.install(stream, encodeWord);
		data.install(stream, encodeWord);
		instruction.install(stream, encodeDword);
		stack.install(stream, encodeWord);
	}

	void Core::dump(std::ostream& stream) {
		auto decodeWord = [](word value, char* buf) { syn::decodeUint16LE(value, (byte*)buf); };
		auto decodeDword = [](dword value, char* buf) { syn::decodeUint32LE(value, (byte*)buf); };
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
    void Core::setDoubleRegister(byte index, dword value) {
        // this is actually an unstable check
        gpr[index] = syn::getLowerHalf(value);
        gpr[index + 1] = syn::getUpperHalf(value);
    }
    dword Core::getDoubleRegister(byte index) {
        return syn::setUpperHalf(syn::setLowerHalf<dword>(0, gpr[index]), gpr[index + 1]);
    }
	void Core::dispatch() {
		current = instruction[getInstructionPointer()];
		auto group = static_cast<InstructionGroup>(getGroup());
		auto makeProblem = [this](const std::string& message, auto operation) {
			std::stringstream stream;
			stream << message << " 0x" << std::hex << operation;
			execute = false;
			advanceIp = false;
			throw syn::Problem(stream.str());
		};
		auto makeIllegalOperationMessage = [this, makeProblem](const std::string& type) {
			makeProblem("Illegal " + type, getOperation());
		};
		auto dispatch32 = [this, makeIllegalOperationMessage]() {
				auto group = static_cast<DoubleInstructionGroup>(syn::encodeBits<byte, byte, 0x02, 1>(decodeDoubleExtraBit0(current), decodeDoubleExtraBit1(current)));
                auto arithmeticOperation = [this, makeIllegalOperationMessage]() {
                    static std::map<DoubleArithmeticOp, UnitDescription<DWordALU>> table = {
                        { DoubleArithmeticOp::Add, makeDesc<DWordALU>(DWordALU::Operation::Add , false) },
                        { DoubleArithmeticOp::Sub, makeDesc<DWordALU>(DWordALU::Operation::Subtract , false ) },
                        { DoubleArithmeticOp::Mul, makeDesc<DWordALU>(DWordALU::Operation::Multiply , false ) } ,
                        { DoubleArithmeticOp::Div, makeDesc<DWordALU>(DWordALU::Operation::Divide , false ) },
                        { DoubleArithmeticOp::Rem, makeDesc<DWordALU>(DWordALU::Operation::Remainder , false ) },
                        { DoubleArithmeticOp::ShiftLeft, makeDesc<DWordALU>(DWordALU::Operation::ShiftLeft , false ) },
                        { DoubleArithmeticOp::ShiftRight, makeDesc<DWordALU>(DWordALU::Operation::ShiftRight , false ) },
                        { DoubleArithmeticOp::BinaryAnd, makeDesc<DWordALU>(DWordALU::Operation::BinaryAnd , false ) },
                        { DoubleArithmeticOp::BinaryOr, makeDesc<DWordALU>(DWordALU::Operation::BinaryOr , false ) },
                        { DoubleArithmeticOp::BinaryNot, makeDesc<DWordALU>(DWordALU::Operation::UnaryNot , false) },
                        { DoubleArithmeticOp::BinaryXor, makeDesc<DWordALU>(DWordALU::Operation::BinaryXor , false ) },
                        { DoubleArithmeticOp::AddImmediate, makeDesc<DWordALU>(DWordALU::Operation::Add , true  ) },
                        { DoubleArithmeticOp::SubImmediate, makeDesc<DWordALU>(DWordALU::Operation::Subtract , true  ) },
                        { DoubleArithmeticOp::MulImmediate, makeDesc<DWordALU>(DWordALU::Operation::Multiply , true  ) } ,
                        { DoubleArithmeticOp::DivImmediate, makeDesc<DWordALU>(DWordALU::Operation::Divide , true  ) },
                        { DoubleArithmeticOp::RemImmediate, makeDesc<DWordALU>(DWordALU::Operation::Remainder , true  ) },
                        { DoubleArithmeticOp::ShiftLeftImmediate, makeDesc<DWordALU>(DWordALU::Operation::ShiftLeft , true ) },
                        { DoubleArithmeticOp::ShiftRightImmediate, makeDesc<DWordALU>(DWordALU::Operation::ShiftRight , true ) },
                    };
                    auto op = static_cast<DoubleArithmeticOp>(getOperation());
                    auto result = table.find(op);
                    auto destination = 0u;
                    auto ds0 = getDoubleRegister(getDoubleSource0());
                    auto ds1 = getDoubleRegister(getDoubleSource1());
                    if (result == table.end()) {
                        switch(op) {
                            case DoubleArithmeticOp::Min:
                                destination = ds0 < ds1 ? ds0 : ds1;
                                break;
                            case DoubleArithmeticOp::Max:
                                destination = ds0 > ds1 ? ds0 : ds1;
                                break;
                            default:
                                makeIllegalOperationMessage("arithmetic operation");
                        }
                    } else {
                        bool immediate;
                        DWordALU::Operation op;
                        std::tie(op, immediate) = result->second;
                        destination = _alu2.performOperation(op, ds0, immediate ? static_cast<dword>(getHalfImmediate()) : ds1);
                    }
                    setDoubleRegister(getDoubleDestination(), destination);
                };
                auto compare = [this, makeIllegalOperationMessage]() {
                    static std::map<DoubleCompareOp, UnitDescription<DWordCompareUnit>> translationTable = {
                        { DoubleCompareOp::LessThan, makeDesc<DWordCompareUnit>(DWordCompareUnit::Operation::LessThan, false) },
                        { DoubleCompareOp::LessThanImmediate, makeDesc<DWordCompareUnit>(DWordCompareUnit::Operation::LessThan, true) },
                        { DoubleCompareOp::LessThanOrEqualTo, makeDesc<DWordCompareUnit>(DWordCompareUnit::Operation::LessThanOrEqualTo, false) },
                        { DoubleCompareOp::LessThanOrEqualToImmediate, makeDesc<DWordCompareUnit>(DWordCompareUnit::Operation::LessThanOrEqualTo, true) },
                        { DoubleCompareOp::GreaterThan, makeDesc<DWordCompareUnit>(DWordCompareUnit::Operation::GreaterThan, false) },
                        { DoubleCompareOp::GreaterThanImmediate, makeDesc<DWordCompareUnit>(DWordCompareUnit::Operation::GreaterThan, true) },
                        { DoubleCompareOp::GreaterThanOrEqualTo, makeDesc<DWordCompareUnit>(DWordCompareUnit::Operation::GreaterThanOrEqualTo, false) },
                        { DoubleCompareOp::GreaterThanOrEqualToImmediate, makeDesc<DWordCompareUnit>(DWordCompareUnit::Operation::GreaterThanOrEqualTo, true) },
                        { DoubleCompareOp::Eq, makeDesc<DWordCompareUnit>(DWordCompareUnit::Operation::Eq, false) },
                        { DoubleCompareOp::EqImmediate, makeDesc<DWordCompareUnit>(DWordCompareUnit::Operation::Eq, true) },
                        { DoubleCompareOp::Neq, makeDesc<DWordCompareUnit>(DWordCompareUnit::Operation::Neq, false) },
                        { DoubleCompareOp::NeqImmediate, makeDesc<DWordCompareUnit>(DWordCompareUnit::Operation::Neq, true) },
                    };
                    auto op = static_cast<DoubleCompareOp>(getOperation());
                    auto result = translationTable.find(op);
                    if (result == translationTable.end()) {
                        makeIllegalOperationMessage("illegal wide compare operation!");
                    } else {
                        auto ds0 = getDoubleRegister(getDoubleSource0());
                        auto ds1 = getDoubleRegister(getDoubleSource1());
                        typename decltype(_compare2)::Operation op;
                        bool immediate = false;
                        std::tie(op, immediate) = result->second;
                        auto result = _compare2.performOperation(op, ds0, immediate ? static_cast<dword>(getHalfImmediate()) : ds1) != 0;
                        predicateResult() = result;
                        if (getPredicateResult() != getPredicateInverse()) {
                            predicateInverseResult() = !result;
                        }
                    }
                };
                auto moveOperation = [this, makeIllegalOperationMessage]() {
                    auto swap = [this]() {
                        auto dest = getDoubleRegister(getDoubleDestination());
                        setDoubleRegister(getDoubleDestination(), getDoubleRegister(getDoubleSource0()));
                        setDoubleRegister(getDoubleSource0(), dest);
                    };
                    switch(static_cast<DoubleMoveOp>(getOperation())) {
                        case DoubleMoveOp::Move:
                            setDoubleRegister(getDoubleDestination(), getDoubleRegister(getDoubleSource0()));
                            break;
                        case DoubleMoveOp::Swap:
                            swap();
                            break;
                        default:
                            makeIllegalOperationMessage("illegal wide move operation!");
                    }
                };
                switch(group) {
                    case DoubleInstructionGroup::Arithmetic:
                        arithmeticOperation();
                        break;
                    case DoubleInstructionGroup::Compare:
                        compare();
                        break;
                    case DoubleInstructionGroup::Move:
                        moveOperation();
                        break;
                    default:
                        makeIllegalOperationMessage("Illegal or unimplemented operation!");
                }
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
            auto op = static_cast<ArithmeticOp>(getOperation());
			auto result = table.find(op);
			if (result == table.end()) {
                switch(op) {
                    case ArithmeticOp::Min:
                        destinationRegister() = source0Register() < source1Register() ? source0Register() : source1Register();
                        break;
                    case ArithmeticOp::Max:
                        destinationRegister() = source0Register() > source1Register() ? source0Register() : source1Register();
                        break;
                    default:
				        makeIllegalOperationMessage("arithmetic operation");
                }
			} else {
				performOperation(_alu, result->second);
			}
		} else if (group == InstructionGroup::Compare) {
			static std::map<CompareOp, UnitDescription<CompareUnit>> translationTable = {
				{ CompareOp::LessThan, makeDesc<CompareUnit>(CompareUnit::Operation::LessThan, false) },
				{ CompareOp::LessThanImmediate, makeDesc<CompareUnit>(CompareUnit::Operation::LessThan, true) },
				{ CompareOp::LessThanOrEqualTo, makeDesc<CompareUnit>(CompareUnit::Operation::LessThanOrEqualTo, false) },
				{ CompareOp::LessThanOrEqualToImmediate, makeDesc<CompareUnit>(CompareUnit::Operation::LessThanOrEqualTo, true) },
				{ CompareOp::GreaterThan, makeDesc<CompareUnit>(CompareUnit::Operation::GreaterThan, false) },
				{ CompareOp::GreaterThanImmediate, makeDesc<CompareUnit>(CompareUnit::Operation::GreaterThan, true) },
				{ CompareOp::GreaterThanOrEqualTo, makeDesc<CompareUnit>(CompareUnit::Operation::GreaterThanOrEqualTo, false) },
				{ CompareOp::GreaterThanOrEqualToImmediate, makeDesc<CompareUnit>(CompareUnit::Operation::GreaterThanOrEqualTo, true) },
				{ CompareOp::Eq, makeDesc<CompareUnit>(CompareUnit::Operation::Eq, false) },
				{ CompareOp::EqImmediate, makeDesc<CompareUnit>(CompareUnit::Operation::Eq, true) },
				{ CompareOp::Neq, makeDesc<CompareUnit>(CompareUnit::Operation::Neq, false) },
				{ CompareOp::NeqImmediate, makeDesc<CompareUnit>(CompareUnit::Operation::Neq, true) },
			};
            auto op = static_cast<CompareOp>(getOperation());
			auto result = translationTable.find(op);
			if (result == translationTable.end()) {
                static std::map<CompareOp, UnitDescription<PredicateComparator>> predicateOperations = {
                    { CompareOp::CRAnd, makeDesc<PredicateComparator>(PredicateComparator::Operation::BinaryAnd, false) },
                    { CompareOp::CROr, makeDesc<PredicateComparator>(PredicateComparator::Operation::BinaryOr, false) },
                    { CompareOp::CRNand, makeDesc<PredicateComparator>(PredicateComparator::Operation::BinaryNand, false) },
                    { CompareOp::CRNor, makeDesc<PredicateComparator>(PredicateComparator::Operation::BinaryNor, false) },
                    { CompareOp::CRXor, makeDesc<PredicateComparator>(PredicateComparator::Operation::BinaryXor, false) },
                    { CompareOp::CRNot, makeDesc<PredicateComparator>(PredicateComparator::Operation::UnaryNot, false) },
                };
                auto result = predicateOperations.find(op);
                if (result == predicateOperations.end()) {
                    switch(op) {
                        case CompareOp::CRSwap:
                            syn::swap<bool>(predicateResult(), predicateInverseResult());
                            break;
                        case CompareOp::CRMove:
                            predicateResult() = predicateInverseResult();
                            break;
                        case CompareOp::SaveCRs:
                            destinationRegister() = savePredicateRegisters(getImmediate());
                            break;
                        case CompareOp::RestoreCRs:
                            restorePredicateRegisters(destinationRegister(), getImmediate());
                            break;
                        default:
                            throw syn::Problem("Defined but unimplemented condition register operation!");
                    }
                } else {
                    typename decltype(_pcompare)::Operation pop;
                    bool immediate = false;
                    std::tie(pop, immediate) = result->second;
                    auto result = _pcompare.performOperation(pop, predicateSource0(), predicateSource1());
                    predicateResult() = result;
                    if (getPredicateResult() != getPredicateInverse()) {
                        predicateInverseResult() = !result;
                    }
                }
			} else {
				typename decltype(_compare)::Operation op;
				bool immediate = false;
				std::tie(op, immediate) = result->second;
				auto result = _compare.performOperation(op, source0Register(), immediate ? getHalfImmediate() : source1Register()) != 0;
				predicateResult() = result;
				if (getPredicateResult() != getPredicateInverse()) {
					predicateInverseResult() = !result;
				}
			}
		} else if (group == InstructionGroup::Jump) {
			// conditional?, immediate?, link?
			static std::map<JumpOp, std::tuple<bool, bool, bool>> translationTable = {
				{ JumpOp:: BranchUnconditionalImmediate ,       std::make_tuple(false, true, false) } ,
				{ JumpOp:: BranchUnconditionalImmediateLink ,   std::make_tuple(false, true, true) } ,
				{ JumpOp:: BranchUnconditional ,                std::make_tuple(false, false, false) } ,
				{ JumpOp:: BranchUnconditionalLink ,            std::make_tuple(false, false, true) } ,
				{ JumpOp:: BranchConditionalImmediate ,     std::make_tuple(true, true, false) } ,
				{ JumpOp:: BranchConditionalImmediateLink , std::make_tuple(true, true, true) } ,
				{ JumpOp:: BranchConditional ,              std::make_tuple(true, false, false) } ,
				{ JumpOp:: BranchConditionalLink ,          std::make_tuple(true, false, true) } ,
			};
			auto operation = static_cast<JumpOp>(getOperation());
			auto result = translationTable.find(operation);
			if (result == translationTable.end()) {
				word temporaryAddress = 0;
				bool cond = false;
				advanceIp = false;
				switch(operation) {
                    case JumpOp::IfThenElse:
                        cond = predicateResult();
                        getInstructionPointer() = gpr[cond ? getSource0() : getSource1()];
                        break;
                    case JumpOp::IfThenElseLink:
                        cond = predicateResult();
                        getLinkRegister() = getInstructionPointer() + 1;
                        getInstructionPointer() = gpr[cond ? getSource0() : getSource1()];
                        break;
					case JumpOp::BranchUnconditionalLR:
						getInstructionPointer() = getLinkRegister();
						break;
					case JumpOp::BranchUnconditionalLRAndLink:
						temporaryAddress = getInstructionPointer() + 1;
						getInstructionPointer() = getLinkRegister();
						getLinkRegister() = temporaryAddress;
						break;
					case JumpOp::BranchConditionalLR:
						cond = predicateResult();
						getInstructionPointer() = cond ? getLinkRegister() : getInstructionPointer() + 1;
						break;
					case JumpOp::BranchConditionalLRAndLink:
						temporaryAddress = getInstructionPointer() + 1;
						cond = predicateResult();
						getInstructionPointer() = cond ? getLinkRegister() : temporaryAddress;
						getLinkRegister() = cond ? temporaryAddress : getLinkRegister();
						break;
					default:
						throw syn::Problem("defined but unimplemented operation!");
				}
			} else {
				auto conditional = false, immediate = false,  link = false;
				std::tie(conditional, immediate, link) = result->second;
				auto newAddr = static_cast<word>(0);
				auto cond = true;
				advanceIp = false;
				auto ip = getInstructionPointer();
				if (conditional) {
					auto cond = predicateResult();
					newAddr = cond ? (immediate ? getImmediate() : source0Register()) : ip + 1;
				} else {
					newAddr = immediate ? getImmediate() : destinationRegister();
				}
				getInstructionPointer() = newAddr;
				if (link && cond) {
					getLinkRegister() = ip + 1;
				}
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
				case MoveOp::LoadWithOffset:
					gpr.set(getDestination(), data[source0Register() + getHalfImmediate()]);
					break;
				case MoveOp::Store:
					data.set(destinationRegister(), source0Register());
					break;
				case MoveOp::StoreWithOffset:
					data.set(destinationRegister() + getHalfImmediate(), source0Register());
					break;
				case MoveOp::Memset:
					data.set(destinationRegister(), getImmediate());
					break;
				case MoveOp::Push:
					stack[++destinationRegister()] = source0Register();
					break;
				case MoveOp::PushImmediate:
					stack[++destinationRegister()] = getImmediate();
					break;
				case MoveOp::Pop:
					destinationRegister() = stack[source0Register()];
					--source0Register();
					break;
				case MoveOp::LoadCode:
					codeStorage = instruction[destinationRegister()];
					source0Register() = syn::getLowerHalf(codeStorage);
					source1Register() = syn::getUpperHalf(codeStorage);
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
				case MoveOp::IOReadWithOffset:
					destinationRegister() = _io.read(source0Register() + getHalfImmediate());
					break;
				case MoveOp::IOWriteWithOffset:
					_io.write(destinationRegister() + getHalfImmediate(), source0Register());
					break;
				case MoveOp::MoveFromIP:
					destinationRegister() = getInstructionPointer();
					break;
				case MoveOp::MoveToIP:
					getInstructionPointer() = destinationRegister();
					advanceIp = false;
					break;
				case MoveOp::MoveFromLR:
					destinationRegister() = getLinkRegister();
					break;
				case MoveOp::MoveToLR:
					getLinkRegister() = destinationRegister();
					break;
				default:
					makeIllegalOperationMessage("move code");
					break;
			}
		} else if (group == InstructionGroup::DoubleWord) {
			dispatch32();
		} else {
			makeProblem("Illegal instruction group", getGroup());
		}
	}

	void Core::restorePredicateRegisters(word input, word mask) noexcept {
		Core::PredicateRegisterDecoder<15>::invoke(this, input, mask);
	}

	word Core::savePredicateRegisters(word mask) noexcept {
		return Core::PredicateRegisterEncoder<15>::invoke(this, mask);
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
				throw syn::Problem("unaligned object file found!");
			} else if (input.gcount() == 0) {
				if (input.eof()) {
					break;
				} else {
					throw syn::Problem("Something bad happened while reading input file!");
				}
			}
			//ignore the first byte, it is always zero
			auto target = static_cast<Segment>(buf[1]);
			auto address = iris::encodeWord(buf[2], buf[3]);
			if (debugEnabled()) {
				std::cerr << "current target = " << static_cast<int>(target) << "\tcurrent address = 0x" << std::hex << address << std::endl;
			}
			if (target == Segment::Code) {
				auto result = iris::encodeDword(buf[4], buf[5], buf[6], buf[7]);
				if (debugEnabled()) {
					std::cerr << " code result: 0x" << std::hex << result << std::endl;
				}
				writeInstructionMemory(address, result);
			} else if (target == Segment::Data) {
				auto result = iris::encodeWord(buf[4], buf[5]);
				if (debugEnabled()) {
					std::cerr << " data result: 0x" << std::hex << result << std::endl;
				}
				writeDataMemory(address, result);
			} else {
				std::stringstream str;
				str << "error: line " << lineNumber << ", unknown segment " << static_cast<int>(target) << "/" << static_cast<int>(buf[1]) << std::endl;
				str << "current address: " << std::hex << address << std::endl;
				throw syn::Problem(str.str());
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
		auto readNothing = syn::readNothing<typename LambdaIODevice::DataType, typename LambdaIODevice::AddressType>;
		// terminate
		_io.install(std::make_shared<LambdaIODevice>(0, 1, readNothing,
					[this](word address, word value) {
						execute = false;
						advanceIp = false;
					}));
		// getc and putc
		_io.install(std::make_shared<syn::StandardInputOutputDevice<word>>(1));
		for (auto i = 0; i < _cr.getSize(); ++i) {
			_cr[i] = false;
		}
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
		return new iris::Core();
	}
	void Core::writeRegister(byte index, word value) {
		gpr.write(index, value);
	}

	word Core::readRegister(byte index) {
		return gpr.read(index);
	}

	bool& Core::getPredicateRegister(byte index) {
		return _cr[index];
	}

}
