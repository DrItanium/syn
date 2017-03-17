/*
 * syn
 * Copyright (c) 2013-2017, Joshua Scoggins and Contributors
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */


#include "IrisCore.h"
#include <functional>
#include <sstream>
#include <vector>
#include "IrisCoreSecondaryStorageController.h"

namespace iris {
	Core::Core() noexcept : execute(true), advanceIp(true), current(0), _ip(0), _lr(0), _error(0), _io(0, 0xFFFF, "IrisCoreIOBootstrap.clp") { }

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
	void Core::saveSystemState() noexcept {
		_saveAdvanceIp = advanceIp;
		_saveExecute = execute;
		_onError[0] = getInstructionPointer();
		_onError[1] = getLinkRegister();
		for(int i = ArchitectureConstants::ErrorRegisterStart, j = 2; j < ArchitectureConstants::RegistersToSaveOnError; --i, ++j) {
			_onError[j] = gpr[i];
		}
	}
	void Core::restoreSystemState() noexcept {
		advanceIp = _saveAdvanceIp;
		execute = _saveExecute;
		setInstructionPointer(_onError[0]);
		setLinkRegister(_onError[1]);
		for(int i = ArchitectureConstants::ErrorRegisterStart, j = 2; j < ArchitectureConstants::RegistersToSaveOnError; --i, ++j) {
			gpr[i] = _onError[j];
		}
	}
	void Core::dispatchInterruptHandler() {
        if (_inInterruptHandler) {
            throw syn::Problem("Double interrupt fault!");
        }
        _inInterruptHandler = true;
        saveSystemState();
        _error = encodeStatusInError(_error, true);
        setInstructionPointer(data[ArchitectureConstants::ErrorDispatchVectorBase]);
		gpr[255] = _error;
		gpr[254] = getGroup();
		gpr[253] = getOperationByte();
		gpr[252] = getDestination();
		gpr[251] = getSource0();
		gpr[250] = getSource1();
		gpr[249] = getImmediate();
		advanceIp = false;
        _error = 0; // clear out the error field now that we have transferred it
        // now we have to perform the normal work of the cycle
        while(_inInterruptHandler && execute) {
            execute = cycle();
        }
        restoreSystemState();
	}
	bool Core::cycle() {
        advanceIp = true;
        dispatch();
        if (advanceIp) {
            incrementInstructionPointer();
        }
        if (_error != 0) {
            dispatchInterruptHandler();
        }
		return execute;
	}
	template<typename T>
	using UnitDescription = std::tuple<typename T::Operation, bool>;
	template<typename T>
	UnitDescription<T> makeDesc(typename T::Operation operation, bool immediate) noexcept {
		return std::make_tuple(operation, immediate);
	}
	void Core::dispatch() noexcept {
		current = instruction[getInstructionPointer()];
		auto group = static_cast<InstructionGroup>(getGroup());
		auto updateStatusRegister = [this](auto fn, bool value) { _error = fn(_error, value); };
		auto enableStatusRegisterBit = [this, updateStatusRegister](auto fn) { updateStatusRegister(fn, true); };
		auto makeIllegalInstructionMessage = [this, enableStatusRegisterBit](const std::string& type) { enableStatusRegisterBit(encodeStatusIllegalInstruction); };
		auto arithmeticOperation = [this, updateStatusRegister, enableStatusRegisterBit, makeIllegalInstructionMessage]() {
			static std::map<ArithmeticOp, UnitDescription<ALU>> table = {
				{ ArithmeticOp::Add, makeDesc<ALU>(ALU::Operation::Add , false) },
				{ ArithmeticOp::Sub, makeDesc<ALU>(ALU::Operation::Subtract , false ) },
				{ ArithmeticOp::Mul, makeDesc<ALU>(ALU::Operation::Multiply , false ) } ,
				{ ArithmeticOp::ShiftLeft, makeDesc<ALU>(ALU::Operation::ShiftLeft , false ) },
				{ ArithmeticOp::ShiftRight, makeDesc<ALU>(ALU::Operation::ShiftRight , false ) },
				{ ArithmeticOp::BinaryAnd, makeDesc<ALU>(ALU::Operation::BinaryAnd , false ) },
				{ ArithmeticOp::BinaryOr, makeDesc<ALU>(ALU::Operation::BinaryOr , false ) },
				{ ArithmeticOp::BinaryNot, makeDesc<ALU>(ALU::Operation::UnaryNot , false) },
				{ ArithmeticOp::BinaryXor, makeDesc<ALU>(ALU::Operation::BinaryXor , false ) },
				{ ArithmeticOp::AddImmediate, makeDesc<ALU>(ALU::Operation::Add , true  ) },
				{ ArithmeticOp::SubImmediate, makeDesc<ALU>(ALU::Operation::Subtract , true  ) },
				{ ArithmeticOp::MulImmediate, makeDesc<ALU>(ALU::Operation::Multiply , true  ) } ,
				{ ArithmeticOp::ShiftLeftImmediate, makeDesc<ALU>(ALU::Operation::ShiftLeft , true ) },
				{ ArithmeticOp::ShiftRightImmediate, makeDesc<ALU>(ALU::Operation::ShiftRight , true ) },
			};
			auto op = getOperation<ArithmeticOp>();
			auto result = table.find(op);
			auto denominatorIsZero = [](auto src1) { return src1 == 0; };
			auto markDivideByZero = [this, enableStatusRegisterBit]() { enableStatusRegisterBit(encodeStatusDivideByZero); };
			auto divide = [this, denominatorIsZero, markDivideByZero](word denominator) {
				if (denominatorIsZero(denominator)) {
					markDivideByZero();
				} else {
					destinationRegister() = source0Register() / denominator;
				}
			};
			auto remainder = [this, denominatorIsZero, markDivideByZero](word denominator) {
				if (denominatorIsZero(denominator)) {
					markDivideByZero();
				} else {
					destinationRegister() = source0Register() % denominator;
				}
			};
			if (result == table.end()) {
                switch(op) {
					case ArithmeticOp::Div:
						divide(source1Register());
						break;
					case ArithmeticOp::DivImmediate:
						divide(getHalfImmediate());
						break;
					case ArithmeticOp::Rem:
						remainder(source1Register());
						break;
					case ArithmeticOp::RemImmediate:
						remainder(getHalfImmediate());
						break;
                    case ArithmeticOp::Min:
                        destinationRegister() = source0Register() < source1Register() ? source0Register() : source1Register();
                        break;
                    case ArithmeticOp::Max:
                        destinationRegister() = source0Register() > source1Register() ? source0Register() : source1Register();
                        break;
                    default:
				        makeIllegalInstructionMessage("arithmetic operation");
                }
			} else {
				performOperation(_alu, result->second);
			}
		};
		auto compareOperation = [this, makeIllegalInstructionMessage]() {
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
			auto result = translationTable.find(getOperation<CompareOp>());
			if (result == translationTable.end()) {
				makeIllegalInstructionMessage("compare code");
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
		};
		auto jumpOperation = [this, makeIllegalInstructionMessage]() {
			// conditional?, immediate?, link?
			static std::map<JumpOp, std::tuple<bool, bool, bool>> translationTable = {
				{ JumpOp:: BranchUnconditionalImmediate ,       std::make_tuple(false, true, false) } ,
				{ JumpOp:: BranchUnconditionalImmediateLink ,   std::make_tuple(false, true, true) } ,
				{ JumpOp:: BranchUnconditional ,                std::make_tuple(false, false, false) } ,
				{ JumpOp:: BranchUnconditionalLink ,            std::make_tuple(false, false, true) } ,
				{ JumpOp:: BranchConditionalImmediate ,         std::make_tuple(true, true, false) } ,
				{ JumpOp:: BranchConditionalImmediateLink ,     std::make_tuple(true, true, true) } ,
				{ JumpOp:: BranchConditional ,                  std::make_tuple(true, false, false) } ,
				{ JumpOp:: BranchConditionalLink ,              std::make_tuple(true, false, true) } ,
			};
			auto operation = getOperation<JumpOp>();
			auto result = translationTable.find(operation);
			if (result == translationTable.end()) {
				word temporaryAddress = 0;
				bool cond = false;
				advanceIp = false;
				auto returnFromError = [this]() {
                    if (_inInterruptHandler) {
                        _inInterruptHandler = false;
					} else {
						throw syn::Problem("ATTEMPTED TO RETURN FROM AN INTERRUPT WHEN NOT IN ONE!!!!");
					}
				};
				switch(operation) {
                    case JumpOp::IfThenElse:
                        cond = predicateResult();
                        setInstructionPointer(gpr[cond ? getSource0() : getSource1()]);
                        break;
                    case JumpOp::IfThenElseLink:
                        cond = predicateResult();
                        setLinkRegister(getInstructionPointer() + 1);
                        setInstructionPointer(gpr[cond ? getSource0() : getSource1()]);
                        break;
					case JumpOp::BranchUnconditionalLR:
                        setInstructionPointer(getLinkRegister());
						break;
					case JumpOp::BranchUnconditionalLRAndLink:
						temporaryAddress = getInstructionPointer() + 1;
                        setInstructionPointer(getLinkRegister());
                        setLinkRegister(temporaryAddress);
						break;
					case JumpOp::BranchConditionalLR:
						cond = predicateResult();
                        setInstructionPointer(cond ? getLinkRegister() : getInstructionPointer() + 1);
						break;
					case JumpOp::BranchConditionalLRAndLink:
						temporaryAddress = getInstructionPointer() + 1;
						cond = predicateResult();
                        setInstructionPointer(cond ? getLinkRegister() : temporaryAddress);
                        setLinkRegister(cond ? temporaryAddress : getLinkRegister());
						break;
					case JumpOp::ReturnFromError:
						returnFromError();
						break;
					default:
						makeIllegalInstructionMessage("defined but unimplemented operation!");
						break;
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
                setInstructionPointer(newAddr);
				if (link && cond) {
                    setLinkRegister(ip + 1);
				}
			}
		};
		auto moveOperation = [this, makeIllegalInstructionMessage]() {
			auto op = getOperation<MoveOp>();
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
				case MoveOp::IORead:
                    destinationRegister() = ioSpaceRead(source0Register());
					break;
				case MoveOp::IOReadWithOffset:
                    destinationRegister() = ioSpaceRead(source0Register() + getHalfImmediate());
					break;
				case MoveOp::IOWrite:
                    ioSpaceWrite(destinationRegister(), source0Register());
					break;
				case MoveOp::IOWriteWithOffset:
                    ioSpaceWrite(destinationRegister() + getHalfImmediate(), source0Register());
					break;
				case MoveOp::MoveFromIP:
					destinationRegister() = getInstructionPointer();
					break;
				case MoveOp::MoveToIP:
                    setInstructionPointer(destinationRegister());
					advanceIp = false;
					break;
				case MoveOp::MoveFromLR:
					destinationRegister() = getLinkRegister();
					break;
				case MoveOp::MoveToLR:
                    setLinkRegister(destinationRegister());
					break;
				default:
					makeIllegalInstructionMessage("move code");
					break;
			}
		};
		auto conditionalRegisterOperation = [this, makeIllegalInstructionMessage]() {
			static std::map<ConditionRegisterOp, UnitDescription<PredicateComparator>> translationTable = {
				{ ConditionRegisterOp::CRAnd, makeDesc<PredicateComparator>(PredicateComparator::Operation::BinaryAnd, false) },
				{ ConditionRegisterOp::CROr, makeDesc<PredicateComparator>(PredicateComparator::Operation::BinaryOr, false) },
				{ ConditionRegisterOp::CRNand, makeDesc<PredicateComparator>(PredicateComparator::Operation::BinaryNand, false) },
				{ ConditionRegisterOp::CRNor, makeDesc<PredicateComparator>(PredicateComparator::Operation::BinaryNor, false) },
				{ ConditionRegisterOp::CRXor, makeDesc<PredicateComparator>(PredicateComparator::Operation::BinaryXor, false) },
				{ ConditionRegisterOp::CRNot, makeDesc<PredicateComparator>(PredicateComparator::Operation::UnaryNot, false) },
			};
			auto op = getOperation<ConditionRegisterOp>();
			auto result = translationTable.find(op);
			if (result  == translationTable.end()) {
				switch(op) {
					case ConditionRegisterOp::CRSwap:
						syn::swap<bool>(predicateResult(), predicateInverseResult());
						break;
					case ConditionRegisterOp::CRMove:
						predicateResult() = predicateInverseResult();
						break;
					case ConditionRegisterOp::SaveCRs:
						destinationRegister() = savePredicateRegisters(getImmediate());
						break;
					case ConditionRegisterOp::RestoreCRs:
						restorePredicateRegisters(destinationRegister(), getImmediate());
						break;
					default:
						makeIllegalInstructionMessage("Predicate operation!");
						break;
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

		};
		switch(group) {
			case InstructionGroup::Arithmetic:
				arithmeticOperation();
				break;
			case InstructionGroup::Compare:
				compareOperation();
				break;
			case InstructionGroup::Jump:
				jumpOperation();
				break;
			case InstructionGroup::Move:
				moveOperation();
				break;
			case InstructionGroup::ConditionalRegister:
				conditionalRegisterOperation();
				break;
			default:
				makeIllegalInstructionMessage("Illegal group!");
				break;
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
		for (auto i = 0; i < _cr.getSize(); ++i) {
			_cr[i] = false;
		}
		//auto readNothing = syn::readNothing<typename LambdaIODevice::DataType, typename LambdaIODevice::AddressType>;
		// terminate
		//_io.install(std::make_shared<LambdaIODevice>(0, 1, readNothing,
		//			[this](word address, word value) {
		//				execute = false;
		//				advanceIp = false;
		//			}));
		//// getc and putc
		//_io.install(std::make_shared<syn::StandardInputOutputDevice<word>>(1));
		//_io.install(std::make_shared<syn::RandomDevice<word, word>>(3));
		//_io.install(std::make_shared<SecondaryStorageController>(0xA));
	}

	void Core::shutdown() {
		gpr.shutdown();
		data.shutdown();
		instruction.shutdown();
		stack.shutdown();
		_io.shutdown();
	}

	//void Core::installIODevice(std::shared_ptr<IODevice> dev) {
	//	_io.install(dev);
	//}

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

    raw_instruction encodeInstruction(byte group, byte operation, byte dest, byte src0, byte src1) {
        return encodeSource1( encodeSource0( encodeDestination(encodeOperation( encodeGroup(0, group), operation), dest), src0), src1);
    }
    raw_instruction encodeInstruction(byte group, byte operation, byte dest, word immediate) {
        return encodeInstruction(group, operation, dest, syn::getLowerHalf(immediate), syn::getUpperHalf(immediate));
    }

    word Core::ioSpaceRead(word address) noexcept {
        return address == 0 ? 0 : _io.read(address);
    }
    void Core::ioSpaceWrite(word address, word value) noexcept {
        if (address == 0) {
            // this is execution termination if you write to address zero in IO
            // space!
            execute = false;
            advanceIp = false;
        } else {
            _io.write(address, value);
        }
    }

}
