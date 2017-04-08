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
#include "IrisClipsExtensions.h"

namespace iris {
	constexpr dword encodeDword(byte a, byte b, byte c, byte d) noexcept {
		return syn::encodeUint32LE(a, b, c, d);
	}
	constexpr word encodeWord(byte a, byte b) noexcept {
		return syn::encodeUint16LE(a, b);
	}
	constexpr dword encodeDword(word lower, word upper) noexcept {
		return syn::encodeUint32LE(lower, upper);
	}
	Core::Core() noexcept : execute(true), advanceIp(true), current(0), _ip(0), _lr(0), _error(0), _io(0, 0xFFFF, "IrisCoreIOBootstrap.clp") { }

	Core::~Core() {
	}


    constexpr bool isImmediate(CompareOp op) noexcept {
        return op == CompareOp::LessThanOrEqualToImmediate ||
            op == CompareOp::GreaterThanImmediate ||
            op == CompareOp::LessThanImmediate ||
            op == CompareOp::GreaterThanOrEqualToImmediate ||
            op == CompareOp::EqImmediate ||
            op == CompareOp::NeqImmediate;
    }
    constexpr bool isImmediate(ArithmeticOp op) noexcept {
        return op == ArithmeticOp::ShiftLeftImmediate ||
            op == ArithmeticOp::ShiftRightImmediate ||
            op == ArithmeticOp::AddImmediate ||
            op == ArithmeticOp::SubImmediate ||
            op == ArithmeticOp::MulImmediate ||
            op == ArithmeticOp::DivImmediate ||
            op == ArithmeticOp::RemImmediate;
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
		gpr[254] = InstructionDecoder::getGroupByte(current);
		gpr[253] = InstructionDecoder::getOperationByte(current);
		gpr[252] = InstructionDecoder::getDestinationIndex(current);
		gpr[251] = InstructionDecoder::getSource0Index(current);
		gpr[250] = InstructionDecoder::getSource1Index(current);
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
    using ALUOperation = syn::ALU::StandardOperations;
	void Core::dispatch() noexcept {
		current = instruction[getInstructionPointer()];
		auto group = InstructionDecoder::getGroup(current);
		auto updateStatusRegister = [this](auto fn, bool value) { _error = fn(_error, value); };
		auto enableStatusRegisterBit = [this, updateStatusRegister](auto fn) { updateStatusRegister(fn, true); };
		auto makeIllegalInstructionMessage = [this, enableStatusRegisterBit](const std::string& type) { enableStatusRegisterBit(encodeStatusIllegalInstruction); };
		auto arithmeticOperation = [this, enableStatusRegisterBit, makeIllegalInstructionMessage]() {
			static std::map<ArithmeticOp, ALUOperation> table = {
				{ ArithmeticOp::Add, (ALUOperation::Add ) },
				{ ArithmeticOp::Sub, (ALUOperation::Subtract ) },
				{ ArithmeticOp::Mul, (ALUOperation::Multiply ) } ,
				{ ArithmeticOp::ShiftLeft, (ALUOperation::ShiftLeft ) },
				{ ArithmeticOp::ShiftRight, (ALUOperation::ShiftRight ) },
				{ ArithmeticOp::BinaryAnd, (ALUOperation::BinaryAnd ) },
				{ ArithmeticOp::BinaryOr, (ALUOperation::BinaryOr ) },
				{ ArithmeticOp::BinaryNot, (ALUOperation::UnaryNot ) },
				{ ArithmeticOp::BinaryXor, (ALUOperation::BinaryXor ) },
				{ ArithmeticOp::AddImmediate, (ALUOperation::Add ) },
				{ ArithmeticOp::SubImmediate, (ALUOperation::Subtract ) },
				{ ArithmeticOp::MulImmediate, (ALUOperation::Multiply ) } ,
				{ ArithmeticOp::ShiftLeftImmediate, (ALUOperation::ShiftLeft ) },
				{ ArithmeticOp::ShiftRightImmediate, (ALUOperation::ShiftRight ) },
			};
			auto op = InstructionDecoder::getOperation<ArithmeticOp>(current);
			auto result = table.find(op);
			auto markDivideByZero = [this, enableStatusRegisterBit]() { enableStatusRegisterBit(encodeStatusDivideByZero); };
			auto divide = [this, markDivideByZero](word denominator) {
                if (denominator == 0) {
					markDivideByZero();
				} else {
					destinationRegister() = source0Register() / denominator;
				}
			};
			auto remainder = [this, markDivideByZero](word denominator) {
                if (denominator == 0) {
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
                ALUOperation theOp = result->second;
                destinationRegister() = syn::ALU::performOperation<word>(theOp, source0Register(), isImmediate(op) ? getHalfImmediate() : source1Register());
			}
		};
		auto compareOperation = [this, makeIllegalInstructionMessage]() {
			static std::map<CompareOp, syn::Comparator::StandardOperations> translationTable = {
				{ CompareOp::LessThan, (syn::Comparator::StandardOperations::LessThan) },
				{ CompareOp::LessThanImmediate, (syn::Comparator::StandardOperations::LessThan) },
				{ CompareOp::LessThanOrEqualTo, (syn::Comparator::StandardOperations::LessThanOrEqualTo) },
				{ CompareOp::LessThanOrEqualToImmediate, (syn::Comparator::StandardOperations::LessThanOrEqualTo) },
				{ CompareOp::GreaterThan, (syn::Comparator::StandardOperations::GreaterThan) },
				{ CompareOp::GreaterThanImmediate, (syn::Comparator::StandardOperations::GreaterThan) },
				{ CompareOp::GreaterThanOrEqualTo, (syn::Comparator::StandardOperations::GreaterThanOrEqualTo) },
				{ CompareOp::GreaterThanOrEqualToImmediate, (syn::Comparator::StandardOperations::GreaterThanOrEqualTo) },
				{ CompareOp::Eq, (syn::Comparator::StandardOperations::Eq) },
				{ CompareOp::EqImmediate, (syn::Comparator::StandardOperations::Eq) },
				{ CompareOp::Neq, (syn::Comparator::StandardOperations::Neq) },
				{ CompareOp::NeqImmediate, (syn::Comparator::StandardOperations::Neq) },
			};
            auto cop = InstructionDecoder::getOperation<CompareOp>(current);
			auto result = translationTable.find(cop);
			if (result == translationTable.end()) {
				makeIllegalInstructionMessage("compare code");
			} else {
				syn::Comparator::StandardOperations op = result->second;
                auto outcome = syn::Comparator::performOperation<word>(op, source0Register(), isImmediate(cop) ? getHalfImmediate() : source1Register()) != 0;
                setPredicateResult(outcome);
                if (!InstructionDecoder::samePredicateDestinations(current)) {
                    setPredicateInverseResult(!outcome);
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
			auto operation = InstructionDecoder::getOperation<JumpOp>(current);
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
                        cond = getPredicateResult();
                        setInstructionPointer(gpr[InstructionDecoder::chooseRegister(current, cond)]);
                        break;
                    case JumpOp::IfThenElseLink:
                        cond = getPredicateResult();
                        setLinkRegister(getInstructionPointer() + 1);
                        setInstructionPointer(gpr[InstructionDecoder::chooseRegister(current, cond)]);
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
                        cond = getPredicateResult();
                        setInstructionPointer(cond ? getLinkRegister() : getInstructionPointer() + 1);
						break;
					case JumpOp::BranchConditionalLRAndLink:
						temporaryAddress = getInstructionPointer() + 1;
						cond = getPredicateResult();
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
					cond = getPredicateResult();
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
            auto getDestinationIndex = [this]() { return InstructionDecoder::getDestinationIndex(current); };
            auto getSource0Index = [this]() { return InstructionDecoder::getSource0Index(current); };
			auto op = InstructionDecoder::getOperation<MoveOp>(current);
			raw_instruction codeStorage = 0u;
			switch(op) {
				case MoveOp::Move:
					gpr.copy(getDestinationIndex(), getSource0Index());
					break;
				case MoveOp::Set:
					gpr.set(getDestinationIndex(), getImmediate());
					break;
				case MoveOp::Swap:
					gpr.swap(getDestinationIndex(), getSource0Index());
					break;
				case MoveOp::Load:
					gpr.set(getDestinationIndex(), data[source0Register()]);
					break;
				case MoveOp::LoadImmediate:
					gpr.set(getDestinationIndex(), data[getImmediate()]);
					break;
				case MoveOp::LoadWithOffset:
					gpr.set(getDestinationIndex(), data[source0Register() + getHalfImmediate()]);
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
			static std::map<ConditionRegisterOp, syn::Comparator::BooleanOperations> translationTable = {
				{ ConditionRegisterOp::CRAnd, (syn::Comparator::BooleanOperations::BinaryAnd) },
				{ ConditionRegisterOp::CROr, (syn::Comparator::BooleanOperations::BinaryOr) },
				{ ConditionRegisterOp::CRNand, (syn::Comparator::BooleanOperations::BinaryNand) },
				{ ConditionRegisterOp::CRNor, (syn::Comparator::BooleanOperations::BinaryNor) },
				{ ConditionRegisterOp::CRXor, (syn::Comparator::BooleanOperations::BinaryXor) },
				{ ConditionRegisterOp::CRNot, (syn::Comparator::BooleanOperations::UnaryNot) },
			};
			auto op = InstructionDecoder::getOperation<ConditionRegisterOp>(current);
			auto result = translationTable.find(op);
			if (result  == translationTable.end()) {
				switch(op) {
					case ConditionRegisterOp::CRSwap:
                        _cr.swapBits(getPredicateResultIndex(), getPredicateInverseResultIndex());
						break;
					case ConditionRegisterOp::CRMove:
						setPredicateResult(getPredicateInverseResult());
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
                syn::Comparator::BooleanOperations pop = result->second;
                auto result = syn::Comparator::performOperation<bool, bool, syn::Comparator::BooleanOperations>(pop, getPredicateSource0(), getPredicateSource1());
                setPredicateResult(result);
                if (!InstructionDecoder::samePredicateDestinations(current)) {
                    setPredicateInverseResult(!result);
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
        if (mask == 0x0000) {
            return;
        }
        if (mask == 0xFFFF) {
            _cr.set(input);
        } else {
            _cr.encode<word>(input, mask, 0);
        }
	}

	word Core::savePredicateRegisters(word mask) noexcept {
        if (mask == 0) {
            return 0;
        }
        if (mask == 0xFFFF) {
            return _cr.get();
        } else {
            return _cr.decode(mask, 0);
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
        installAssemblerParsingState(_io.getRawEnvironment());
        _cr.set(0);
	}

	void Core::shutdown() {
		gpr.shutdown();
		data.shutdown();
		instruction.shutdown();
		stack.shutdown();
		_io.shutdown();
	}

	bool Core::getPredicateRegister(byte index) const {
        return _cr.getBit(index);
	}

    void Core::setPredicateRegister(byte index, bool bit) {
        _cr.setBit(index, bit);
    }

    word Core::ioSpaceRead(word address) noexcept {
        return address == ArchitectureConstants::TerminateIOAddress ? 0 : _io.read(address);
    }

    void Core::ioSpaceWrite(word address, word value) noexcept {
        if (address == ArchitectureConstants::TerminateIOAddress) {
            // this is execution termination if you write to address zero in IO
            // space!
            execute = false;
            advanceIp = false;
        } else {
            _io.write(address, value);
        }
    }
    bool Core::getPredicateResult() const noexcept { return getPredicate<0>(); }
    bool Core::getPredicateInverseResult() const noexcept { return getPredicate<1>(); }
    bool Core::getPredicateSource0() const noexcept { return getPredicate<2>(); }
    bool Core::getPredicateSource1() const noexcept { return getPredicate<3>(); }
    byte Core::getPredicateResultIndex() const noexcept { return getPredicateIndex<0>(); }
    byte Core::getPredicateInverseResultIndex() const noexcept { return getPredicateIndex<1>(); }
    byte Core::getPredicateSource0Index() const noexcept { return getPredicateIndex<2>(); }
    byte Core::getPredicateSource1Index() const noexcept { return getPredicateIndex<3>(); }
    void Core::setPredicateResult(bool bit) noexcept { setPredicate<0>(bit); }
    void Core::setPredicateInverseResult(bool bit) noexcept { setPredicate<1>(bit); }
    void Core::setPredicateSource0(bool bit) noexcept { setPredicate<2>(bit); }
    void Core::setPredicateSource1(bool bit) noexcept { setPredicate<3>(bit); }
    word& Core::destinationRegister() noexcept { return getRegister<0>(); }
    word& Core::source0Register() noexcept { return getRegister<1>(); }
    word& Core::source1Register() noexcept { return getRegister<2>(); }
    void Core::setInstructionPointer(QuadWord value) noexcept { _ip.set(value); }
    void Core::setLinkRegister(QuadWord value) noexcept { _lr.set(value); }
    void Core::incrementInstructionPointer() noexcept { ++_ip; }
}
