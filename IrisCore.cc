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
	constexpr word encodeWord(byte a, byte b) noexcept {
		return syn::encodeUint16LE(a, b);
	}
	constexpr dword encodeDword(word lower, word upper) noexcept {
		return syn::encodeUint32LE(lower, upper);
	}

	Core::Core(syn::CLIPSIOController& io) noexcept : Parent(io), execute(true), advanceIp(true), current(0), _ip(0), _lr(0), _error(0) { }

	Core::~Core() {
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
		gpr[252] = getDestinationIndex();
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

    constexpr bool isNormalBranchInstruction(JumpOp op) noexcept {
            switch(op) {
                case JumpOp::BranchUnconditionalImmediate:
                case JumpOp::BranchUnconditionalImmediateLink:
                case JumpOp::BranchUnconditionalLink:
                case JumpOp::BranchUnconditional:
                case JumpOp::BranchConditionalImmediate:
                case JumpOp::BranchConditionalImmediateLink:
                case JumpOp::BranchConditionalLink:
                case JumpOp::BranchConditional:
                    return true;
                default:
                    return false;
            }
    }
    constexpr bool isConditionalBranchInstruction(JumpOp op) noexcept {
        switch(op) {
            case JumpOp::BranchConditionalImmediate:
            case JumpOp::BranchConditionalImmediateLink:
            case JumpOp::BranchConditionalLink:
            case JumpOp::BranchConditional:
                return true;
            default:
                return false;
        }
    }
    constexpr bool isLinkBranchInstruction(JumpOp op) noexcept {
        switch(op) {
            case JumpOp::BranchUnconditionalImmediateLink:
            case JumpOp::BranchUnconditionalLink:
            case JumpOp::BranchConditionalImmediateLink:
            case JumpOp::BranchConditionalLink:
                return true;
            default:
                return false;
        }
    }
    constexpr bool isImmediate(JumpOp op) noexcept {
        switch(op) {
            case JumpOp::BranchUnconditionalImmediate:
            case JumpOp::BranchConditionalImmediate:
            case JumpOp::BranchUnconditionalImmediateLink:
            case JumpOp::BranchConditionalImmediateLink:
                return true;
            default:
                return false;
        }
    }
    constexpr bool isImmediate(CompareOp op) noexcept {
        switch(op) {
            case CompareOp::LessThanOrEqualToImmediate :
            case CompareOp::GreaterThanImmediate :
            case CompareOp::LessThanImmediate :
            case CompareOp::GreaterThanOrEqualToImmediate :
            case CompareOp::EqImmediate :
            case CompareOp::NeqImmediate :
                return true;
            default:
                return false;
        }
    }
    constexpr bool isImmediate(ArithmeticOp op) noexcept {
        switch (op) {
            case ArithmeticOp::ShiftLeftImmediate:
            case ArithmeticOp::ShiftRightImmediate:
            case ArithmeticOp::AddImmediate:
            case ArithmeticOp::SubImmediate:
            case ArithmeticOp::MulImmediate:
            case ArithmeticOp::DivImmediate:
            case ArithmeticOp::RemImmediate:
                return true;
            default:
                return false;
        }
    }
    template<ArithmeticOp op>
    constexpr word minOrMax(word a, word b) noexcept {
        static_assert(op == ArithmeticOp::Min || op == ArithmeticOp::Max, "Illegal arithmetic op!");
        return ((op == ArithmeticOp::Min) ? (a < b) : (a > b))? a : b;
    }

	void Core::dispatch() noexcept {
		current = instruction[getInstructionPointer()];
		auto group = InstructionDecoder::getGroup(current);
		auto updateStatusRegister = [this](auto fn, bool value) { _error = fn(_error, value); };
		auto enableStatusRegisterBit = [this, updateStatusRegister](auto fn) { updateStatusRegister(fn, true); };
		auto makeIllegalInstructionMessage = [this, enableStatusRegisterBit](const std::string& type) { enableStatusRegisterBit(encodeStatusIllegalInstruction); };
		auto arithmeticOperation = [this, enableStatusRegisterBit, makeIllegalInstructionMessage]() {
			auto op = InstructionDecoder::getOperation<ArithmeticOp>(current);
            auto result = translate(op);
            if (syn::isErrorState(result)) {
                switch(op) {
                    case ArithmeticOp::Min:
                        destinationRegister() = minOrMax<ArithmeticOp::Min>(source0Register(), source1Register());
                        break;
                    case ArithmeticOp::Max:
                        destinationRegister() = minOrMax<ArithmeticOp::Max>(source0Register(), source1Register());
                        break;
                    default:
				        makeIllegalInstructionMessage("arithmetic operation");
                }
			} else {
                bool divByZeroHappened = false;
                syn::OnDivideByZero<word> markDivideByZero = [this, enableStatusRegisterBit, &divByZeroHappened]() {
                    divByZeroHappened = true;
                    enableStatusRegisterBit(encodeStatusDivideByZero);
                    return static_cast<word>(0);
                };
                auto outcome = syn::ALU::performOperation<word>(result, source0Register(), isImmediate(op) ? getHalfImmediate() : source1Register(), markDivideByZero);
                if (!divByZeroHappened) {
                    destinationRegister() = outcome;
                }
			}
		};
		auto compareOperation = [this, makeIllegalInstructionMessage]() {
            auto cop = InstructionDecoder::getOperation<CompareOp>(current);
            auto result = translate(cop);
            if (syn::isErrorState(result)) {
				makeIllegalInstructionMessage("compare code");
			} else {
				syn::Comparator::StandardOperations op = result;
                auto outcome = syn::Comparator::performOperation<word>(op, source0Register(), isImmediate(cop) ? getHalfImmediate() : source1Register()) != 0;
                setPredicateResult(outcome);
                if (!InstructionDecoder::samePredicateDestinations(current)) {
                    setPredicateInverseResult(!outcome);
				}
			}
		};
		auto jumpOperation = [this, makeIllegalInstructionMessage]() {
			auto operation = InstructionDecoder::getOperation<JumpOp>(current);
			if (!isNormalBranchInstruction(operation)) {
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
				auto newAddr = 0u;
				auto cond = true;
				advanceIp = false;
				auto ip = getInstructionPointer();
				if (isConditionalBranchInstruction(operation)) {
					cond = getPredicateResult();
					newAddr = cond ? (isImmediate(operation) ? getImmediate() : source0Register()) : ip + 1;
				} else {
					newAddr = isImmediate(operation) ? getImmediate() : destinationRegister();
				}
                setInstructionPointer(newAddr);
				if (isLinkBranchInstruction(operation) && cond) {
                    setLinkRegister(ip + 1);
				}
			}
		};
		auto moveOperation = [this, makeIllegalInstructionMessage]() {
            auto getSource0Index = [this]() { return InstructionDecoder::getSource0Index(current); };
			auto op = InstructionDecoder::getOperation<MoveOp>(current);
			raw_instruction codeStorage = 0u;
            auto pushValue = [this](auto value) {
                stack[++destinationRegister()] = value;
            };
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
				case MoveOp::StoreImmediate:
					data.set(destinationRegister(), getImmediate());
					break;
				case MoveOp::Push:
                    pushValue(source0Register());
					break;
				case MoveOp::PushImmediate:
                    pushValue(getImmediate());
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
				case MoveOp::LoadIO:
					destinationRegister() = readFromBus(source0Register());
					break;
				case MoveOp::LoadIOWithOffset:
					destinationRegister() = readFromBus(source0Register() + getHalfImmediate());
					break;
				case MoveOp::StoreIO:
					writeToBus(destinationRegister(), source0Register());
					break;
				case MoveOp::StoreIOWithOffset:
					writeToBus(destinationRegister() + getHalfImmediate(), source0Register());
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
                case MoveOp::SaveAllRegisters:
                    pushValue(destinationRegister()); // save the stack pointer
                    for (int i = 0; i < ArchitectureConstants::RegisterCount; ++i) {
                        if (i != getDestinationIndex()) {
                            pushValue(gpr[i]);
                        }
                    }
                    break;
                case MoveOp::RestoreAllRegisters:
                    for (int i = ArchitectureConstants::RegisterCount - 1; i >= 0; --i) {
                        if (i != getDestinationIndex()) {
                            gpr[i] = stack[destinationRegister()];
                            --destinationRegister();
                        }
                    }
                    destinationRegister() = stack[destinationRegister()];
                    break;
				default:
					makeIllegalInstructionMessage("move code");
					break;
			}
		};
		auto conditionalRegisterOperation = [this, makeIllegalInstructionMessage]() {
			auto op = InstructionDecoder::getOperation<ConditionRegisterOp>(current);
			auto result = translate(op);
            if (syn::isErrorState(result)) {
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
                syn::Comparator::BooleanOperations pop = result;
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
        installAssemblerParsingState(_bus.getRawEnvironment());
        _cr.set(0);
	}

	void Core::shutdown() {
		gpr.shutdown();
		data.shutdown();
		instruction.shutdown();
		stack.shutdown();
	}

	bool Core::getPredicateRegister(byte index) const {
        return _cr.getBit(index);
	}

    void Core::setPredicateRegister(byte index, bool bit) {
        _cr.setBit(index, bit);
    }

	word Core::readFromBus(word addr) {
		if (addr == ArchitectureConstants::TerminateIOAddress) {
			return 0;
		} else {
			return Parent::readFromBus(addr);
		}
	}

	void Core::writeToBus(word addr, word value) {
		if (addr == ArchitectureConstants::TerminateIOAddress) {
            // this is execution termination if you write to address zero in IO
            // space!
            execute = false;
            advanceIp = false;
		} else {
			Parent::writeToBus(addr, value);
		}
	}


    bool Core::getPredicateResult() const noexcept        { return getPredicate<0>(); }
    bool Core::getPredicateInverseResult() const noexcept { return getPredicate<1>(); }
    bool Core::getPredicateSource0() const noexcept       { return getPredicate<2>(); }
    bool Core::getPredicateSource1() const noexcept       { return getPredicate<3>(); }

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

    word Core::getHalfImmediate() const noexcept { return InstructionDecoder::getHalfImmediate(current); }
    word Core::getImmediate() const noexcept { return InstructionDecoder::getImmediate(current); }
    byte Core::getDestinationIndex() const noexcept {
        return InstructionDecoder::getDestinationIndex(current);
    }
}
