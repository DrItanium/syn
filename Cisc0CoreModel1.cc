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


#include "Cisc0CoreModel1.h"
#include <functional>
#include <sstream>
#include "Problem.h"
#include <utility>
#include <map>
#include "Cisc0ClipsExtensions.h"

namespace cisc0 {
    CoreModel1::CoreModel1() noexcept : Parent("Cisc0CoreModel1IOBus.clp"), _instruction(getInstructionPointer())  { }
    CoreModel1::~CoreModel1() noexcept { }

    void CoreModel1::initialize() {
        Parent::initialize();
		_terminateAddress = ArchitectureConstants::TerminateAddress;
    }

    bool CoreModel1::cycle() {
        _instruction.reset(loadWord(getInstructionPointer()),
                       loadWord(getInstructionPointer() + 1),
                       loadWord(getInstructionPointer() + 2));
        if (debugEnabled()) {
            std::cout << "Current Instruction Location: " << std::hex << getInstructionPointer() << std::endl;
            std::cout << "\tCurrent word value: " << std::hex << _instruction.firstWord().getRawValue() << std::endl;
        }
        // dispatch on the first one!
        dispatch();
        if (advanceIp) {
            incrementInstructionPointer();
        } else {
            // just re-enable it
            advanceIp = true;
        }
        return execute;
    }

    void CoreModel1::dispatch() {
        auto swapOperation = [this]() noexcept {
            constexpr auto group = Operation::Swap;
            auto dInd = _instruction.getDestinationRegister<group>();
            auto sInd = _instruction.getSourceRegister<group>();
            if (dInd != sInd) {
                _gpr.swap(dInd, sInd);
            }
        };
        auto moveOperation = [this]() {
            constexpr auto group = Operation::Move;
            using T = RegisterValue;
            auto dInd = destinationRegister<group>();
            auto source0 = sourceRegister<group>();
            auto bmask = _instruction.expandedBitmask<group>();
            registerValue(dInd) = syn::decodeBits<T, T>(source0, bmask, 0);
        };
        auto setOperation = [this]() {
            constexpr auto group = Operation::Set;
            auto dInd = _instruction.getDestinationRegister<group>();
            auto bmask = _instruction.getBitmask<group>();
            registerValue(dInd) = _instruction.retrieveImmediate(bmask);
        };
        switch(_instruction.getControl()) {
            case Operation::Shift:
                shiftOperation();
                break;
            case Operation::Arithmetic:
                arithmeticOperation();
                break;
            case Operation::Logical:
                logicalOperation();
                break;
            case Operation::Memory:
                memoryOperation();
                break;
            case Operation::Branch:
                branchOperation();
                break;
            case Operation::Compare:
                compareOperation();
                break;
            case Operation::Complex:
                complexOperation();
                break;
            case Operation::Swap:
                swapOperation();
                break;
			case Operation::Return:
                returnOperation();
				break;
            case Operation::Move:
                moveOperation();
                break;
            case Operation::Set:
                setOperation();
                break;
            default:
                execute = false;
                illegalInstruction(_instruction.firstWord(), getInstructionPointer());
                break;
        }
    }
    void CoreModel1::branchOperation() {
		static constexpr auto group = Operation::Branch;
        bool isCall, isCond;
        std::tie(isCall, isCond) = _instruction.firstWord().getOtherBranchFlags();
        advanceIp = true;
		auto whereToGo = _instruction.isImmediate<group>() ? _instruction.retrieveImmediate(0b1111) : registerValue(_instruction.firstWord().getBranchIndirectDestination());
        auto shouldUpdateInstructionPointer = isCall || (isCond && getConditionRegister()) || (!isCond);
        if (isCall) {
            // call instruction
            // figure out where we are going to go, this will cause loads and
            // incrementation of the instruction pointer.
            // Once done, we then push the next address following the newly
            // modified ip to the stack. Then we update the ip of where we are
            // going to go!
            pushRegisterValue(getInstructionPointer() + 1, getCallStackPointer());
        }
        // otherwise we are looking at a standard jump operation
        if (shouldUpdateInstructionPointer) {
            advanceIp = false;
            getInstructionPointer() = whereToGo;
        }
    }

    void CoreModel1::moveToCondition(byte index) noexcept {
		getConditionRegister() = registerValue(index) != 0;
    }
    void CoreModel1::moveFromCondition(byte index) noexcept {
        registerValue(index) = normalizeCondition(getConditionRegister());
    }

    void CoreModel1::compareOperation() {
        static constexpr auto group = Operation::Compare;
		auto compareResult = _instruction.getSubtype<group>();
		auto destinationIndex = _instruction.getDestinationRegister<group>();
        auto normalCompare = [this, destinationIndex, compareResult]() {
			auto compareUnitType = translate(compareResult);
        	syn::throwOnErrorState(compareResult, "Illegal compare type!");
			auto first = registerValue(destinationIndex);
			auto second = _instruction.isImmediate<group>() ?  _instruction.retrieveImmediate<group>() : sourceRegister<group>();
			getConditionRegister() = syn::Comparator::performOperation(compareUnitType, first, second);
        };
        switch(compareResult) {
            case CompareStyle::MoveToCondition:
                moveToCondition(destinationIndex);
                break;
            case CompareStyle::MoveFromCondition:
                moveFromCondition(destinationIndex);
                break;
            default:
                normalCompare();
                break;
        }
    }
    void CoreModel1::memoryOperation() {
        static constexpr auto group = Operation::Memory;
        auto rawMask = _instruction.getBitmask<group>();
        auto useLower = readLower(rawMask);
        auto useUpper = readUpper(rawMask);
        auto fullMask = mask(rawMask);
        auto lmask = lowerMask(rawMask);
        auto umask = upperMask(rawMask);
        auto computeAddress = [this]() {
            auto address = getAddressRegister() + _instruction.firstWord().getMemoryOffset();
            if (_instruction.firstWord().isIndirectOperation()) {
                address = encodeRegisterValue(loadWord(address + 1), loadWord(address));
            }
            return address;
        };
        auto loadOperation = [this, computeAddress, useLower, useUpper, fullMask]() {
			auto& value = getValueRegister();
			auto address = computeAddress();
			auto lower = useLower ? encodeLowerHalf(0, loadWord(address)) : 0;
			auto upper = useUpper ? encodeUpperHalf(0, loadWord(address + 1)) : 0;
			auto combinedValue = lower | upper;
			value = syn::encodeBits<RegisterValue, RegisterValue>(0, combinedValue, fullMask, 0);
        };
        auto storeOperation = [this, computeAddress, useLower, useUpper, lmask, umask]() {
            constexpr Word maskCheck = 0xFFFF;
            auto value = getValueRegister();
            auto address = computeAddress();
            if (useLower) {
                auto lowerValue = decodeLowerHalf(value);
                Word tmp = lowerValue;
                if (lmask != maskCheck) {
                    tmp = ((lmask & lowerValue) | (loadWord(address) & ~lmask));
                }
                storeWord(address, tmp);
            }
            if (useUpper) {
                auto newAddress = address + 1;
                // pull the upper 16 bits out into a separate variable
                auto upperValue = decodeUpperHalf(value);
                // by default, assume that we will store the top half of the value into memory
                auto tmp = upperValue;
                if (umask != maskCheck) {
                    // needs to be the masked value instead!
                    tmp = (umask & upperValue) | (loadWord(newAddress) & ~umask);
                }
                storeWord(newAddress, tmp);
            }
        };
        auto pushOperation = [this, useUpper, useLower, umask, lmask]() {
            if (_instruction.firstWord().isIndirectOperation()) {
                throw syn::Problem("Indirect bit not supported in push operations!");
            }
            // update the target stack to something different
            auto pushToStack = registerValue(_instruction.firstWord().getMemoryRegister());
            // read backwards because the stack grows upward towards zero
            if (useUpper) {
                pushWord(umask & decodeUpperHalf(pushToStack));
            }
            if (useLower) {
                pushWord(lmask & decodeLowerHalf(pushToStack));
            }
        };
        auto popOperation = [this, useUpper, useLower, lmask, umask]() {
            if (_instruction.firstWord().isIndirectOperation()) {
                throw syn::Problem("Indirect bit not supported in pop operations!");
            }
            auto lower = useLower ? lmask & popWord() : 0;
            auto upper = useUpper ? umask & popWord() : 0;
            registerValue(_instruction.firstWord().getMemoryRegister()) = encodeRegisterValue(upper, lower);
            // can't think of a case where we should
            // restore the instruction pointer and then
            // immediate advance so just don't do it
            advanceIp = _instruction.firstWord().getMemoryRegister() != ArchitectureConstants::InstructionPointer;
        };
        switch(_instruction.getSubtype<group>()) {
            case MemoryOperation::Load:
                loadOperation();
                break;
            case MemoryOperation::Store:
                storeOperation();
                break;
            case MemoryOperation::Push:
                pushOperation();
                break;
            case MemoryOperation::Pop:
                popOperation();
                break;
            default:
                throw syn::Problem("Illegal memory operation!");
        }
    }

    void CoreModel1::complexOperation() {
        auto type = _instruction.firstWord().getSubtype<Operation::Complex>();
        switch(type) {
            case ComplexSubTypes::Encoding:
                encodingOperation();
                break;
            case ComplexSubTypes::Extended:
                extendedOperation();
                break;
			case ComplexSubTypes::Parsing:
				parsingOperation();
				break;
			case ComplexSubTypes::FeatureCheck:
				featureCheckOperation();
				break;
            default:
                throw syn::Problem("Undefined complex subtype!");
        }
    }
    void CoreModel1::shiftOperation() {
        static constexpr auto group = Operation::Shift;
        auto &destination = registerValue(_instruction.getDestinationRegister<group>());
        auto source = (_instruction.isImmediate<group>() ? static_cast<RegisterValue>(_instruction.getImmediate<group>()) : registerValue(_instruction.getSourceRegister<group>()));
        auto direction = _instruction.firstWord().shouldShiftLeft() ?  ALUOperation::ShiftLeft : ALUOperation::ShiftRight;
        destination = syn::ALU::performOperation<RegisterValue>(direction, destination, source);
    }

    void CoreModel1::extendedOperation() {
		constexpr auto group = ComplexSubTypes::Extended;
		auto wordsBeforeFirstZero = [this]() {
			auto addr = getAddressRegister();
			auto count = 0;
			while(true) {
				if (loadWord(addr) == 0) {
					break;
				}
				++count;
				++addr;
			}
			getValueRegister() = count;
		};
		switch(_instruction.firstWord().getExtendedOperation()) {
			case ExtendedOperation::PopValueAddr:
				getValueRegister() = popRegisterValue();
				getAddressRegister() = popRegisterValue();
				break;
			case ExtendedOperation::PushValueAddr:
				pushRegisterValue(getAddressRegister());
				pushRegisterValue(getValueRegister());
				break;
			case ExtendedOperation::IsEven:
				getConditionRegister() = syn::isEven(registerValue(_instruction.firstWord().getDestinationRegister<group>()));
				break;
			case ExtendedOperation::IsOdd:
				getConditionRegister() = syn::isOdd(registerValue(_instruction.firstWord().getDestinationRegister<group>()));
				break;
			case ExtendedOperation::IncrementValueAddr:
				++getValueRegister();
				++getAddressRegister();
				break;
			case ExtendedOperation::DecrementValueAddr:
				--getValueRegister();
				--getAddressRegister();
				break;
			case ExtendedOperation::WordsBeforeFirstZero:
				wordsBeforeFirstZero();
				break;
			default:
				throw syn::Problem("Undefined extended operation!");
        }
    }
	void CoreModel1::parsingOperation() {
		switch(_instruction.firstWord().getParsingOperation()) {
			case ParsingOperation::Hex8ToRegister:
				hex8ToRegister();
				break;
			case ParsingOperation::RegisterToHex8:
				registerToHex8();
				break;
			case ParsingOperation::MemCopy:
				// the address in addr loaded from and stored into the address
				// contained within value
				storeWord(getValueRegister(), loadWord(getAddressRegister()));
				break;
			default:
				throw syn::Problem("Illegal parsing operation!");
		}
	}

    void CoreModel1::arithmeticOperation() {
        static constexpr auto group = Operation::Arithmetic;
        auto src1 = _instruction.isImmediate<group>() ? _instruction.getImmediate<group>() : registerValue(_instruction.getSourceRegister<group>());
        auto &src0 = registerValue(_instruction.getDestinationRegister<group>());
        auto subType = _instruction.getSubtype<group>();
        auto defaultArithmetic = [&src0, src1, subType]() {
            auto result = translate(subType);
            syn::throwOnErrorState(result, "Illegal arithmetic operation!");
            src0 = syn::ALU::performOperation<RegisterValue>(result, src0, src1);
        };
        switch(subType) {
            case ArithmeticOps::Min:
                getValueRegister() = src0 > src1 ? src1 : src0;
                break;
            case ArithmeticOps::Max:
                getValueRegister() = src0 > src1 ? src0 : src1;
                break;
            default:
                defaultArithmetic();
                break;
        }
    }

    void CoreModel1::logicalOperation() {
        static constexpr auto group = Operation::Logical;
        auto result = translate(_instruction.getSubtype<group>());
        syn::throwOnErrorState(result, "Illegal logical operation!");
        auto op = result;
        auto source1 = _instruction.isImmediate<group>() ? _instruction.retrieveImmediate<group>() : sourceRegister<group>();
        auto& dest = destinationRegister<group>();
        dest = syn::ALU::performOperation<RegisterValue>(op, dest, source1);
    }


    void CoreModel1::encodingOperation() {
        defaultEncodingOperation(_instruction.firstWord().getEncodingOperation());
    }

	bool CoreModel1::isTerminateAddress(RegisterValue address) const noexcept {
		return address == _terminateAddress;
	}

	void CoreModel1::featureCheckOperation() {
		switch (_instruction.firstWord().getFeatureCheckOperation()) {
			case FeatureCheckOperation::GetModelNumber:
				getValueRegister() = 1;
				break;
			case FeatureCheckOperation::GetTerminateAddress:
				getAddressRegister() = _terminateAddress;
				break;
			case FeatureCheckOperation::SetTerminateAddress:
				_terminateAddress = getValueRegister();
				break;
			default:
				throw syn::Problem("Undefined feature check operation!");
		}
	}
    void CoreModel1::FusedInstruction::reset(Word first, Word second, Word third) noexcept {
        _first = DecodedInstruction(first);
        _second = DecodedInstruction(second);
        _third = DecodedInstruction(third);
    }

    Operation CoreModel1::FusedInstruction::getControl() const noexcept {
        return _first.getControl();
    }

    RegisterValue CoreModel1::FusedInstruction::retrieveImmediate(byte bitmask) const noexcept {
        // the immediate cache will be in positions 1 and 2
        auto offset = 1;
        auto getWord = [this, &offset](auto cond, auto shift, const auto& ref) noexcept {
            if (cond) {
                ++_ip;
                auto result = ref.getRawValue();
                ++offset;
                return static_cast<RegisterValue>(result) << shift;
            } else {
                return static_cast<RegisterValue>(0);
            }
        };
        auto lower = getWord(readLower(bitmask), 0, _second);
        auto upper = getWord(readUpper(bitmask), 16, _third);
        return mask(bitmask) & (lower | upper);
    }
    CoreModel1::FusedInstruction::~FusedInstruction() { }
    CoreModel1::FusedInstruction::FusedInstruction(RegisterValue& ip, Word first, Word second, Word third) : _first(first), _second(second), _third(third), _ip(ip) { }
}
