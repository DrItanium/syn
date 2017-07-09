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
	RegisterValue CoreModel1::retrieveImmediate(byte bitmask) noexcept {
		// the immediate cache will be in positions 1 and 2
		auto offset = 1;
		auto getWord = [this, &offset](auto cond, auto shift) noexcept {
			if (cond) {
				incrementInstructionPointer();
				auto result = _instruction[offset].getRawValue();
				++offset;
				return static_cast<RegisterValue>(result) << shift;
			} else {
				return static_cast<RegisterValue>(0);
			}
		};
		auto lower = getWord(readLower(bitmask), 0);
		auto upper = getWord(readUpper(bitmask), 16);
		return mask(bitmask) & (lower | upper);
	}

    CoreModel1::CoreModel1() noexcept : Parent("Cisc0CoreModel1IOBus.clp")  { }
    CoreModel1::~CoreModel1() noexcept { }

    void CoreModel1::initialize() {
        Parent::initialize();
		_gpr.initialize();
		getInstructionPointer() = ArchitectureConstants::StartingIPAddress;
    }

    void CoreModel1::shutdown() {
        Parent::shutdown();
        _gpr.shutdown();
	}

    bool CoreModel1::cycle() {
        for (auto i = 0; i < instructionCacheWidth; ++i) {
            _instruction[i] = loadWord(getInstructionPointer() + i);
        }
        if (debugEnabled()) {
            std::cout << "Current Instruction Location: " << std::hex << getInstructionPointer() << std::endl;
            std::cout << "\tCurrent word value: " << std::hex << firstWord().getRawValue() << std::endl;
        }
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
        const auto& current = firstWord();
        auto swapOperation = [this, &current]() noexcept {
            constexpr auto group = Operation::Swap;
            auto dInd = current.getDestinationRegister<group>();
            auto sInd = current.getSourceRegister<group>();
            if (dInd != sInd) {
                _gpr.swap(dInd, sInd);
            }
        };
        auto moveOperation = [this,&current]() {
            constexpr auto group = Operation::Move;
            using T = RegisterValue;
            auto dInd = current.getDestinationRegister<group>();
            auto source0 = registerValue(current.getSourceRegister<group>());
            auto bmask = mask(current.getBitmask<group>());
            registerValue(dInd) = syn::decodeBits<T, T>(source0, bmask, 0);
        };
        auto setOperation = [this, &current]() {
            constexpr auto group = Operation::Set;
            auto dInd = current.getDestinationRegister<group>();
            auto bmask = current.getBitmask<group>();
            registerValue(dInd) = retrieveImmediate(bmask);
        };
        switch(current.getControl()) {
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
                illegalInstruction(current, getInstructionPointer());
                break;
        }
    }
    void CoreModel1::branchOperation() {
		static constexpr auto group = Operation::Branch;
        const auto& inst = firstWord();
        bool isCall, isCond;
        std::tie(isCall, isCond) = inst.getOtherBranchFlags();
        advanceIp = true;
		auto whereToGo = inst.getImmediateFlag<group>() ? retrieveImmediate(0b1111) : registerValue(inst.getBranchIndirectDestination());
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
        conditionRegister = registerValue(index) != 0;
    }
    void CoreModel1::moveFromCondition(byte index) noexcept {
        registerValue(index) = normalizeCondition(conditionRegister);
    }

    void CoreModel1::compareOperation() {
        const auto& inst = firstWord();
        static constexpr auto group = Operation::Compare;
		auto compareResult = inst.getSubtype<group>();
		auto destinationIndex = inst.getDestinationRegister<group>();
        auto normalCompare = [this, destinationIndex, &inst, compareResult]() {
			auto compareUnitType = translate(compareResult);
        	syn::throwOnErrorState(compareResult, "Illegal compare type!");
			auto first = registerValue(destinationIndex);
			auto second = inst.getImmediateFlag<group>() ? retrieveImmediate(inst.getBitmask<group>()) : registerValue(inst.getSourceRegister<group>());
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
        const auto& inst = firstWord();
        static constexpr auto group = Operation::Memory;
        auto rawMask = inst.getBitmask<group>();
        auto useLower = readLower(rawMask);
        auto useUpper = readUpper(rawMask);
        auto fullMask = mask(rawMask);
        auto lmask = lowerMask(rawMask);
        auto umask = upperMask(rawMask);
        auto computeAddress = [this, &inst]() {
            auto address = getAddressRegister() + inst.getMemoryOffset();
            if (inst.isIndirectOperation()) {
                address = encodeRegisterValue(loadWord(address + 1), loadWord(address));
            }
            return address;
        };
        auto loadOperation = [this, computeAddress, useLower, useUpper, fullMask, &inst]() {
			auto& value = getValueRegister();
			auto address = computeAddress();
			auto lower = useLower ? encodeLowerHalf(0, loadWord(address)) : 0;
			auto upper = useUpper ? encodeUpperHalf(0, loadWord(address + 1)) : 0;
			auto combinedValue = lower | upper;
			value = syn::encodeBits<RegisterValue, RegisterValue>(0, combinedValue, fullMask, 0);
        };
        auto storeOperation = [this, computeAddress, useLower, useUpper, lmask, umask, &inst]() {
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
        auto pushOperation = [this, useUpper, useLower, umask, lmask, &inst]() {
            if (inst.isIndirectOperation()) {
                throw syn::Problem("Indirect bit not supported in push operations!");
            }
            // update the target stack to something different
            auto pushToStack = registerValue(inst.getMemoryRegister());
            // read backwards because the stack grows upward towards zero
            if (useUpper) {
                pushWord(umask & decodeUpperHalf(pushToStack));
            }
            if (useLower) {
                pushWord(lmask & decodeLowerHalf(pushToStack));
            }
        };
        auto popOperation = [this, useUpper, useLower, lmask, umask, &inst]() {
            if (inst.isIndirectOperation()) {
                throw syn::Problem("Indirect bit not supported in pop operations!");
            }
            auto lower = useLower ? lmask & popWord() : 0;
            auto upper = useUpper ? umask & popWord() : 0;
            registerValue(inst.getMemoryRegister()) = encodeRegisterValue(upper, lower);
            // can't think of a case where we should
            // restore the instruction pointer and then
            // immediate advance so just don't do it
            advanceIp = inst.getMemoryRegister() != ArchitectureConstants::InstructionPointer;
        };
        switch(inst.getSubtype<group>()) {
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
        auto type = firstWord().getSubtype<Operation::Complex>();
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
            default:
                throw syn::Problem("Undefined complex subtype!");
        }
    }
    void CoreModel1::shiftOperation() {
        const auto& inst = firstWord();
        static constexpr auto group = Operation::Shift;
        auto &destination = registerValue(inst.getDestinationRegister<group>());
        auto source = (inst.getImmediateFlag<group>() ? static_cast<RegisterValue>(inst.getImmediate<group>()) : registerValue(inst.getSourceRegister<group>()));
		auto direction = inst.shouldShiftLeft() ? ALUOperation::ShiftLeft : ALUOperation::ShiftRight;
        destination = syn::ALU::performOperation<RegisterValue>(direction, destination, source);
    }

    void CoreModel1::extendedOperation() {
        const auto& inst = firstWord();
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
		switch(inst.getExtendedOperation()) {
			case ExtendedOperation::PopValueAddr:
				getValueRegister() = popRegisterValue();
				getAddressRegister() = popRegisterValue();
				break;
			case ExtendedOperation::PushValueAddr:
				pushRegisterValue(getAddressRegister());
				pushRegisterValue(getValueRegister());
				break;
			case ExtendedOperation::IsEven:
				getConditionRegister() = syn::isEven(registerValue(inst.getDestinationRegister<group>()));
				break;
			case ExtendedOperation::IsOdd:
				getConditionRegister() = syn::isOdd(registerValue(inst.getDestinationRegister<group>()));
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
        const auto& inst = firstWord();
		switch(inst.getParsingOperation()) {
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
        const auto& inst = firstWord();
        auto src1 = inst.getImmediateFlag<group>() ? inst.getImmediate<group>() : registerValue(inst.getSourceRegister<group>());
        auto &src0 = registerValue(inst.getDestinationRegister<group>());
        auto defaultArithmetic = [&src0, src1, subType = inst.getSubtype<group>()]() {
            auto result = translate(subType);
            syn::throwOnErrorState(result, "Illegal arithmetic operation!");
            src0 = syn::ALU::performOperation<RegisterValue>(result, src0, src1);
        };
        switch(inst.getSubtype<group>()) {
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
        const auto& inst = firstWord();
        static constexpr auto group = Operation::Logical;
        auto result = translate(inst.getSubtype<group>());
        syn::throwOnErrorState(result, "Illegal logical operation!");
        auto op = result;
        auto source1 = inst.getImmediateFlag<group>() ? retrieveImmediate(inst.getBitmask<group>()) : registerValue(inst.getSourceRegister<group>());
        auto& dest = registerValue(inst.getDestinationRegister<group>());
        dest = syn::ALU::performOperation<RegisterValue>(op, dest, source1);
    }


    void CoreModel1::encodingOperation() {
        defaultEncodingOperation(firstWord().getEncodingOperation());
    }

    RegisterValue& CoreModel1::registerValue(byte index) {
        return _gpr[index];
    }
}
