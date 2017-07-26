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


#include "Cisc0CoreModel0.h"
#include <functional>
#include <sstream>
#include "Problem.h"
#include <utility>
#include <map>
#include "Cisc0ClipsExtensions.h"

namespace cisc0 {

    RegisterValue CoreModel0::retrieveImmediate(byte bitmask) noexcept {
        auto useLower = readLower(bitmask);
        auto useUpper = readUpper(bitmask);
        if (!useLower && !useUpper) {
            return 0;
        } else {
            auto lower = tryReadNext(useLower);
            auto upper = static_cast<RegisterValue>(tryReadNext(useUpper)) << 16;
            return mask(bitmask) & (lower | upper);
        }
    }

    CoreModel0::CoreModel0() noexcept : Parent("Cisc0CoreModel0IOBus.clp") { }
    CoreModel0::~CoreModel0() noexcept { }

    bool CoreModel0::cycle() {
        if (debugEnabled()) {
            std::cout << "Current Instruction Location: " << std::hex << getInstructionPointer() << std::endl;
            std::cout << "\tCurrent word value: " << std::hex << getCurrentCodeWord() << std::endl;
        }
        _first = getCurrentCodeWord();
        dispatch();
        if (advanceIp) {
            incrementInstructionPointer();
        } else {
            // just re-enable it
            advanceIp = true;
        }
        return execute;
    }


    void CoreModel0::dispatch() {
        auto tControl = _first.getControl();
        auto swapOperation = [this]() {
            constexpr auto group = Operation::Swap;
			// make sure that the control bits are always zero in this case so
			// that backwards compatibility isn't broken
            if (_first.getSubtypeControlBits() != 0) {
				throw syn::Problem("Control bits for swap must be zero!");
			}
            auto dInd = _first.getDestinationRegister<group>();
            auto sInd = _first.getSourceRegister<group>();
            if (dInd != sInd) {
                _gpr.swap(dInd, sInd);
            }
        };
        auto moveOperation = [this]() {
            constexpr auto group = Operation::Move;
            using T = RegisterValue;
            auto dInd = _first.getDestinationRegister<group>();
            auto source0 = registerValue(_first.getSourceRegister<group>());
            auto bmask = mask(_first.getBitmask<group>());
            registerValue(dInd) = syn::decodeBits<T, T>(source0, bmask, 0);
        };
        auto setOperation = [this] () {
            constexpr auto group = Operation::Set;
            auto dInd = _first.getDestinationRegister<group>();
            auto bmask = _first.getBitmask<group>();
            registerValue(dInd) = retrieveImmediate(bmask);
        };
        switch(tControl) {
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
                illegalInstruction(_first, getInstructionPointer());
                break;
        }
    }
    void CoreModel0::branchOperation() {
        static constexpr auto group = Operation::Branch;
        bool isCall, isCond;
        std::tie(isCall, isCond) = _first.getOtherBranchFlags();
        advanceIp = true;
        auto whereToGo = 0;
        if (_first.getImmediateFlag<group>()) {
            auto lower = static_cast<RegisterValue>(tryReadNext<true>());
            auto upper = static_cast<RegisterValue>(tryReadNext<true>()) << 16;
            whereToGo = lower | upper;
        } else {
            whereToGo = registerValue(_first.getDestinationRegister<group>());
        }
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

    void CoreModel0::compareOperation() {
        static constexpr auto group = Operation::Compare;
		auto compareResult = _first.getSubtype<group>();
		auto destinationIndex = _first.getDestinationRegister<group>();
        auto moveToCondition = [this, destinationIndex]() {
            getConditionRegister() = (registerValue(destinationIndex) != 0);
        };
        auto moveFromCondition = [this, destinationIndex]() {
            registerValue(destinationIndex) = normalizeCondition(getConditionRegister());
        };
        auto normalCompare = [this, destinationIndex, compareResult]() {
			auto first = registerValue(destinationIndex);
			auto second = _first.getImmediateFlag<group>() ? retrieveImmediate(_first.getBitmask<group>()) : registerValue(_first.getSourceRegister<group>());
			auto compareUnitType = translate(compareResult);
        	syn::throwOnErrorState(compareResult, "Illegal compare type!");
			getConditionRegister() = syn::Comparator::performOperation(compareUnitType, first, second);
        };
        switch(compareResult) {
            case CompareStyle::MoveToCondition:
                moveToCondition();
                break;
            case CompareStyle::MoveFromCondition:
                moveFromCondition();
                break;
            default:
                normalCompare();
                break;
        }
    }
    void CoreModel0::memoryOperation() {
        static constexpr auto group = Operation::Memory;
        auto rawMask = _first.getBitmask<group>();
        auto useLower = readLower(rawMask);
        auto useUpper = readUpper(rawMask);
        auto fullMask = mask(rawMask);
        auto lmask = lowerMask(rawMask);
        auto umask = upperMask(rawMask);
        auto computeAddress = [this]() {
            auto address = getAddressRegister() + _first.getMemoryOffset();
            if (_first.isIndirectOperation()) {
                address = encodeRegisterValue(loadWord(address + 1), loadWord(address));
            }
            return address;
        };
        auto loadOperation = [this, computeAddress, useLower, useUpper, fullMask]() {
            if (useLower || useUpper) {
                auto& value = getValueRegister();
                auto address = computeAddress();
                auto lower = useLower ? encodeLowerHalf(0, loadWord(address)) : 0;
                auto upper = useUpper ? encodeUpperHalf(0, loadWord(address + 1)) : 0;
                auto combinedValue = lower | upper;
                value = syn::encodeBits<RegisterValue, RegisterValue>(0, combinedValue, fullMask, 0);
            }
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
                    // needs to be the masked value _firstead!
                    tmp = (umask & upperValue) | (loadWord(newAddress) & ~umask);
                }
                storeWord(newAddress, tmp);
            }
        };
        auto pushOperation = [this, useUpper, useLower, umask, lmask]() {
            if (_first.isIndirectOperation()) {
                throw syn::Problem("Indirect bit not supported in push operations!");
            }
            // update the target stack to something different
            auto pushToStack = registerValue(_first.getDestination());
            // read backwards because the stack grows upward towards zero
            if (useUpper) {
                pushWord(umask & decodeUpperHalf(pushToStack));
            }
            if (useLower) {
                pushWord(lmask & decodeLowerHalf(pushToStack));
            }
        };
        auto popOperation = [this, useUpper, useLower, lmask, umask]() {
            if (_first.isIndirectOperation()) {
                throw syn::Problem("Indirect bit not supported in pop operations!");
            }
            auto lower = useLower ? lmask & popWord() : 0;
            auto upper = useUpper ? umask & popWord() : 0;
            registerValue(_first.getDestination()) = encodeRegisterValue(upper, lower);
            // can't think of a case where we should
            // restore the _firstruction pointer and then
            // immediate advance so just don't do it
            advanceIp = _first.getDestination() != ArchitectureConstants::InstructionPointer;
        };
        switch(_first.getSubtype<group>()) {
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

    void CoreModel0::shiftOperation() {
        static constexpr auto group = Operation::Shift;
        auto &destination = registerValue(_first.getDestinationRegister<group>());
        auto source = (_first.getImmediateFlag<group>() ? static_cast<RegisterValue>(_first.getImmediate<group>()) : registerValue(_first.getSourceRegister<group>()));
		auto direction = _first.shouldShiftLeft() ? ALUOperation::ShiftLeft : ALUOperation::ShiftRight;
        destination = syn::ALU::performOperation<RegisterValue>(direction, destination, source);
    }

    void CoreModel0::complexOperation() {
        auto type = _first.getSubtype<Operation::Complex>();
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

    void CoreModel0::extendedOperation() {
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
		switch(_first.getExtendedOperation()) {
			case ExtendedOperation::PopValueAddr:
				getValueRegister() = popRegisterValue();
				getAddressRegister() = popRegisterValue();
				break;
			case ExtendedOperation::PushValueAddr:
				pushRegisterValue(getAddressRegister());
				pushRegisterValue(getValueRegister());
				break;
			case ExtendedOperation::IsEven:
				getConditionRegister() = syn::isEven(registerValue(_first.getDestinationRegister<group>()));
				break;
			case ExtendedOperation::IsOdd:
				getConditionRegister() = syn::isOdd(registerValue(_first.getDestinationRegister<group>()));
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
	void CoreModel0::parsingOperation() {
		switch(_first.getParsingOperation()) {
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

    void CoreModel0::arithmeticOperation() {
        static constexpr auto group = Operation::Arithmetic;
        auto src1 = _first.getImmediateFlag<group>() ? _first.getImmediate<group>() : registerValue(_first.getSourceRegister<group>());
        auto &src0 = registerValue(_first.getDestinationRegister<group>());
        auto minOp = [this, &src0, src1]() {
            getValueRegister() = src0 > src1 ? src1 : src0;
        };
        auto maxOp = [this, &src0, src1]() {
            getValueRegister() = src0 > src1 ? src0 : src1;
        };
        auto defaultArithmetic = [this, &src0, src1]() {
            auto result = translate(_first.getSubtype<group>());
            syn::throwOnErrorState(result, "Illegal arithmetic operation!");
            auto src = src1;
            auto& dest = src0;
            dest = syn::ALU::performOperation<RegisterValue>(result, dest, src);
        };
        switch(_first.getSubtype<group>()) {
            case ArithmeticOps::Min:
                minOp();
                break;
            case ArithmeticOps::Max:
                maxOp();
                break;
            default:
                defaultArithmetic();
                break;
        }
    }

    void CoreModel0::logicalOperation() {
        static constexpr auto group = Operation::Logical;
        auto result = translate(_first.getSubtype<group>());
        syn::throwOnErrorState(result, "Illegal logical operation!");
        auto op = result;
        auto source1 = _first.getImmediateFlag<group>() ? retrieveImmediate(_first.getBitmask<group>()) : registerValue(_first.getSourceRegister<group>());
        auto& dest = registerValue(_first.getDestinationRegister<group>());
        dest = syn::ALU::performOperation<RegisterValue>(op, dest, source1);
    }

    void CoreModel0::encodingOperation() {
        defaultEncodingOperation(_first.getEncodingOperation());
    }

    Word CoreModel0::getCurrentCodeWord() {
        return loadWord(getInstructionPointer());
    }
    Word CoreModel0::tryReadNext(bool readNext) {
        return readNext ? tryReadNext<true>() : tryReadNext<false>();
    }
}
