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
        auto useLower = readLower(bitmask);
        auto useUpper = readUpper(bitmask);
        if (!useLower && !useUpper) {
            return 0;
        } else {
            auto offset = 1;
            auto lower = 0;
            auto upper = 0;
            if (useLower) {
                incrementInstructionPointer();
                lower = static_cast<RegisterValue>(_instruction[offset].getRawValue());
                ++offset;
            }
            if (useUpper) {
                incrementInstructionPointer();
                upper = static_cast<RegisterValue>(_instruction[offset].getRawValue()) << 16;
                ++offset;
            }
            return mask(bitmask) & (lower | upper);
        }
    }

    CoreModel1::CoreModel1() noexcept : _bus(0x00000000, 0xFFFFFFFF, "Cisc0CoreModel1IOBus.clp")  { }
    CoreModel1::~CoreModel1() noexcept { }

    void CoreModel1::initialize() {
        cisc0::installAssemblerParsingState(_bus.getRawEnvironment());
		_gpr.initialize();
		_bus.initialize();
		getInstructionPointer() = ArchitectureConstants::StartingIPAddress;
    }

    void CoreModel1::shutdown() {
		_bus.shutdown();
        _gpr.shutdown();
	}

    bool CoreModel1::cycle() {
        if (debugEnabled()) {
            std::cout << "Current Instruction Location: " << std::hex << getInstructionPointer() << std::endl;
            std::cout << "\tCurrent word value: " << std::hex << getCurrentCodeWord() << std::endl;
        }
        for (auto i = 0; i < 4; ++i) {
            _instruction[i] = getCurrentCodeWord(i);
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
        auto& current = _instruction[0];
        auto tControl = current.getControl();
        auto swapOperation = [this, &current]() noexcept {
            constexpr auto group = Operation::Swap;
            auto dInd = current.getDestinationRegister<group>();
            auto sInd = current.getSourceRegister<group>();
            if (dInd != sInd) {
                _gpr.swap(dInd, sInd);
            }
        };
        auto returnOperation = [this]() noexcept {
            // pop the top address off of the call stack and place it in
            // the instruction pointer
            getInstructionPointer() = popRegisterValue(getCallStackPointer());
            advanceIp = false;
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
        switch(tControl) {
            case Operation::Shift:
                shiftOperation((current));
                break;
            case Operation::Arithmetic:
                arithmeticOperation((current));
                break;
            case Operation::Logical:
                logicalOperation((current));
                break;
            case Operation::Memory:
                memoryOperation((current));
                break;
            case Operation::Branch:
                branchOperation();
                break;
            case Operation::Compare:
                compareOperation();
                break;
            case Operation::Complex:
                complexOperation((current));
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
        auto& inst = _instruction[0];
        bool isCall, isCond;
        std::tie(isCall, isCond) = inst.getOtherBranchFlags();
        advanceIp = true;
        auto whereToGo = 0;
        if (inst.getImmediateFlag<Operation::Branch>()) {
            incrementInstructionPointer();
            incrementInstructionPointer();
            auto lower = static_cast<RegisterValue>(_instruction[1].getRawValue());
            auto upper = static_cast<RegisterValue>(_instruction[2].getRawValue()) << 16;
            whereToGo = lower | upper;
        } else {
            whereToGo = registerValue(inst.getBranchIndirectDestination());
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

    void CoreModel1::moveToCondition(byte index) noexcept {
        conditionRegister = registerValue(index) != 0;
    }
    void CoreModel1::moveFromCondition(byte index) noexcept {
        registerValue(index) = normalizeCondition(conditionRegister);
    }

    void CoreModel1::compareOperation() {
        auto& inst = _instruction[0];
        static constexpr auto group = Operation::Compare;
		auto compareResult = inst.getSubtype<group>();
		auto destinationIndex = inst.getDestinationRegister<group>();
        auto normalCompare = [this, destinationIndex, &inst, compareResult]() {
			auto first = registerValue(destinationIndex);
			auto second = inst.getImmediateFlag<group>() ? retrieveImmediate(inst.getBitmask<group>()) : registerValue(inst.getSourceRegister<group>());
			auto compareUnitType = translate(compareResult);
        	syn::throwOnErrorState(compareResult, "Illegal compare type!");
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
    void CoreModel1::memoryOperation(const DecodedInstruction& inst) {
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
            if (!useLower && !useUpper) {
                value = 0;
            } else {
                auto address = computeAddress();
                auto lower = useLower ? encodeLowerHalf(0, loadWord(address)) : 0;
                auto upper = useUpper ? encodeUpperHalf(0, loadWord(address + 1)) : 0;
                auto combinedValue = lower | upper;
                value = syn::encodeBits<RegisterValue, RegisterValue>(0, combinedValue, fullMask, 0);
            }
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

    void CoreModel1::complexOperation(const DecodedInstruction& inst) {
        auto type = inst.getSubtype<Operation::Complex>();
        switch(type) {
            case ComplexSubTypes::Encoding:
                encodingOperation((inst));
                break;
            case ComplexSubTypes::Extended:
                extendedOperation((inst));
                break;
			case ComplexSubTypes::Parsing:
				parsingOperation((inst));
				break;
            default:
                throw syn::Problem("Undefined complex subtype!");
        }
    }
    void CoreModel1::shiftOperation(const DecodedInstruction& inst) {
        static constexpr auto group = Operation::Shift;
        auto &destination = registerValue(inst.getDestinationRegister<group>());
        auto source = (inst.getImmediateFlag<group>() ? static_cast<RegisterValue>(inst.getImmediate<group>()) : registerValue(inst.getSourceRegister<group>()));
		auto direction = inst.shouldShiftLeft() ? ALUOperation::ShiftLeft : ALUOperation::ShiftRight;
        destination = syn::ALU::performOperation<RegisterValue>(direction, destination, source);
    }

    void CoreModel1::extendedOperation(const DecodedInstruction& inst) {
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
	void CoreModel1::parsingOperation(const DecodedInstruction& inst) {
		auto hex8ToRegister = [this, &inst]() {
			// 1) use the address contained in address to read the next 8 words
			// 2) Parse each word as an ascii character and convert it into a 4 bit quantity
			// 3) Place that 4bit quantity into the appropriate position in value
			auto addr = getAddressRegister();
			auto value = syn::encodeBits<RegisterValue, byte, 0x0000000F, 0>(0, convertTextToHex(loadWord(addr)));
			value = syn::encodeBits<RegisterValue, byte, 0x000000F0, 4>(value, convertTextToHex(loadWord(addr + 1)));
			value = syn::encodeBits<RegisterValue, byte, 0x00000F00, 8>(value, convertTextToHex(loadWord(addr + 2)));
			value = syn::encodeBits<RegisterValue, byte, 0x0000F000, 12>(value, convertTextToHex(loadWord(addr + 3)));
			value = syn::encodeBits<RegisterValue, byte, 0x000F0000, 16>(value, convertTextToHex(loadWord(addr + 4)));
			value = syn::encodeBits<RegisterValue, byte, 0x00F00000, 20>(value, convertTextToHex(loadWord(addr + 5)));
			value = syn::encodeBits<RegisterValue, byte, 0x0F000000, 24>(value, convertTextToHex(loadWord(addr + 6)));
			getValueRegister() = syn::encodeBits<RegisterValue, byte, 0xF0000000, 28>(value, convertTextToHex(loadWord(addr + 7)));
		};
		auto registerToHex8 = [this, &inst]() {
			auto addr = getAddressRegister();
			auto value = getValueRegister();
			storeWord(addr + 0, extractHexAndConvertToText<0x0000000F, 0>(value));
			storeWord(addr + 1, extractHexAndConvertToText<0x000000F0, 4>(value));
			storeWord(addr + 2, extractHexAndConvertToText<0x00000F00, 8>(value));
			storeWord(addr + 3, extractHexAndConvertToText<0x0000F000, 12>(value));
			storeWord(addr + 4, extractHexAndConvertToText<0x000F0000, 16>(value));
			storeWord(addr + 5, extractHexAndConvertToText<0x00F00000, 20>(value));
			storeWord(addr + 6, extractHexAndConvertToText<0x0F000000, 24>(value));
			storeWord(addr + 7, extractHexAndConvertToText<0xF0000000, 28>(value));
		};
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

    void CoreModel1::arithmeticOperation(const DecodedInstruction& inst) {
        static constexpr auto group = Operation::Arithmetic;
        auto src1 = inst.getImmediateFlag<group>() ? inst.getImmediate<group>() : registerValue(inst.getSourceRegister<group>());
        auto &src0 = registerValue(inst.getDestinationRegister<group>());
        auto minOp = [this, &src0, src1]() {
            getValueRegister() = src0 > src1 ? src1 : src0;
        };
        auto maxOp = [this, &src0, src1]() {
            getValueRegister() = src0 > src1 ? src0 : src1;
        };
        auto defaultArithmetic = [this, &inst, &src0, src1]() {
            auto result = translate(inst.getSubtype<group>());
            syn::throwOnErrorState(result, "Illegal arithmetic operation!");
            auto src = src1;
            auto& dest = src0;
            dest = syn::ALU::performOperation<RegisterValue>(result, dest, src);
        };
        switch(inst.getSubtype<group>()) {
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

    void CoreModel1::logicalOperation(const DecodedInstruction& inst) {
        static constexpr auto group = Operation::Logical;
        auto result = translate(inst.getSubtype<group>());
        syn::throwOnErrorState(result, "Illegal logical operation!");
        auto op = result;
        auto source1 = inst.getImmediateFlag<group>() ? retrieveImmediate(inst.getBitmask<group>()) : registerValue(inst.getSourceRegister<group>());
        auto& dest = registerValue(inst.getDestinationRegister<group>());
        dest = syn::ALU::performOperation<RegisterValue>(op, dest, source1);
    }
    template<syn::Comparator::StandardOperations op>
    constexpr RegisterValue sliceBitAndCheck(RegisterValue a, RegisterValue b) noexcept {
        using T = RegisterValue;
        return syn::Comparator::performOperation<op, T>(
                syn::ALU::performOperation<translate(LogicalOps::And), T>(
                    syn::ALU::performOperation<ALUOperation::ShiftRight, T>(
                        a,
                        b), 0x1), 1);
    }


    void CoreModel1::encodingOperation(const DecodedInstruction& inst) {
        switch(inst.getEncodingOperation()) {
            case EncodingOperation::Decode:
                // connect the result of the logical operations alu to the
                // shifter alu then store the result in the value register
                getValueRegister() = syn::decodeBits<RegisterValue, RegisterValue>(getAddressRegister(), getMaskRegister(), getShiftRegister());
                break;
            case EncodingOperation::Encode:
                getAddressRegister() = syn::encodeBits<RegisterValue, RegisterValue>(getAddressRegister(), getValueRegister(), getMaskRegister(), getShiftRegister());
                break;
            case EncodingOperation::BitSet:
                getConditionRegister() = sliceBitAndCheck<syn::Comparator::StandardOperations::Eq>(getAddressRegister(), getFieldRegister());
                break;
            case EncodingOperation::BitUnset:
                getConditionRegister() = sliceBitAndCheck<syn::Comparator::StandardOperations::Neq>(getAddressRegister(), getFieldRegister());
                break;
            default:
                throw syn::Problem("Illegal complex encoding operation defined!");
        }
    }

    RegisterValue& CoreModel1::registerValue(byte index) {
        return _gpr[index];
    }
    Word CoreModel1::getCurrentCodeWord(int offset) {
        return loadWord(getInstructionPointer() + offset);
    }
    void CoreModel1::tryReadNext(bool readNext) {
        return readNext ? tryReadNext<true>() : tryReadNext<false>();
    }
    void CoreModel1::storeWord(RegisterValue address, Word value) {
		if (address == ArchitectureConstants::TerminateAddress) {
			execute = false;
			advanceIp = false;
		} else {
			_bus.write(address, value);
		}
    }
    Word CoreModel1::loadWord(RegisterValue address) {
		return _bus.read(address);
    }
}
