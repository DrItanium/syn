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


#include "Cisc0Core.h"
#include <functional>
#include <sstream>
#include "Problem.h"
#include <utility>
#include <map>
#include "Cisc0ClipsExtensions.h"

namespace cisc0 {
	constexpr Word lowerMask(byte bitmask) noexcept {
		return syn::encodeUint16LE(syn::expandBit(syn::getBit<byte, 0>(bitmask)),
									syn::expandBit(syn::getBit<byte, 1>(bitmask)));
	}
	constexpr Word upperMask(byte bitmask) noexcept {
		return syn::encodeUint16LE(syn::expandBit(syn::getBit<byte, 2>(bitmask)),
									syn::expandBit(syn::getBit<byte, 3>(bitmask)));
	}

	constexpr RegisterValue mask(byte bitmask) noexcept {
		return syn::encodeUint32LE(lowerMask(bitmask), upperMask(bitmask));
	}

	constexpr bool readLower(byte bitmask) noexcept {
		return lowerMask(bitmask) != 0;
	}

	constexpr bool readUpper(byte bitmask) noexcept {
		return upperMask(bitmask) != 0;
	}
    constexpr RegisterValue encodeRegisterValue(byte a, byte b, byte c, byte d) noexcept {
        return syn::encodeUint32LE(a, b, c, d);
    }
    constexpr Word encodeWord(byte a, byte b) noexcept {
        return syn::encodeUint16LE(a, b);
    }

    constexpr Word decodeUpperHalf(RegisterValue value) noexcept {
        return syn::decodeBits<RegisterValue, Word, mask(0b1100), 16>(value);
    }
    constexpr Word decodeLowerHalf(RegisterValue value) noexcept {
        return syn::decodeBits<RegisterValue, Word, mask(0b0011), 0>(value);
    }

    constexpr RegisterValue encodeUpperHalf(RegisterValue value, Word upperHalf) noexcept {
        return syn::encodeBits<RegisterValue, Word, mask(0b1100), 16>(value, upperHalf);
    }
    constexpr RegisterValue encodeLowerHalf(RegisterValue value, Word lowerHalf) noexcept {
        return syn::encodeBits<RegisterValue, Word, mask(0b0011), 0>(value, lowerHalf);
    }

    constexpr RegisterValue encodeRegisterValue(Word upper, Word lower) noexcept {
        if (upper == 0 && lower == 0) {
            return 0;
        }
        return encodeUpperHalf(encodeLowerHalf(0, lower), upper);
    }
    constexpr RegisterValue normalizeCondition(RegisterValue input) noexcept {
        return input != 0 ? 0xFFFFFFFF : 0x00000000;
    }

    RegisterValue Core::retrieveImmediate(byte bitmask) noexcept {
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

    Core::Core() noexcept : _bus(0x00000000, 0xFFFFFFFF, "Cisc0CoreIOBus.clp")  { }
    Core::~Core() noexcept { }

    void Core::initialize() {
        cisc0::installAssemblerParsingState(_bus.getRawEnvironment());
		gpr.initialize();
		_bus.initialize();
		getInstructionPointer() = ArchitectureConstants::StartingIPAddress;
    }

    void Core::shutdown() {
		_bus.shutdown();
	}

    bool Core::cycle() {
        if (debugEnabled()) {
            std::cout << "Current Instruction Location: " << std::hex << getInstructionPointer() << std::endl;
            std::cout << "\tCurrent word value: " << std::hex << getCurrentCodeWord() << std::endl;
        }
        DecodedInstruction di(getCurrentCodeWord());
        dispatch(std::move(di));
        if (advanceIp) {
            incrementInstructionPointer();
        } else {
            // just re-enable it
            advanceIp = true;
        }
        return execute;
    }
    void Core::incrementAddress(RegisterValue& ptr) noexcept {
        ++ptr;
    }
    void Core::decrementAddress(RegisterValue& ptr) noexcept {
        --ptr;
    }
    void Core::incrementInstructionPointer() noexcept {
        incrementAddress(getInstructionPointer());
    }

    void illegalInstruction(DecodedInstruction&& current, RegisterValue ip) {
        std::stringstream str;
        str << "Illegal instruction " << std::hex << static_cast<int>(current.getControl()) << std::endl;
        str << "Location: " << std::hex << ip << std::endl;
        auto s = str.str();
        throw syn::Problem(s);
    }

    void Core::dispatch(DecodedInstruction&& current) {
        auto tControl = current.getControl();
        switch(tControl) {
            case Operation::Shift:
                shiftOperation(std::move(current));
                break;
            case Operation::Arithmetic:
                arithmeticOperation(std::move(current));
                break;
            case Operation::Logical:
                logicalOperation(std::move(current));
                break;
            case Operation::Memory:
                memoryOperation(std::move(current));
                break;
            case Operation::Branch:
                branchOperation(std::move(current));
                break;
            case Operation::Compare:
                compareOperation(std::move(current));
                break;
            case Operation::Complex:
                complexOperation(std::move(current));
                break;
            case Operation::Swap:
                if (current.getDestinationRegister<Operation::Swap>() != current.getSourceRegister<Operation::Swap>()) {
                    gpr.swap(current.getDestinationRegister<Operation::Swap>(), current.getSourceRegister<Operation::Swap>());
                }
                break;
            case Operation::Move:
                registerValue(current.getDestinationRegister<Operation::Move>()) = syn::decodeBits<RegisterValue, RegisterValue>( registerValue(current.getSourceRegister<Operation::Move>()), mask(current.getBitmask<Operation::Move>()), 0);
                break;
            case Operation::Set:
                registerValue(current.getSetDestination()) = retrieveImmediate(current.getBitmask<Operation::Set>());
                break;
            default:
                execute = false;
                illegalInstruction(std::move(current), getInstructionPointer());
                break;
        }
    }
    void Core::branchOperation(DecodedInstruction&& inst) {
        auto isImm = inst.getImmediateFlag<Operation::Branch>();
        bool isCall, isCond;
        std::tie(isCall, isCond) = inst.getOtherBranchFlags();
        advanceIp = true;
        auto choice = getConditionRegister() != 0;
        auto readAddress = [this]() {
            auto lower = static_cast<RegisterValue>(tryReadNext<true>());
            auto upper = static_cast<RegisterValue>(tryReadNext<true>()) << 16;
            return lower | upper;
        };
        auto updateInstructionPointer = [this](auto value) {
            advanceIp = false;
            getInstructionPointer() = value;
        };
        if (isCall) {
            // call instruction
            // figure out where we are going to go, this will cause loads and
            // incrementation of the instruction pointer.
            // Once done, we then push the next address following the newly
            // modified ip to the stack. Then we update the ip of where we are
            // going to go!
            auto whereToGo = isImm ? readAddress() : registerValue(inst.getBranchIndirectDestination());
            pushDword(getInstructionPointer() + 1);
            updateInstructionPointer(whereToGo);
        } else {
            // jump instruction
            auto whereToGo = isImm ? readAddress() : registerValue(inst.getBranchIndirectDestination());
            if (isCond) {
                if (choice) {
                    updateInstructionPointer(whereToGo);
                }
            } else {
                updateInstructionPointer(whereToGo);
            }
        }
    }

    void Core::compareOperation(DecodedInstruction&& inst) {
        static constexpr auto group = Operation::Compare;
        auto compareResult = translate(inst.getSubtype<group>());
        syn::throwOnErrorState(compareResult, "Illegal compare type!");
        DecodedInstruction next(tryReadNext<true>());
        auto first = registerValue(next.getDestinationRegister<group>());
        auto isImm = inst.getImmediateFlag<group>();
        auto second = isImm ? next.getUpper() : registerValue(next.getSourceRegister<group>());
        auto result = syn::Comparator::performOperation(compareResult, first, second);
        // make sure that the condition takes up the entire width of the
        // register, that way normal operations will make sense!
        getConditionRegister() = normalizeCondition(result);
    }

    void Core::memoryOperation(DecodedInstruction&& inst) {
        static constexpr auto group = Operation::Memory;
        auto rawMask = inst.getBitmask<group>();
        auto useLower = readLower(rawMask);
        auto useUpper = readUpper(rawMask);
        auto fullMask = mask(rawMask);
        auto rawType = inst.getSubtype<group>();
        auto memoryRegister = inst.getMemoryRegister();
        auto lmask = lowerMask(rawMask);
        auto umask = upperMask(rawMask);
        auto upper = 0u;
        auto lower = 0u;
        auto loadIndirectAddress = [this](auto address) {
			return encodeRegisterValue(loadWord(address + 1), loadWord(address));
        };
        if (rawType == MemoryOperation::Load) {
            auto& value = getValueRegister();
            if (!useLower && !useUpper) {
                value = 0;
            } else {
                auto address = getAddressRegister() + inst.getMemoryOffset();
                if (inst.isIndirectOperation()) {
                    address = loadIndirectAddress(address);
                }
                if (useLower) {
                    lower = encodeLowerHalf(0, loadWord(address));
                }
                if (useUpper) {
                    upper = encodeUpperHalf(0, loadWord(address + 1));
                }
                auto combinedValue = lower | upper;
                value = syn::encodeBits<RegisterValue, RegisterValue>(0u, combinedValue, fullMask, 0);
            }
        } else if (rawType == MemoryOperation::Store) {
            static constexpr Word maskCheck = 0xFFFF;
            auto value = getValueRegister();
            auto address = getAddressRegister() + inst.getMemoryOffset();
            if (inst.isIndirectOperation()) {
                address = loadIndirectAddress(address);
            }
            if (useLower) {
                auto lowerValue = decodeLowerHalf(value);
                if (lmask == maskCheck) {
                    storeWord(address, lowerValue);
                } else {
                    storeWord(address, (lmask & lowerValue) | (loadWord(address) & ~lmask));
                }
            }
            if (useUpper) {
                auto newAddress = address + 1;
                // pull the upper 16 bits out into a separate variable
                auto upperValue = decodeUpperHalf(value);
                if (umask == maskCheck) {
                    // store the top half of the value into memory
                    storeWord(newAddress, upperValue);
                } else {
                    // needs to be the masked value instead!
                    storeWord(newAddress, (umask & upperValue) | (loadWord(newAddress) & ~umask));
                }
            }
        } else if (rawType == MemoryOperation::Push) {
            if (inst.isIndirectOperation()) {
                throw syn::Problem("Indirect bit not supported in push operations!");
            }
            // update the target stack to something different
            auto pushToStack = registerValue(memoryRegister);
            // read backwards because the stack grows upward towards zero
            if (useUpper) {
                pushWord(umask & decodeUpperHalf(pushToStack));
            }
            if (useLower) {
                pushWord(lmask & decodeLowerHalf(pushToStack));
            }
        } else if (rawType == MemoryOperation::Pop) {
            if (inst.isIndirectOperation()) {
                throw syn::Problem("Indirect bit not supported in pop operations!");
            }
            if (useLower) {
                lower = lmask & popWord();
            }
            if (useUpper) {
                upper = umask & popWord();
            }
            registerValue(memoryRegister) = encodeRegisterValue(upper, lower);
            // can't think of a case where we should
            // restore the instruction pointer and then
            // immediate advance so just don't do it
            advanceIp = memoryRegister != ArchitectureConstants::InstructionPointer;
        } else {
            throw syn::Problem("Illegal memory operation!");
        }

    }

    void Core::complexOperation(DecodedInstruction&& inst) {
        auto type = inst.getSubtype<Operation::Complex>();
        switch(type) {
            case ComplexSubTypes::Encoding:
                encodingOperation(std::move(inst));
                break;
            case ComplexSubTypes::Extended:
                extendedOperation(std::move(inst));
                break;
			case ComplexSubTypes::Parsing:
				parsingOperation(std::move(inst));
				break;
            default:
                throw syn::Problem("Undefined complex subtype!");
        }
    }
    void Core::shiftOperation(DecodedInstruction&& inst) {
        static constexpr auto group = Operation::Shift;
        auto &destination = registerValue(inst.getDestinationRegister<group>());
        auto source = (inst.getImmediateFlag<group>() ? static_cast<RegisterValue>(inst.getImmediate<group>()) : registerValue(inst.getSourceRegister<group>()));
		auto direction = inst.shouldShiftLeft() ? ALUOperation::ShiftLeft : ALUOperation::ShiftRight;
        destination = syn::ALU::performOperation<RegisterValue>(direction, destination, source);
    }

    void Core::extendedOperation(DecodedInstruction&& inst) {
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
				pushDword(getAddressRegister());
				pushDword(getValueRegister());
				break;
			case ExtendedOperation::IsEven:
				getConditionRegister() = normalizeCondition(syn::isEven(registerValue(inst.getDestinationRegister<group>())));
				break;
			case ExtendedOperation::IsOdd:
				getConditionRegister() = normalizeCondition(syn::isOdd(registerValue(inst.getDestinationRegister<group>())));
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
	static constexpr byte convertTextToHex(Word input) noexcept {
		switch(input) {
			case 'f':
			case 'F':
				return 0xF;
			case 'e':
			case 'E':
				return 0xE;
			case 'd':
			case 'D':
				return 0xD;
			case 'c':
			case 'C':
				return 0xC;
			case 'b':
			case 'B':
				return 0xB;
			case 'a':
			case 'A':
				return 0xA;
			case '9': return 9;
			case '8': return 8;
			case '7': return 7;
			case '6': return 6;
			case '5': return 5;
			case '4': return 4;
			case '3': return 3;
			case '2': return 2;
			case '1': return 1;
			case '0':
			default:
				return 0x0;
		}
	}
	static constexpr Word hexToText(byte input) noexcept {
		switch(syn::decodeBits<byte, byte, 0x0F, 0>(input)) {
			case 0x1: return static_cast<Word>('1');
			case 0x2: return static_cast<Word>('2');
			case 0x3: return static_cast<Word>('3');
			case 0x4: return static_cast<Word>('4');
			case 0x5: return static_cast<Word>('5');
			case 0x6: return static_cast<Word>('6');
			case 0x7: return static_cast<Word>('7');
			case 0x8: return static_cast<Word>('8');
			case 0x9: return static_cast<Word>('9');
			case 0xA: return static_cast<Word>('A');
			case 0xB: return static_cast<Word>('B');
			case 0xC: return static_cast<Word>('C');
			case 0xD: return static_cast<Word>('D');
			case 0xE: return static_cast<Word>('E');
			case 0xF: return static_cast<Word>('F');
			case 0x0:
			default:
				return static_cast<Word>('0');
		}
	}
	template<RegisterValue mask, RegisterValue shift>
	static constexpr Word extractHexAndConvertToText(RegisterValue value) noexcept {
		return hexToText(syn::decodeBits<RegisterValue, byte, mask, shift>(value));
	}
	void Core::parsingOperation(DecodedInstruction&& inst) {
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

    void Core::arithmeticOperation(DecodedInstruction&& inst) {
        static constexpr auto group = Operation::Arithmetic;
        auto subType = inst.getSubtype<group>();
        auto src1 = inst.getImmediateFlag<group>() ? inst.getImmediate<group>() : registerValue(inst.getSourceRegister<group>());
        auto &src0 = registerValue(inst.getDestinationRegister<group>());
        if (subType == ArithmeticOps::Min) {
            getValueRegister() = src0 > src1 ? src1 : src0;
        } else if (subType == ArithmeticOps::Max) {
            getValueRegister() = src0 > src1 ? src0 : src1;
        } else {
            auto result = translate(inst.getSubtype<group>());
            syn::throwOnErrorState(result, "Illegal arithmetic operation!");
            auto src = src1;
            auto& dest = src0;
            dest = syn::ALU::performOperation<RegisterValue>(result, dest, src);
        }
    }

    void Core::logicalOperation(DecodedInstruction&& inst) {
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
        return syn::Comparator::performOperation<op, RegisterValue>(
                syn::ALU::performOperation<translate(LogicalOps::And), RegisterValue>(
                    syn::ALU::performOperation<ALUOperation::ShiftRight, RegisterValue>(
                        a,
                        b), 0x1), 1);
    }


    void Core::encodingOperation(DecodedInstruction&& inst) {
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
                getConditionRegister() = normalizeCondition(sliceBitAndCheck<syn::Comparator::StandardOperations::Eq>(getAddressRegister(), getFieldRegister()));
                break;
            case EncodingOperation::BitUnset:
                getConditionRegister() = normalizeCondition(sliceBitAndCheck<syn::Comparator::StandardOperations::Neq>(getAddressRegister(), getFieldRegister()));
                break;
            default:
                throw syn::Problem("Illegal complex encoding operation defined!");
        }
    }

    RegisterValue& Core::registerValue(byte index) {
        return gpr[index];
    }
    Word Core::getCurrentCodeWord() {
        return loadWord(getInstructionPointer());
    }
    void Core::storeWord(RegisterValue address, Word value) {
		if (address == ArchitectureConstants::TerminateAddress) {
			execute = false;
			advanceIp = false;
		} else {
			_bus.write(address, value);
		}
    }
    Word Core::loadWord(RegisterValue address) {
		return _bus.read(address);
    }
    void Core::pushWord(Word value) {
		decrementAddress(getStackPointer());
		storeWord(getStackPointer(), value);
    }
    void Core::pushDword(DWord value) {
        pushWord(decodeUpperHalf(value));
        pushWord(decodeLowerHalf(value));
    }

    Word Core::popWord() {
        auto result = loadWord(getStackPointer());
		incrementAddress(getStackPointer());
        return result;
    }
    RegisterValue Core::popRegisterValue() {
        auto lower = popWord();
        auto upper = popWord();
        return encodeRegisterValue(upper, lower);
    }
    Word Core::tryReadNext(bool readNext) {
        return readNext ? tryReadNext<true>() : tryReadNext<false>();
    }
}
