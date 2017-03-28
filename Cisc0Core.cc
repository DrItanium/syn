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
    /*
     * Iris18 is a variable length encoding 16 bit architecture.
     * It has a 24 bit memory space across 256 16-bit sections. The variable length
     * encoding comes from different register choices. The reserved registers are
     * used to compress the encoding.
     */

	inline constexpr Word lowerMask(byte bitmask) noexcept {
		return syn::encodeUint16LE(syn::expandBit(syn::getBit<byte, 0>(bitmask)),
									syn::expandBit(syn::getBit<byte, 1>(bitmask)));
	}
	inline constexpr Word upperMask(byte bitmask) noexcept {
		return syn::encodeUint16LE(syn::expandBit(syn::getBit<byte, 2>(bitmask)),
									syn::expandBit(syn::getBit<byte, 3>(bitmask)));
	}

	inline constexpr RegisterValue mask(byte bitmask) noexcept {
		return syn::encodeUint32LE(lowerMask(bitmask), upperMask(bitmask));
	}

	inline constexpr bool readLower(byte bitmask) noexcept {
		return lowerMask(bitmask) != 0;
	}

	inline constexpr bool readUpper(byte bitmask) noexcept {
		return upperMask(bitmask) != 0;
	}
    inline constexpr RegisterValue encodeRegisterValue(byte a, byte b, byte c, byte d) noexcept {
        return syn::encodeUint32LE(a, b, c, d);
    }
    inline constexpr Word encodeWord(byte a, byte b) noexcept {
        return syn::encodeUint16LE(a, b);
    }

    inline constexpr Word decodeUpperHalf(RegisterValue value) noexcept {
        return syn::decodeBits<RegisterValue, Word, mask(0b1100), 16>(value);
    }
    inline constexpr Word decodeLowerHalf(RegisterValue value) noexcept {
        return syn::decodeBits<RegisterValue, Word, mask(0b0011), 0>(value);
    }

    inline constexpr RegisterValue encodeUpperHalf(RegisterValue value, Word upperHalf) noexcept {
        return syn::encodeBits<RegisterValue, Word, mask(0b1100), 16>(value, upperHalf);
    }
    inline constexpr RegisterValue encodeLowerHalf(RegisterValue value, Word lowerHalf) noexcept {
        return syn::encodeBits<RegisterValue, Word, mask(0b0011), 0>(value, lowerHalf);
    }

    inline constexpr RegisterValue encodeRegisterValue(Word upper, Word lower) noexcept {
        return encodeUpperHalf(encodeLowerHalf(0, lower), upper);
    }
    inline constexpr RegisterValue normalizeCondition(RegisterValue input) noexcept {
        return input != 0 ? 0xFFFFFFFF : 0x00000000;
    }

    inline constexpr int instructionSizeFromImmediateMask(byte bitmask) noexcept {
        return 1 + (readLower(bitmask) ? 1 : 0) + (readUpper(bitmask) ? 1 : 0);
    }

    Core* newCore() noexcept {
        return new Core();
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

    void Core::installprogram(std::istream& stream) {
        //gpr.install(stream, [](char* buf) { return syn::encodeUint32LE((byte*)buf); });
		// the installation of a program is handled by the io bus now!
		// TODO: figure out how to do a program install...
    }

    void Core::dump(std::ostream& stream) {
        //gpr.dump(stream, [](RegisterValue value, char* buf) { syn::decodeUint32LE(value, (byte*)buf); });
		// the io bus must handle dumping stuff to disk
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
    void Core::incrementStackPointer() noexcept {
        incrementStackPointer(getStackPointer());
    }
    void Core::incrementStackPointer(RegisterValue& ptr) noexcept {
        incrementAddress(ptr);
    }

    void Core::decrementStackPointer() noexcept {
        decrementStackPointer(getStackPointer());
    }
    void Core::decrementStackPointer(RegisterValue& ptr) noexcept {
        decrementAddress(ptr);
    }
    auto throwIfNotFound = [](auto result, auto& table, const std::string& msg) {
        if (result == table.end()) {
            throw syn::Problem(msg);
        }
    };

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
                if (current.getSwapRegister<0>() != current.getSwapRegister<1>()) {
                    gpr.swap(current.getSwapRegister<0>(), current.getSwapRegister<1>());
                }
                break;
            case Operation::Move:
                registerValue(current.getMoveRegister<0>()) = syn::decodeBits<RegisterValue, RegisterValue>( registerValue(current.getMoveRegister<1>()), mask(current.getBitmask<Operation::Move>()), 0);
                break;
            case Operation::Set:
                registerValue(current.getSetDestination()) = retrieveImmediate(current.getBitmask<Operation::Set>());
                break;
            default: {
                         std::stringstream str;
                         str << "Illegal instruction " << std::hex << static_cast<int>(current.getControl()) << std::endl;
                         str << "Location: " << std::hex << getInstructionPointer() << std::endl;
                         execute = false;
                         throw syn::Problem(str.str());
                     }

        }
    }
    void Core::branchOperation(DecodedInstruction&& inst) {
        auto isImm = inst.getImmediateFlag<Operation::Branch>();
        bool isIf, isCall, isCond;
        std::tie(isIf, isCall, isCond) = inst.getOtherBranchFlags();
        advanceIp = true;
        auto choice = getConditionRegister() != 0;
        auto readAddress = [this]() {
            auto lower = static_cast<RegisterValue>(tryReadNext<true>());
            auto upper = static_cast<RegisterValue>(tryReadNext<true>()) << 16;
            return lower | upper;
        };
        if (isIf) {
            advanceIp = false;
            if (isCall) {
                // push the instruction pointer onto the stack
				pushDword(getInstructionPointer() + 1);
            }
            auto reg = choice ? inst.getBranchIfPathRegister<true>() : inst.getBranchIfPathRegister<false>();
            getInstructionPointer() = registerValue(reg);
        } else if (isCall) {
            // call instruction
            advanceIp = false;
            // determine next
            auto length = isImm ? 2 : 1;
			pushDword(getInstructionPointer() + length);
            getInstructionPointer() = isImm ? readAddress() : registerValue(inst.getBranchIndirectDestination());
        } else {
            // jump instruction
            if ((isCond && choice) || !isCond) {
                 advanceIp = false;
                 getInstructionPointer() = isImm ? readAddress() : registerValue(inst.getBranchIndirectDestination());
            }
        }
    }
    void Core::compareOperation(DecodedInstruction&& inst) {
        static constexpr auto group = Operation::Compare;
        static std::map<CompareStyle, CompareUnit::Operation> translationTable = {
            { CompareStyle::Equals, CompareUnit::Operation::Eq },
            { CompareStyle::NotEquals, CompareUnit::Operation::Neq },
            { CompareStyle::LessThan, CompareUnit::Operation::LessThan },
            { CompareStyle::LessThanOrEqualTo, CompareUnit::Operation::LessThanOrEqualTo },
            { CompareStyle::GreaterThan, CompareUnit::Operation::GreaterThan },
            { CompareStyle::GreaterThanOrEqualTo, CompareUnit::Operation::GreaterThanOrEqualTo },
        };
        DecodedInstruction next(tryReadNext<true>());
        auto first = registerValue(next.getCompareRegister<0>());
        auto second = inst.getImmediateFlag<group>() ? next.getUpper() : registerValue(next.getCompareRegister<1>());
        auto compareResult = translationTable.find(inst.getSubtype<group>());
        throwIfNotFound(compareResult, translationTable, "Illegal compare type!");
        auto result = _compare(compareResult->second, first, second);
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
                lower = useLower ? encodeLowerHalf(0, loadWord(address)) : 0u;
                upper = useUpper ? encodeUpperHalf(0, loadWord(address + 1)) : 0u;
                value = syn::encodeBits<RegisterValue, RegisterValue>(0u, lower | upper, fullMask, 0);
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
            } else {
                // update the target stack to something different
                auto pushToStack = registerValue(memoryRegister);
                auto &stackPointer = getStackPointer();
                // read backwards because the stack grows upward towards zero
                if (useUpper) {
                    pushWord(umask & decodeUpperHalf(pushToStack), stackPointer);
                }
                if (useLower) {
                    pushWord(lmask & decodeLowerHalf(pushToStack), stackPointer);
                }
            }
        } else if (rawType == MemoryOperation::Pop) {
            if (inst.isIndirectOperation()) {
                throw syn::Problem("Indirect bit not supported in pop operations!");
            } else {
                auto &stackPointer = getStackPointer();
                if (useLower) {
                    lower = lmask & popWord(stackPointer);
                }
                if (useUpper) {
                    upper = umask & popWord(stackPointer);
                }
                registerValue(memoryRegister) = encodeRegisterValue(upper, lower);
                // can't think of a case where we should
                // restore the instruction pointer and then
                // immediate advance so just don't do it
                advanceIp = memoryRegister != ArchitectureConstants::InstructionPointer;
            }
        } else {
            throw syn::Problem("Illegal memory operation!");
        }

    }

    void Core::complexOperation(DecodedInstruction&& inst) {
        auto type = inst.getSubtype<Operation::Complex>();
        if (type == ComplexSubTypes::Encoding) {
            encodingOperation(std::move(inst));
        } else {
            throw syn::Problem("Undefined complex subtype!");
        }
    }

    void Core::shiftOperation(DecodedInstruction&& inst) {
        static constexpr auto group = Operation::Shift;
        auto &destination = registerValue(inst.getShiftRegister<0>());
        auto source = (inst.getImmediateFlag<group>() ? static_cast<RegisterValue>(inst.getImmediate<group>()) : registerValue(inst.getShiftRegister<1>()));
        destination = _shifter( inst.shouldShiftLeft() ? ALU::Operation::ShiftLeft : ALU::Operation::ShiftRight, destination, source);
    }
    void Core::arithmeticOperation(DecodedInstruction&& inst) {
        static std::map<ArithmeticOps, ALU::Operation> translationTable = {
            { ArithmeticOps::Add, ALU::Operation::Add },
            { ArithmeticOps::Sub, ALU::Operation::Subtract },
            { ArithmeticOps::Mul, ALU::Operation::Multiply },
            { ArithmeticOps::Div, ALU::Operation::Divide},
            { ArithmeticOps::Rem, ALU::Operation::Remainder},
        };
        static constexpr auto group = Operation::Arithmetic;
        auto result = translationTable.find(inst.getSubtype<Operation::Arithmetic>());
        throwIfNotFound(result, translationTable, "Illegal arithmetic operation!");
        auto op = result->second;
        auto src = inst.getImmediateFlag<group>() ? inst.getImmediate<group>() : registerValue(inst.getArithmeticRegister<1>());
        auto& dest = registerValue(inst.getArithmeticRegister<0>());
        dest = _alu(op, dest, src);
    }
    void Core::logicalOperation(DecodedInstruction&& inst) {
        static std::map<LogicalOps, ALU::Operation> dispatchTable = {
            { LogicalOps::Not, ALU::Operation::UnaryNot },
            { LogicalOps::Or, ALU::Operation::BinaryOr },
            { LogicalOps::And, ALU::Operation::BinaryAnd },
            { LogicalOps::Xor, ALU::Operation::BinaryXor },
            { LogicalOps::Nand, ALU::Operation::BinaryNand },
        };
        static constexpr auto group = Operation::Logical;
        auto result = dispatchTable.find(inst.getSubtype<Operation::Logical>());
        throwIfNotFound(result, dispatchTable, "Illegal logical operation!");
        auto op = result->second;
        auto source1 = inst.getImmediateFlag<group>() ? retrieveImmediate(inst.getBitmask<group>()) : registerValue(inst.getLogicalRegister<1>());
        auto& dest = registerValue(inst.getLogicalRegister<0>());
        dest = _logicalOps(op, dest, source1);
    }

    void Core::encodingOperation(DecodedInstruction&& inst) {
        auto sliceBitAndCheck = [this](CompareUnit::Operation op) {
            return normalizeCondition(_compare(op,
                        _logicalOps(ALU::Operation::BinaryAnd,
                            _shifter(ALU::Operation::ShiftRight,
                                getAddressRegister(),
                                getFieldRegister()), 0x1), 1));
        };
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
                getConditionRegister() = sliceBitAndCheck(CompareUnit::Operation::Eq);
                break;
            case EncodingOperation::BitUnset:
                getConditionRegister() = sliceBitAndCheck(CompareUnit::Operation::Neq);
                break;
            default:
                throw syn::Problem("Illegal complex encoding operation defined!");
        }
    }

    void Core::terminate(Core* core, DecodedInstruction&& inst) {
        core->execute = false;
        core->advanceIp = false;
    }

    void Core::link(std::istream& input) {
        // we have some more data to read through
        // two address system, 1 RegisterValue -> address, 1 Word -> value
        static constexpr int bufSize = 8;
        char buf[bufSize] = { 0 };
        for(int lineNumber = 0; input.good(); ++lineNumber) {
            input.read(buf, bufSize);
            if (input.gcount() == 0) {
                break;
            } else if (input.gcount() != bufSize) {
                throw syn::Problem("unaligned object file found");
            } else {
                // use the first byte to determine what sort of installation
                // should occur
                switch (buf[0]) {
                    case 0: // memory value
                        storeWord(encodeRegisterValue(buf[2], buf[3], buf[4], buf[5]), encodeWord(buf[6], buf[7]));
                        break;
                    case 1: // register value
                        gpr[static_cast<byte>(buf[1])] = encodeRegisterValue(buf[2], buf[3], buf[4], buf[5]);
                        break;
                    default:
                        throw syn::Problem("undefined link class!");
                }
            }
        }
    }
    RegisterValue& Core::registerValue(byte index) {
        return gpr[index];
    }
    Word Core::getCurrentCodeWord() noexcept {
		return _bus.read(getInstructionPointer());
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
    RegisterValue Core::loadRegisterValue(RegisterValue address) {
        return syn::encodeBits<RegisterValue, Word, mask(0b1111), 16>(static_cast<RegisterValue>(loadWord(address)), loadWord(address + 1));
    }
    void Core::storeRegisterValue(RegisterValue address, RegisterValue value) {
        storeWord(address, decodeLowerHalf(value));
        storeWord(address + 1, decodeUpperHalf(value));
    }

    void Core::pushWord(Word value) {
        pushWord(value, getStackPointer());
    }
    void Core::pushWord(Word value, RegisterValue& ptr) {
        decrementStackPointer(ptr);
        storeWord(ptr, value);
    }
    void Core::pushDword(DWord value, RegisterValue& ptr) {
        pushWord(decodeUpperHalf(value), ptr);
        pushWord(decodeLowerHalf(value), ptr);
    }
    void Core::pushDword(DWord value) {
        return pushDword(value, getStackPointer());
    }

    Word Core::popWord() {
        return popWord(getStackPointer());
    }
    Word Core::popWord(RegisterValue& ptr) {
        auto result = loadWord(ptr);
        incrementStackPointer(ptr);
        return result;
    }
    Word Core::tryReadNext(bool readNext) noexcept {
        if (readNext) {
            return tryReadNext<true>();
        } else {
            return tryReadNext<false>();
        }
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeArithmetic() const {
        auto first = encodeControl(0, type);
        first = encodeArithmeticFlagImmediate(first, immediate);
        first = encodeArithmeticFlagType(first, static_cast<ArithmeticOps>(subType));
        first = encodeArithmeticDestination(first, arg0);
        first = immediate ? encodeArithmeticImmediate(first, arg1) : encodeArithmeticSource(first, arg1);
        return std::make_tuple(1, first, 0, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeMove() const {
        auto first = encodeControl(0, type);
        first = encodeMoveBitmask(first, bitmask);
        first = encodeMoveRegister0(first, arg0);
        first = encodeMoveRegister1(first, arg1);
        return std::make_tuple(1, first, 0, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeSwap() const {
        return std::make_tuple(1, encodeSwapSource( encodeSwapDestination( encodeControl(0, type), arg0), arg1), 0, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeShift() const {
        auto first = encodeControl(0, type);
        first = encodeShiftFlagImmediate(first, immediate);
        first = encodeShiftFlagLeft(first, shiftLeft);
        first = encodeShiftRegister0(first, arg0);
        first = immediate ? encodeShiftImmediate(first, arg1) : encodeShiftRegister1(first, arg1);
        return std::make_tuple(1, first, 0, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeCompare() const {
        auto first = encodeControl(0, type);
        first = encodeCompareType(first, static_cast<CompareStyle>(subType));
        first = encodeCompareImmediateFlag(first, immediate);
        auto second = encodeCompareRegister0(0, arg0);
        second = immediate ? encodeCompareImmediate(second, arg1) : encodeCompareRegister1(second, arg1);
        return std::make_tuple(2, first, second, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeSet() const {
        int count = instructionSizeFromImmediateMask(bitmask);
        auto first = encodeControl(0, type);
        first = encodeSetBitmask(first, bitmask);
        first = encodeSetDestination(first, arg0);
        // use the mask during encoding since we know how many Words the
        // instruction is made up of
        auto maskedValue = mask(bitmask) & fullImmediate;
        auto second = static_cast<Word>(maskedValue);
        auto third = static_cast<Word>(maskedValue >> 16);
        return std::make_tuple(count, first, second, third);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeMemory() const {
        auto first = encodeControl(0, type);
        first = encodeMemoryFlagType(first, static_cast<MemoryOperation>(subType));
        first = encodeMemoryFlagBitmask(first, bitmask);
        first = encodeMemoryFlagIndirect(first, indirect);
        // the register and offset occupy the same space
        first = encodeMemoryOffset(first, arg0);
        // be lazy and set up the second word even if it isn't used. Reduces
        // the amount of branching and special cases :)
        auto second = encodeMemoryAddress(0, arg1);
        second = encodeMemoryValue(0, arg2);
        return std::make_tuple(readNextWord ? 2 : 1, first, second, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeLogical() const {
        auto first = encodeControl(0, type);
        first = encodeLogicalFlagImmediate(first, immediate);
        first = encodeLogicalFlagType(first, static_cast<LogicalOps>(subType));
        if (immediate) {
            first = encodeLogicalFlagImmediateMask(first, bitmask);
            first = encodeLogicalImmediateDestination(first, arg0);
            auto maskedImmediate = mask(bitmask) & fullImmediate;
            auto second = static_cast<Word>(maskedImmediate);
            auto third = static_cast<Word>(maskedImmediate >> 16);
            return std::make_tuple(instructionSizeFromImmediateMask(bitmask), first, second, third);
        } else {
            first = encodeLogicalRegister0(first, arg0);
            first = encodeLogicalRegister1(first, arg1);
            return std::make_tuple(1, first, 0, 0);
        }
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeBranch() const {
        auto first = encodeControl(0, type);
        first = encodeBranchFlagIsConditional(first, isConditional);
        first = encodeBranchFlagIsIfForm(first, isIf);
        first = encodeBranchFlagIsImmediate(first, immediate);
        first = encodeBranchFlagIsCallForm(first, isCall);
        if (isIf) {
            first = encodeBranchIfOnTrue(first, arg0);
            first = encodeBranchIfOnFalse(first, arg1);
            return std::make_tuple(1, first, 0, 0);
        } else {
            if (immediate) {
                // encode the 24-bit number
                auto second = static_cast<Word>(fullImmediate);
				auto third = static_cast<Word>(fullImmediate >> 16);
                return std::make_tuple(3, first, second, third);
            } else {
                first = encodeBranchIndirectDestination(first, arg0);
                return std::make_tuple(1, first, 0, 0);
            }
        }
    }
    InstructionEncoder::Encoding InstructionEncoder::encodeComplex() const {
        auto sType = static_cast<ComplexSubTypes>(subType);
        auto first = encodeControl(0, type);
        first = encodeComplexSubClass(first, sType);
        if (sType == ComplexSubTypes::Encoding) {
            // right now it is a single word
            first = encodeComplexClassEncoding_Type(first, static_cast<EncodingOperation>(bitmask));
            return std::make_tuple(1, first, 0, 0);
        } else {
            throw syn::Problem("Attempted to encode an unsupported value as a complex type!");
        }
    }

    InstructionEncoder::Encoding InstructionEncoder::encode() const {
        // always encode the type
        static auto testMemFn = std::mem_fn(&InstructionEncoder::encode);
        static std::map<Operation, decltype(testMemFn)> dispatchTable = {
            { Operation::Memory, std::mem_fn(&InstructionEncoder::encodeMemory) },
            { Operation::Arithmetic, std::mem_fn(&InstructionEncoder::encodeArithmetic) },
            { Operation::Shift, std::mem_fn(&InstructionEncoder::encodeShift) },
            { Operation::Logical, std::mem_fn(&InstructionEncoder::encodeLogical) },
            { Operation::Compare, std::mem_fn(&InstructionEncoder::encodeCompare) },
            { Operation::Branch, std::mem_fn(&InstructionEncoder::encodeBranch) },
            { Operation::Move, std::mem_fn(&InstructionEncoder::encodeMove) },
            { Operation::Set, std::mem_fn(&InstructionEncoder::encodeSet) },
            { Operation::Swap, std::mem_fn(&InstructionEncoder::encodeSwap) },
            { Operation::Complex, std::mem_fn(&InstructionEncoder::encodeComplex) },
        };

        auto result = dispatchTable.find(type);
        if (result == dispatchTable.end()) {
            throw syn::Problem("Illegal type to encode!");
        } else {
            return result->second(this);
        }
    }

    int InstructionEncoder::numWords() const {
        return std::get<0>(encode());
    }
    void InstructionEncoder::clear() {
        currentLine = 0;
        address = 0;
        type = Operation::Memory;
        immediate = false;
        shiftLeft = false;
        isIf = false;
        isCall = false;
        isConditional = false;
        bitmask = 0b0000;
        arg0 = 0;
        arg1 = 0;
        arg2 = 0;
        isLabel = false;
        labelValue.clear();
        subType = 0;
        fullImmediate = 0;
        indirect = false;
        readNextWord = false;
    }

    DecodedInstruction::BranchFlags DecodedInstruction::getOtherBranchFlags() const noexcept {
        return std::make_tuple(decodeBranchFlagIsIfForm(_rawValue),
                               decodeBranchFlagIsCallForm(_rawValue),
                               decodeBranchFlagIsConditional(_rawValue));
    }

    constexpr bool DecodedInstruction::hasBitmask(Operation op) noexcept {
        return op == Operation::Set ||
            op == Operation::Memory ||
            op == Operation::Move ||
            op == Operation::Logical;
    }

    constexpr bool DecodedInstruction::hasImmediateFlag(Operation op) noexcept {
        return op == Operation::Shift ||
            op == Operation::Arithmetic ||
            op == Operation::Logical ||
            op == Operation::Branch ||
            op == Operation::Compare;
    }
    constexpr bool DecodedInstruction::hasImmediateValue(Operation op) noexcept {
        return op == Operation::Shift ||
            op == Operation::Arithmetic;
    }
    constexpr bool DecodedInstruction::hasSubtype(Operation op) noexcept {
        return op == Operation::Compare ||
            op == Operation::Memory ||
            op == Operation::Arithmetic ||
            op == Operation::Complex ||
            op == Operation::Logical;

    }
}
