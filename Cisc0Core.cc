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
    template<typename T, T value>
    constexpr auto toExecutionUnitValue = syn::defaultErrorState<T>;

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
    template<typename T>
    void throwOnCount(T result, const std::string& msg) {
        if (syn::isErrorState(result)) {
            throw syn::Problem(msg);
        }
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
    using CompareOperation = syn::Comparator::StandardOperations;
    template<> constexpr auto toExecutionUnitValue<CompareStyle, CompareStyle::Equals> = CompareOperation::Eq;
    template<> constexpr auto toExecutionUnitValue<CompareStyle, CompareStyle::NotEquals> = CompareOperation::Neq;
    template<> constexpr auto toExecutionUnitValue<CompareStyle, CompareStyle::LessThan> = CompareOperation::LessThan;
    template<> constexpr auto toExecutionUnitValue<CompareStyle, CompareStyle::LessThanOrEqualTo> = CompareOperation::LessThanOrEqualTo;
    template<> constexpr auto toExecutionUnitValue<CompareStyle, CompareStyle::GreaterThan> = CompareOperation::GreaterThan;
    template<> constexpr auto toExecutionUnitValue<CompareStyle, CompareStyle::GreaterThanOrEqualTo> = CompareOperation::GreaterThanOrEqualTo;
    constexpr CompareOperation translate(CompareStyle cs) noexcept {
        using T = CompareStyle;
        switch(cs) {
            case T::Equals:
                return toExecutionUnitValue<T, T::Equals>;
            case T::NotEquals:
                return toExecutionUnitValue<T, T::NotEquals>;
            case T::LessThan:
                return toExecutionUnitValue<T, T::LessThan>;
            case T::LessThanOrEqualTo:
                return toExecutionUnitValue<T, T::LessThanOrEqualTo>;
            case T::GreaterThan:
                return toExecutionUnitValue<T, T::GreaterThan>;
            case T::GreaterThanOrEqualTo:
                return toExecutionUnitValue<T, T::GreaterThanOrEqualTo>;
            default:
                return syn::defaultErrorState<CompareOperation>;
        }
    }
    void Core::compareOperation(DecodedInstruction&& inst) {
        static constexpr auto group = Operation::Compare;
        auto compareResult = translate(inst.getSubtype<group>());
        throwOnCount(compareResult, "Illegal compare type!");
        DecodedInstruction next(tryReadNext<true>());
        auto first = registerValue(next.getCompareRegister<0>());
        auto second = inst.getImmediateFlag<group>() ? next.getUpper() : registerValue(next.getCompareRegister<1>());
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

    using ALUOperation = syn::ALU::StandardOperations;
    void Core::shiftOperation(DecodedInstruction&& inst) {
        static constexpr auto group = Operation::Shift;
        auto &destination = registerValue(inst.getShiftRegister<0>());
        auto source = (inst.getImmediateFlag<group>() ? static_cast<RegisterValue>(inst.getImmediate<group>()) : registerValue(inst.getShiftRegister<1>()));
        destination = syn::ALU::performOperation<RegisterValue>( inst.shouldShiftLeft() ? ALUOperation::ShiftLeft : ALUOperation::ShiftRight, destination, source);
    }
    template<> const auto toExecutionUnitValue<ArithmeticOps, ArithmeticOps::Add> = ALUOperation::Add;
    template<> const auto toExecutionUnitValue<ArithmeticOps, ArithmeticOps::Sub> = ALUOperation::Subtract;
    template<> const auto toExecutionUnitValue<ArithmeticOps, ArithmeticOps::Mul> = ALUOperation::Multiply;
    template<> const auto toExecutionUnitValue<ArithmeticOps, ArithmeticOps::Div> = ALUOperation::Divide;
    template<> const auto toExecutionUnitValue<ArithmeticOps, ArithmeticOps::Rem> = ALUOperation::Remainder;

    constexpr ALUOperation translate(ArithmeticOps op) noexcept {
        using T = ArithmeticOps;
        switch(op) {
            case T::Add:
                return toExecutionUnitValue<T, T::Add>;
            case T::Sub:
                return toExecutionUnitValue<T, T::Sub>;
            case T::Mul:
                return toExecutionUnitValue<T, T::Mul>;
            case T::Div:
                return toExecutionUnitValue<T, T::Div>;
            case T::Rem:
                return toExecutionUnitValue<T, T::Rem>;
            default:
                return syn::defaultErrorState<ALUOperation>;
        }
    }
    void Core::arithmeticOperation(DecodedInstruction&& inst) {
        static constexpr auto group = Operation::Arithmetic;
        auto result = translate(inst.getSubtype<group>());
        throwOnCount(result, "Illegal arithmetic operation!");
        auto op = result;
        auto src = inst.getImmediateFlag<group>() ? inst.getImmediate<group>() : registerValue(inst.getArithmeticRegister<1>());
        auto& dest = registerValue(inst.getArithmeticRegister<0>());
        dest = syn::ALU::performOperation<RegisterValue>(op, dest, src);
    }

    template<> constexpr auto toExecutionUnitValue<LogicalOps, LogicalOps::Not> = ALUOperation::UnaryNot;
    template<> constexpr auto toExecutionUnitValue<LogicalOps, LogicalOps::Or> = ALUOperation::BinaryOr;
    template<> constexpr auto toExecutionUnitValue<LogicalOps, LogicalOps::And> = ALUOperation::BinaryAnd;
    template<> constexpr auto toExecutionUnitValue<LogicalOps, LogicalOps::Xor> = ALUOperation::BinaryXor;
    template<> constexpr auto toExecutionUnitValue<LogicalOps, LogicalOps::Nand> = ALUOperation::BinaryNand;

    constexpr ALUOperation translate(LogicalOps op) noexcept {
        switch(op) {
            case LogicalOps::Not:
                return toExecutionUnitValue<LogicalOps, LogicalOps::Not>;
            case LogicalOps::Or:
                return toExecutionUnitValue<LogicalOps, LogicalOps::Or>;
            case LogicalOps::And:
                return toExecutionUnitValue<LogicalOps, LogicalOps::And>;
            case LogicalOps::Xor:
                return toExecutionUnitValue<LogicalOps, LogicalOps::Xor>;
            case LogicalOps::Nand:
                return toExecutionUnitValue<LogicalOps, LogicalOps::Nand>;
            default:
                return syn::defaultErrorState<ALUOperation>;
        }
    }
    void Core::logicalOperation(DecodedInstruction&& inst) {
        static constexpr auto group = Operation::Logical;
        auto result = translate(inst.getSubtype<group>());
        throwOnCount(result, "Illegal logical operation!");
        auto op = result;
        auto source1 = inst.getImmediateFlag<group>() ? retrieveImmediate(inst.getBitmask<group>()) : registerValue(inst.getLogicalRegister<1>());
        auto& dest = registerValue(inst.getLogicalRegister<0>());
        dest = syn::ALU::performOperation<RegisterValue>(op, dest, source1);
    }
    template<syn::Comparator::StandardOperations op>
    constexpr RegisterValue sliceBitAndCheck(RegisterValue a, RegisterValue b) noexcept {
        return syn::Comparator::performOperation<op, RegisterValue>(
                syn::ALU::performOperation<ALUOperation::BinaryAnd, RegisterValue>(
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


    DecodedInstruction::BranchFlags DecodedInstruction::getOtherBranchFlags() const noexcept {
        return std::make_tuple(decodeBranchFlagIsIfForm(_rawValue),
                               decodeBranchFlagIsCallForm(_rawValue),
                               decodeBranchFlagIsConditional(_rawValue));
    }

    constexpr bool DecodedInstruction::hasBitmask(Operation op) noexcept {
        switch(op) {
            case Operation::Set:
            case Operation::Memory:
            case Operation::Move:
            case Operation::Logical:
                return true;
            default:
                return false;
        }
    }

    constexpr bool DecodedInstruction::hasImmediateFlag(Operation op) noexcept {
        switch(op) {
            case Operation::Shift:
            case Operation::Logical:
            case Operation::Branch:
            case Operation::Compare:
            case Operation::Arithmetic:
                return true;
            default:
                return false;
        }
    }
    constexpr bool DecodedInstruction::hasImmediateValue(Operation op) noexcept {
        switch (op) {
            case Operation::Shift:
            case Operation::Arithmetic:
                return true;
            default:
                return false;
        }
    }
    constexpr bool DecodedInstruction::hasSubtype(Operation op) noexcept {
        switch(op) {
            case Operation::Compare:
            case Operation::Memory:
            case Operation::Arithmetic:
            case Operation::Complex:
            case Operation::Logical:
                return true;
            default:
                return false;
        }
    }
}
