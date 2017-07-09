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
    Core::Core(const std::string& busUcode, RegisterValue ioStart, RegisterValue ioEnd) noexcept : _bus(ioStart, ioEnd, busUcode) { }
    void Core::initialize() {
        cisc0::installAssemblerParsingState(_bus.getRawEnvironment());
		_bus.initialize();
    }
    void Core::shutdown() {
        _bus.shutdown();
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

    void illegalInstruction(const DecodedInstruction& current, RegisterValue ip) {
        std::stringstream str;
        str << "Illegal instruction " << std::hex << static_cast<int>(current.getControl()) << std::endl;
        str << "Location: " << std::hex << ip << std::endl;
        auto s = str.str();
        throw syn::Problem(s);
    }

    RegisterValue& Core::getInstructionPointer() noexcept { return registerValue(ArchitectureConstants::InstructionPointer); }
    RegisterValue& Core::getStackPointer() noexcept { return registerValue(ArchitectureConstants::StackPointer); }
    RegisterValue& Core::getCallStackPointer() noexcept { return registerValue(ArchitectureConstants::CallStackPointer); }
    RegisterValue& Core::getAddressRegister() noexcept { return registerValue(ArchitectureConstants::AddressRegister); }
    RegisterValue& Core::getValueRegister() noexcept { return registerValue(ArchitectureConstants::ValueRegister); }
    RegisterValue& Core::getMaskRegister() noexcept { return registerValue(ArchitectureConstants::MaskRegister); }
    RegisterValue  Core::getShiftRegister() noexcept { return 0b11111 & registerValue(ArchitectureConstants::ShiftRegister); }
    RegisterValue  Core::getFieldRegister() noexcept { return 0b11111 & registerValue(ArchitectureConstants::FieldRegister); }


    void Core::pushWord(Word value) {
		pushWord(value, getStackPointer());
    }
	void Core::pushWord(Word value, RegisterValue& sp) {
		decrementAddress(sp);
		storeWord(sp, value);
	}
    void Core::pushRegisterValue(RegisterValue value) {
		pushRegisterValue(value, getStackPointer());
    }

	void Core::pushRegisterValue(RegisterValue value, RegisterValue& sp) {
		pushWord(decodeUpperHalf(value), sp);
		pushWord(decodeLowerHalf(value), sp);
	}

    Word Core::popWord() {
		return popWord(getStackPointer());
    }
	Word Core::popWord(RegisterValue& sp) {
		auto result = loadWord(sp);
		incrementAddress(sp);
		return result;
	}
    RegisterValue Core::popRegisterValue(RegisterValue& sp) {
        auto lower = popWord(sp);
        auto upper = popWord(sp);
        return encodeRegisterValue(upper, lower);
    }
	RegisterValue Core::popRegisterValue() {
		return popRegisterValue(getStackPointer());
	}

    void Core::storeWord(RegisterValue address, Word value) {
        if (isTerminateAddress(address)) {
            execute = false;
            advanceIp = false;
        } else {
            _bus.write(address, value);
        }
    }

    Word Core::loadWord(RegisterValue address) {
        if (isTerminateAddress(address)) {
            return 0;
        } else {
            return _bus.read(address);
        }
    }

    bool Core::isTerminateAddress(RegisterValue address) const noexcept {
        return address == ArchitectureConstants::TerminateAddress;
    }

    void Core::returnOperation() noexcept {
        // pop the top address off of the call stack and place it in the
        // instruction pointer!
        getInstructionPointer() = popRegisterValue(getCallStackPointer());
        advanceIp = false;
    }

    void Core::storeWord(RegisterValue addr, byte offset, Word value) {
        storeWord(addr + offset, value);
    }

    Word Core::loadWord(RegisterValue addr, byte offset) {
        return loadWord(addr + offset);
    }

    void Core::hex8ToRegister() {
        // 1) use the address contained in address to read the next 8 words
        // 2) Parse each word as an ascii character and convert it into a 4 bit quantity
        // 3) Place that 4bit quantity into the appropriate position in value
        auto addr = getAddressRegister();
        auto value = syn::encodeBits<RegisterValue, byte, 0x0000000F, 0>(0, convertTextToHex(loadWord(addr)));
        value = syn::encodeBits<RegisterValue, byte, 0x000000F0, 4>(value, convertTextToHex(loadWord(addr, 1)));
        value = syn::encodeBits<RegisterValue, byte, 0x00000F00, 8>(value, convertTextToHex(loadWord(addr, 2)));
        value = syn::encodeBits<RegisterValue, byte, 0x0000F000, 12>(value, convertTextToHex(loadWord(addr, 3)));
        value = syn::encodeBits<RegisterValue, byte, 0x000F0000, 16>(value, convertTextToHex(loadWord(addr, 4)));
        value = syn::encodeBits<RegisterValue, byte, 0x00F00000, 20>(value, convertTextToHex(loadWord(addr, 5)));
        value = syn::encodeBits<RegisterValue, byte, 0x0F000000, 24>(value, convertTextToHex(loadWord(addr, 6)));
        getValueRegister() = syn::encodeBits<RegisterValue, byte, 0xF0000000, 28>(value, convertTextToHex(loadWord(addr, 7)));
    }

    void Core::registerToHex8() {
        auto addr = getAddressRegister();
        auto value = getValueRegister();
        storeWord(addr, extractHexAndConvertToText<0x0000000F, 0>(value));
        storeWord(addr, 1, extractHexAndConvertToText<0x000000F0, 4>(value));
        storeWord(addr, 2, extractHexAndConvertToText<0x00000F00, 8>(value));
        storeWord(addr, 3, extractHexAndConvertToText<0x0000F000, 12>(value));
        storeWord(addr, 4, extractHexAndConvertToText<0x000F0000, 16>(value));
        storeWord(addr, 5, extractHexAndConvertToText<0x00F00000, 20>(value));
        storeWord(addr, 6, extractHexAndConvertToText<0x0F000000, 24>(value));
        storeWord(addr, 7, extractHexAndConvertToText<0xF0000000, 28>(value));
    }

    void Core::setBit() {
        defaultSliceBitAndCheck<syn::Comparator::StandardOperations::Eq>();
    }

    void Core::unsetBit() {
        defaultSliceBitAndCheck<syn::Comparator::StandardOperations::Neq>();
    }
    void Core::encodeBits() {
        getAddressRegister() = syn::encodeBits<RegisterValue, RegisterValue>(getAddressRegister(), getValueRegister(), getMaskRegister(), getShiftRegister());
    }
    void Core::decodeBits() {
        // connect the result of the logical operations alu to the
        // shifter alu then store the result in the value register
        getValueRegister() = syn::decodeBits<RegisterValue, RegisterValue>(getAddressRegister(), getMaskRegister(), getShiftRegister());
    }

    void Core::defaultEncodingOperation(EncodingOperation op) {
        switch(op) {
            case EncodingOperation::Decode:
                decodeBits();
                break;
            case EncodingOperation::Encode:
                encodeBits();
                break;
            case EncodingOperation::BitSet:
                setBit();
                break;
            case EncodingOperation::BitUnset:
                unsetBit();
                break;
            default:
                throw syn::Problem("Illegal complex encoding operation defined!");
        }
    }


}
