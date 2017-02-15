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


#ifndef _TARGET_GENERIC_HARVARD_CORE_H
#define _TARGET_GENERIC_HARVARD_CORE_H
#include "Base.h"
#include "Core.h"
#include "ExecutionUnits.h"
#include <cstdint>
#include <memory>
namespace generic {
    template<
    template<typename DataType, typename RegisterType, typename AddressType, typename InstructionType, AddressType ipMask, AddressType dataMask, AddressType registerCount, AddressType codeSectionSize, AddressType dataSectionSize, bool performStaticAssertChecks= true>
    class HarvardCore : public syn::Core {
        static_assert(performStaticAssertChecks ? ipMask >= codeSectionSize : true , "The instruction pointer can't address all of the code section memory!");
        static_assert(performStaticAssertChecks ? dataMask >= dataSectionSize : true, "The data mask can't address all of the data section!");
        public:
            using RegisterFile = syn::FixedSizeLoadStoreUnit<RegisterType, AddressType, registerCount>;
            using CodeSpace = syn::FixedSizeLoadStoreUnit<InstructionType, AddressType, codeSectionSize>;
            using DataSpace = syn::FixedSizeLoadStoreUnit<DataType, AddressType, dataSectionSize>;
        public:
            HarvardCore() noexcept { }
            virtual ~HarvardCore() { }
            inline AddressType getCodeSize() const noexcept { return codeSectionSize; }
            inline AddressType getDataSize() const noexcept { return dataSectionSize; }
            inline AddressType getRegisterCount() const noexcept { return registerCount; }
        protected:
            static inline constexpr AddressType makeDataAddress(AddressType address) noexcept { return syn::decodeBits<AddressType, AddressType, dataMask, 0>(address); }
            static inline constexpr AddressType makeCodeAddress(AddressType address) noexcept { return syn::decodeBits<AddressType, AddressType, ipMask, 0>(address); }

            inline AddressType getInstructionPointer() noexcept { return makeCodeAddress(_ip); }
            inline void setInstructionPointer(AddressType value) noexcept { _ip = makeCodeAddress(value); }
            inline void incrementInstructionPointer() noexcept { setInstructionPointer(_ip + 1); }
            inline void decrementInstructionPointer() noexcept { setInstructionPointer(_ip - 1); }

            inline DataType readWordFromDataMemory(AddressType addr) { return _data[addr]; }
            inline void writeWordToDataMemory(AddressType addr, DataType value) { _data[addr] = value; }

            inline RegisterType getRegisterValue(AddressType index) { return _registers[index]; }
            inline void setRegisterValue(AddressType index, RegisterType value) { _registers[index] = value; }

            inline void writeInstructionToCodeMemory(AddressType addr, InstructionType inst) { _code[addr] = inst; }
            inline InstructionType readInstructionFromCodeMemory(AddressType addr) { return _code[addr]; }
            inline InstructionType readInstructionFromCodeMemory() noexcept { return readInstructionFromCodeMemory(getInstructionPointer()); }

        protected:
            AddressType _ip;
            RegisterFile _registers;
            CodeSpace _code;
            DataSpace _data;
    };
    template<typename DataType, typename RegisterType, typename AddressType, typename InstructionType, AddressType ipMask, AddressType registerCount, bool performStaticAssertChecks = true>
    using BalancedHarvardCore = HarvardCore<DataType, RegisterType, AddressType, InstructionType, ipMask, ipMask, registerCount, ipMask, ipMask, performStaticAssertChecks>;

} // end namespace generic
#endif
