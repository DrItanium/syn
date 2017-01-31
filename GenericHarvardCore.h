#ifndef _TARGET_GENERIC_HARVARD_CORE_H
#define _TARGET_GENERIC_HARVARD_CORE_H
#include "Base.h"
#include "Core.h"
#include "ExecutionUnits.h"
#include <cstdint>
#include <memory>
namespace generic {
    // TODO: add support for
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
