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


#ifndef _TARGET_CISC0_IRIS_H
#define _TARGET_CISC0_IRIS_H
#include "Base.h"
#include "ExecutionUnits.h"
#include "Core.h"
#include "Problem.h"
#include <cstdint>
#include <sstream>
#include <memory>
#include <vector>
#include <tuple>
#include "IODevice.h"
#include "IOController.h"

extern "C" {
	#include "clips.h"
}

namespace cisc0 {
	using Word = uint16_t;
	using DWord = uint32_t;
	using RawInstruction = Word; // this is more of a packet!
	using RegisterValue = DWord;
    using Address = DWord;

	enum ArchitectureConstants  {
		RegisterCount = 16,
		SegmentCount = 256,
		AddressMax = 65536 * SegmentCount,
		MaxInstructionCount = 16,
		TerminateAddress = 0xFFFFFFFF,
		StartingIPAddress = 0xFE000000,
		// unlike iris16 and iris32, there is a limited set of registers with
		// a majority of them marked for explicit usage, instructions
		// themselves are still 16 bits wide but 32bits are extracted per
		// packet.
		R15 = RegisterCount - 1,
		R14 = RegisterCount - 2,
		R13 = RegisterCount - 3,
		R12 = RegisterCount - 4,
		R11 = RegisterCount - 5,
		R10 = RegisterCount - 6,
		R9  = RegisterCount - 7,
		R8  = RegisterCount - 8,
		R7  = RegisterCount - 9,
		R6  = RegisterCount - 10,
		R5  = RegisterCount - 11,
		R4  = RegisterCount - 12,
		R3  = RegisterCount - 13,
		R2  = RegisterCount - 14,
		R1  = RegisterCount - 15,
		R0  = RegisterCount - 16,
		InstructionPointer = R15,
		StackPointer = R14,
		ConditionRegister = R13,
		AddressRegister = R12,
		ValueRegister = R11,
		MaskRegister = R10,
		ShiftRegister = R9,
		FieldRegister = R9,
	};
} // end namespace cisc0

#include "cisc0_defines.h"

namespace cisc0 {
	class DecodedInstruction {
		public:
            using BranchFlags = std::tuple<bool, bool, bool>;
        private:
            static constexpr bool hasBitmask(Operation op) noexcept;
            static constexpr bool hasImmediateFlag(Operation op) noexcept;
            static constexpr bool hasImmediateValue(Operation op) noexcept;
            static constexpr bool hasSubtype(Operation op) noexcept;
        public:
			DecodedInstruction(RawInstruction input) noexcept : _rawValue(input) { }
			DecodedInstruction(const DecodedInstruction&) = delete;
            virtual ~DecodedInstruction() { }
			RawInstruction getRawValue() const noexcept { return _rawValue; }
            inline byte getUpper() const noexcept { return decodeUpper(_rawValue); }
            inline Operation getControl() const noexcept { return decodeControl(_rawValue); }
            inline byte getSetDestination() const noexcept { return decodeSetDestination(_rawValue); }
            inline byte getMemoryRegister() const noexcept { return decodeMemoryRegister(_rawValue); }
            inline byte getBranchIndirectDestination() const noexcept { return decodeBranchIndirectDestination(_rawValue); }
            inline bool shouldShiftLeft() const noexcept { return decodeShiftFlagLeft(_rawValue); }
            inline bool isIndirectOperation() const noexcept { return decodeMemoryFlagIndirect(_rawValue); }
            inline byte getMemoryOffset() const noexcept { return decodeMemoryOffset(_rawValue); }
            BranchFlags getOtherBranchFlags() const noexcept;
            inline EncodingOperation getEncodingOperation() const noexcept { return decodeComplexClassEncoding_Type(_rawValue); }

            template<int index>
            inline byte getShiftRegister() const noexcept {
                static_assert(index >= 0 && index < 2, "Illegal shift register index!");
                if (index == 0) {
                    return decodeShiftRegister0(_rawValue);
                } else {
                    return decodeShiftRegister1(_rawValue);
                }
            }
            template<int index>
            inline byte getMoveRegister() const noexcept {
                static_assert(index >= 0 && index < 2, "Illegal move register index!");
                if (index == 0) {
                    return decodeMoveRegister0(_rawValue);
                } else {
                    return decodeMoveRegister1(_rawValue);
                }
            }
            template<int index>
            inline byte getSwapRegister() const noexcept {
                static_assert(index >= 0 && index < 2, "Illegal swap register index!");
                if (index == 0) {
                    return decodeSwapDestination(_rawValue);
                } else {
                    return decodeSwapSource(_rawValue);
                }
            }
            template<int index>
            inline byte getCompareRegister() const noexcept {
                static_assert(index >= 0 && index < 2, "Illegal compare register index!");
                if (index == 0) {
                    return decodeCompareRegister0(_rawValue);
                } else {
                    return decodeCompareRegister1(_rawValue);
                }
            }
            template<int index>
            inline byte getArithmeticRegister() const noexcept {
                static_assert(index >= 0 && index < 2, "Illegal arithmetic register index!");
                if (index == 0) {
                    return decodeArithmeticDestination(_rawValue);
                } else {
                    return decodeArithmeticSource(_rawValue);
                }
            }
            template<Operation op>
            inline byte getBitmask() const noexcept {
                static_assert(hasBitmask(op), "provided operation does not use a bitmask!");
                switch(op) {
                    case Operation::Set:
                        return decodeSetBitmask(_rawValue);
                    case Operation::Memory:
                        return decodeMemoryFlagBitmask(_rawValue);
                    case Operation::Move:
                        return decodeMoveBitmask(_rawValue);
                    case Operation::Logical:
                        return decodeLogicalFlagImmediateMask(_rawValue);
                    default:
                        return 0;
                }
            }
            template<Operation op>
            inline bool getImmediateFlag() const noexcept {
                static_assert(hasImmediateFlag(op), "provided operation does not have an immediate flag!");
                switch(op) {
                    case Operation::Shift:
                        return decodeShiftFlagImmediate(_rawValue);
                    case Operation::Arithmetic:
                        return decodeArithmeticFlagImmediate(_rawValue);
                    case Operation::Logical:
                        return decodeLogicalFlagImmediate(_rawValue);
                    case Operation::Branch:
                        return decodeBranchFlagIsImmediate(_rawValue);
                    case Operation::Compare:
                        return decodeCompareImmediateFlag(_rawValue);
                    default:
                        return false;
                }
            }
            template<Operation op>
            inline byte getImmediate() const noexcept {
                static_assert(hasImmediateValue(op), "provided operation cannot contain an immediate value!");
                switch(op) {
                    case Operation::Shift:
                        return decodeShiftImmediate(_rawValue);
                    case Operation::Arithmetic:
                        return decodeArithmeticImmediate(_rawValue);
                    default:
                        return 0;
                }
            }
            template<int index>
            inline byte getLogicalRegister() const noexcept {
                static_assert(index >= 0 && index < 2, "Illegal logical register index!");
                if (index == 0) {
                    if (getImmediateFlag<Operation::Logical>()) {
                        return decodeLogicalImmediateDestination(_rawValue);
                    } else {
                        return decodeLogicalRegister0(_rawValue);
                    }
                } else {
                    if (getImmediateFlag<Operation::Logical>()) {
                        throw syn::Problem("There is no second register argument for an immediate logical operation!");
                    } else {
                        return decodeLogicalRegister1(_rawValue);
                    }
                }
            }
            template<bool path>
            inline byte getBranchIfPathRegister() const noexcept {
                if (path) {
                    return decodeBranchIfOnTrue(_rawValue);
                } else {
                    return decodeBranchIfOnFalse(_rawValue);
                }
            }
            template<Operation op>
            struct SubtypeConversion {
                SubtypeConversion() = delete;
                SubtypeConversion(const SubtypeConversion&) = delete;
                SubtypeConversion(SubtypeConversion&&) = delete;
                ~SubtypeConversion() = delete;
            };
            template<Operation op>
            using SubtypeType = typename SubtypeConversion<op>::ResultantType;
            template<Operation op>
            inline SubtypeType<op> getSubtype() const noexcept {
                static_assert(hasSubtype(op), "There is no subtype for the given operation!");
                using CurrentType = SubtypeType<op>;
                switch(op) {
                    case Operation::Compare:
                        return static_cast<CurrentType>(decodeCompareType(_rawValue));
                    case Operation::Arithmetic:
                        return static_cast<CurrentType>(decodeArithmeticFlagType(_rawValue));
                    case Operation::Memory:
                        return static_cast<CurrentType>(decodeCompareType(_rawValue));
                    case Operation::Complex:
                        return static_cast<CurrentType>(decodeComplexSubClass(_rawValue));
                    case Operation::Logical:
                        return static_cast<CurrentType>(decodeLogicalFlagType(_rawValue));
                    default:
                        return static_cast<CurrentType>(0);
                }
            }
		private:
			RawInstruction _rawValue;
	};


	class Core : public syn::Core {
		public:
			using IOBus = syn::CLIPSIOController<Word, CLIPSInteger>;
            using RegisterFile = syn::FixedSizeLoadStoreUnit<RegisterValue, byte, ArchitectureConstants::RegisterCount>;
        public:
			static Core* make() noexcept;
		public:
			Core() noexcept;
			virtual ~Core() noexcept;
			virtual void initialize() override;
			virtual void shutdown() override;
			virtual bool cycle() override;
			bool shouldExecute() const noexcept { return execute; }
			bool handleOperation(void* env, CLIPSValue* ret);
		private:
			void pushWord(Word value);
            void pushWord(Word value, RegisterValue& ptr);
			void pushDword(DWord value);
            void pushDword(DWord value, RegisterValue& ptr);
			Word popWord();
			Word popWord(RegisterValue& ptr);
			void dispatch(DecodedInstruction&& inst);
			template<byte rindex>
				inline RegisterValue& registerValue() noexcept {
					static_assert(rindex < ArchitectureConstants::RegisterCount, "Not a legal register index!");
					return gpr[rindex];
				}
            template<bool readNext>
            inline Word tryReadNext() noexcept {
                if (readNext) {
                    incrementInstructionPointer();
                    return getCurrentCodeWord();
                } else {
                    return 0;
                }
            }
            Word tryReadNext(bool readNext) noexcept;
			RegisterValue retrieveImmediate(byte bitmask) noexcept;

			RegisterValue& registerValue(byte index);
			RegisterValue& getInstructionPointer() noexcept     { return registerValue<ArchitectureConstants::InstructionPointer>(); }
			RegisterValue& getStackPointer() noexcept           { return registerValue<ArchitectureConstants::StackPointer>(); }
			RegisterValue& getConditionRegister() noexcept      { return registerValue<ArchitectureConstants::ConditionRegister>(); }
			RegisterValue& getAddressRegister() noexcept        { return registerValue<ArchitectureConstants::AddressRegister>(); }
			RegisterValue& getValueRegister() noexcept          { return registerValue<ArchitectureConstants::ValueRegister>(); }
			RegisterValue& getMaskRegister() noexcept           { return registerValue<ArchitectureConstants::MaskRegister>(); }

			RegisterValue getShiftRegister() noexcept           { return 0b11111 & registerValue<ArchitectureConstants::ShiftRegister>(); }
			RegisterValue getFieldRegister() noexcept           { return 0b11111 & registerValue<ArchitectureConstants::FieldRegister>(); }

			void incrementInstructionPointer() noexcept;
			void incrementStackPointer() noexcept;
			void decrementStackPointer() noexcept;
			void decrementStackPointer(RegisterValue& ptr) noexcept;
			void incrementStackPointer(RegisterValue& ptr) noexcept;
			void incrementAddress(RegisterValue& ptr) noexcept;
			void decrementAddress(RegisterValue& ptr) noexcept;
			Word getCurrentCodeWord() noexcept;
			void storeWord(RegisterValue address, Word value);
			Word loadWord(RegisterValue address);
			RegisterValue loadRegisterValue(RegisterValue address);
			void storeRegisterValue(RegisterValue address, RegisterValue value);
		private:
			void complexOperation(DecodedInstruction&& inst);
			void encodingOperation(DecodedInstruction&& inst);
			void performEncodeOp(DecodedInstruction&& inst);
        private:
            void compareOperation(DecodedInstruction&& inst);
            void systemCallOperation(DecodedInstruction&& inst);
            void branchOperation(DecodedInstruction&& inst);
            void memoryOperation(DecodedInstruction&& inst);
            void logicalOperation(DecodedInstruction&& inst);
            void arithmeticOperation(DecodedInstruction&& inst);
            void shiftOperation(DecodedInstruction&& inst);
		private:
			bool execute = true,
				 advanceIp = true;
			RegisterFile gpr;
			IOBus _bus;
	};


	struct InstructionEncoder {
		using Encoding = std::tuple<int, Word, Word, Word>;
		int currentLine;
		RegisterValue address;
		Operation type;
		bool immediate;
		bool shiftLeft;
		bool isIf;
		bool isCall;
		bool isConditional;
		bool indirect;
		bool readNextWord;
		byte bitmask;
		byte arg0;
		byte arg1;
		byte arg2;
		bool isLabel;
		std::string labelValue;
		byte subType;
		RegisterValue fullImmediate;
		int numWords() const;
		Encoding encode() const;
		void clear();
		private:
            Encoding encodeMemory() const;
            Encoding encodeArithmetic() const;
            Encoding encodeShift() const;
            Encoding encodeLogical() const;
            Encoding encodeCompare() const;
            Encoding encodeBranch() const;
            Encoding encodeMove() const;
            Encoding encodeSet() const;
            Encoding encodeSwap() const;
            Encoding encodeComplex() const;
	};
#define DefSubtypeConversion(op, type) \
    template<> \
    struct DecodedInstruction::SubtypeConversion<Operation:: op > { \
                SubtypeConversion() = delete; \
                SubtypeConversion(const SubtypeConversion&) = delete; \
                SubtypeConversion(SubtypeConversion&&) = delete; \
                ~SubtypeConversion() = delete; \
        using ResultantType = type; \
    }
DefSubtypeConversion(Compare, CompareStyle);
DefSubtypeConversion(Arithmetic, ArithmeticOps);
DefSubtypeConversion(Memory, MemoryOperation);
DefSubtypeConversion(Complex, ComplexSubTypes);
DefSubtypeConversion(Logical, LogicalOps);
#undef DefSubtypeConversion
} // end namespace cisc0

#endif // end _TARGET_CISC0_IRIS_H
