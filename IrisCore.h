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


#ifndef _TARGET_IRIS16_IRIS_H
#define _TARGET_IRIS16_IRIS_H
#include "Base.h"
#include "ExecutionUnits.h"
#include "Core.h"
#include "IOController.h"
#include <cstdint>
#include <memory>
#include "IrisCoreTypes.h"
#include "ClipsExtensions.h"
namespace iris {
	enum ArchitectureConstants  {
		RegisterCount = 256,
		AddressMax = 0xFFFF,
        AddressCount = AddressMax + 1,
		RegisterMax = 0xFF,
		ConditionRegisterCount = 16,
		StackPointerIndex = RegisterCount - 1,
		MaxGroups = 8,
		MaxOperations = 32,
		ErrorDispatchVectorBase = 0x00FF,
		RegistersToSaveOnError = 18,
		ErrorRegisterStart = 255,
        TerminateIOAddress = 0xFFFF,
	};
} // end namespace iris
#include "iris_defines.h"

namespace iris {
	using IOSpace = syn::CLIPSIOController<word, CLIPSInteger>;
	template<dword capacity>
	using WordMemorySpace = syn::FixedSizeLoadStoreUnit<word, dword, capacity>;
	using WordMemorySpace64k = WordMemorySpace<ArchitectureConstants::AddressMax + 1>;
	using ALU = syn::ALU<word>;
	using CompareUnit = syn::Comparator<word>;
	using RegisterFile = WordMemorySpace<ArchitectureConstants::RegisterCount>;
	using IODevice = syn::IODevice<word>;
	using PredicateRegisterFile = syn::FixedSizeLoadStoreUnit<bool, byte, ArchitectureConstants::ConditionRegisterCount>;
	using PredicateComparator = syn::Comparator<bool, bool>;
	using ErrorStorage = WordMemorySpace<ArchitectureConstants::RegistersToSaveOnError>;
    using InstructionPointer = syn::Register<QuadWord, ArchitectureConstants::AddressMax>;
    using LinkRegister = syn::Register<QuadWord, ArchitectureConstants::AddressMax>;
    struct InstructionDecoder {
        InstructionDecoder() = delete;
        ~InstructionDecoder() = delete;
        InstructionDecoder(const InstructionDecoder&) = delete;
        InstructionDecoder(InstructionDecoder&&) = delete;
        static inline constexpr byte getDestinationIndex(raw_instruction value) noexcept { return decodeDestination(value); }
        static inline constexpr byte getSource0Index(raw_instruction value) noexcept { return decodeSource0(value); }
        static inline constexpr byte getSource1Index(raw_instruction value) noexcept { return decodeSource1(value); }
        static inline constexpr byte getOperationByte(raw_instruction value) noexcept { return decodeOperation(value); }
        static inline constexpr byte getGroupByte(raw_instruction value) noexcept { return decodeGroup(value); }
        static inline constexpr InstructionGroup getGroup(raw_instruction value) noexcept { return static_cast<InstructionGroup>(getGroupByte(value)); }
        static inline constexpr word getHalfImmediate(raw_instruction value) noexcept { return decodeHalfImmediate(value); }
        static inline constexpr word getImmediate(raw_instruction value) noexcept { return decodeImmediate(value); }
        template<typename T>
        static inline constexpr T getOperation(raw_instruction value) noexcept {
            return static_cast<T>(getOperationByte(value));
        }
        template<int index>
        static inline constexpr byte getRegisterIndex(raw_instruction value) noexcept {
            static_assert(index >= 0 && index < 3, "Illegal register index!");
            if (index == 0) {
                return getDestinationIndex(value);
            } else if (index == 1) {
                return getSource0Index(value);
            } else {
                return getSource1Index(value);
            }
        }
        static inline constexpr byte getPredicateResultIndex(raw_instruction value) noexcept { return decodePredicateResult(value); }
        static inline constexpr byte getPredicateInverseResultIndex(raw_instruction value) noexcept { return decodePredicateInverseResult(value); }
        static inline constexpr byte getPredicateSource0Index(raw_instruction value) noexcept { return decodePredicateSource0(value); }
        static inline constexpr byte getPredicateSource1Index(raw_instruction value) noexcept { return decodePredicateSource1(value); }
        template<int index>
        static inline constexpr byte getPredicateIndex(raw_instruction value) noexcept {
            static_assert(index >= 0 && index < 4, "Illegal predicate field index!");
            switch(index) {
                case 0: return getPredicateResultIndex(value);
                case 1: return getPredicateInverseResultIndex(value);
                case 2: return getPredicateSource0Index(value);
                case 3: return getPredicateSource1Index(value);
                default:
                    throw syn::Problem("Illegal index!!!!");
            }
        }
    };
	class Core : public syn::Core {
		public:
			Core() noexcept;
			virtual ~Core();
			virtual void initialize() override;
			virtual void installprogram(std::istream& stream) override;
			virtual void shutdown() override;
			virtual void dump(std::ostream& stream) override;
			virtual void link(std::istream& input) override;
			inline void writeInstructionMemory(word address, dword value) noexcept { instruction[address] = value; }
			inline void writeDataMemory(word address, word value) noexcept         { data[address] = value; }
			inline dword readInstructionMemory(word address) noexcept             { return instruction[address]; }
			inline word readDataMemory(word address) noexcept                     { return data[address]; }
			void writeIOMemory(word address, word value);
			word readIOMemory(word address);
			//void installIODevice(std::shared_ptr<IODevice> dev);
			void writeRegister(byte index, word value);
			word readRegister(byte index);
			virtual bool cycle() override;
		private:
            QuadWord getInstructionPointer() const noexcept { return _ip.get(); }
            void setInstructionPointer(QuadWord value) noexcept { _ip.set(value); }
            QuadWord getLinkRegister() const noexcept { return _lr.get(); }
            void setLinkRegister(QuadWord value) noexcept { _lr.set(value); }
			bool& getPredicateRegister(byte index);
            void incrementInstructionPointer() noexcept { setInstructionPointer(getInstructionPointer() + 1); }

		private:
			void dispatch() noexcept;
            template<int index>
            inline word& getRegister() noexcept {
                return gpr[InstructionDecoder::getRegisterIndex<index>(current)];
            }
            inline word& destinationRegister() noexcept { return getRegister<0>(); }
            inline word& source0Register() noexcept { return getRegister<1>(); }
            inline word& source1Register() noexcept { return getRegister<2>(); }

            template<int index>
            inline bool& getPredicate() noexcept {
                return getPredicateRegister(InstructionDecoder::getPredicateIndex<index>(current));
            }
			inline bool& predicateResult() noexcept { return getPredicate<0>(); }
			inline bool& predicateInverseResult() noexcept { return getPredicate<1>(); }
			inline bool& predicateSource0() noexcept { return getPredicate<2>(); }
			inline bool& predicateSource1() noexcept { return getPredicate<3>(); }

		private:
			void saveSystemState() noexcept;
			void restoreSystemState() noexcept;
			void dispatchInterruptHandler();
            void ioSpaceWrite(word address, word value) noexcept;
            word ioSpaceRead(word address) noexcept;
		private:
			template<typename Unit>
			void performOperation(Unit& unit, typename Unit::Operation op, bool immediate) {
				destinationRegister() = unit(op, source0Register(), (immediate ? InstructionDecoder::getHalfImmediate(current) : source1Register()));
			}
			template<typename Unit>
			inline void performOperation(Unit& unit, std::tuple<typename Unit::Operation, bool>& tuple) {
				typename Unit::Operation op;
				bool immediate = false;
				std::tie(op, immediate) = tuple;
				performOperation(unit, op, immediate);
			}
			template<word index>
			struct PredicateRegisterEncoder {
				static_assert(index < 16, "Provided predicate register is out of range!");
				static PredicateRegisterEncoder<index - 1> next;
				static word invoke(Core* c, word mask) {
					auto result = next.invoke(c, mask);
					if (syn::getBit<word,index>(mask)) {
						return syn::setBit<word, index>(result, c->getPredicateRegister(index));
					} else {
						return result;
					}
				}
			};
			template<word index>
			struct PredicateRegisterDecoder {
				static_assert(index < 16, "Provided predicate register index is too large!");
				static PredicateRegisterDecoder<index - 1> next;
				static void invoke(Core* c, word input, word mask) noexcept {
					if (syn::getBit<word, index>(mask)) {
						c->getPredicateRegister(index) = syn::getBit<word, index>(input);
					}
					next.invoke(c, input, mask);
				}
			};
			void restorePredicateRegisters(word input, word mask) noexcept;
			word savePredicateRegisters(word mask) noexcept;

		private:
			bool execute;
			bool advanceIp;
			raw_instruction current;
            InstructionPointer _ip;
            LinkRegister _lr;
			word _error;
			IOSpace _io;
			CompareUnit _compare;
			ALU _alu;
			RegisterFile gpr;
			WordMemorySpace64k data;
			syn::FixedSizeLoadStoreUnit<dword, word, ArchitectureConstants::AddressMax> instruction;
			WordMemorySpace64k stack;
			PredicateRegisterFile _cr;
			PredicateComparator _pcompare;
			ErrorStorage _onError;
			bool _saveAdvanceIp = false;
			bool _saveExecute = false;
            bool _inInterruptHandler = false;
	};
	template<>
		struct Core::PredicateRegisterEncoder<0> {
			static word invoke(Core* c, word mask) {
				if (syn::getBit<word, 0>(mask)) {
					return static_cast<word>(c->getPredicateRegister(0) ? 1 : 0);
				} else {
					return 0;
				}
			}
		};
	template<>
		struct Core::PredicateRegisterDecoder<0> {
			static void invoke(Core* c, word input, word mask) noexcept {
				if (syn::getBit<word, 0>(mask)) {
					c->getPredicateRegister(0) = syn::getBit<word, 0>(input);
				}
			}
		};
	template<typename Data, typename Address>
	class ExposedCoreDataMemory : public syn::AddressableIODevice<Data, Address> {
		public:
            using Self = ExposedCoreDataMemory<Data, Address>;
            using Parent = syn::AddressableIODevice<Data, Address>;
			static constexpr Address computeScaleFactor() noexcept {
				return sizeof(Data) / sizeof(word);
			}
			static constexpr Address computeDataLength() noexcept {
				return (ArchitectureConstants::AddressMax + 1) / computeScaleFactor();
			}
			static constexpr Address computeDataMemoryEnd() noexcept {
				return computeDataLength() - 1;
			}
			static constexpr word computeScaledInternalAddress(Address addr) noexcept {
				return static_cast<word>(addr * computeScaleFactor());
			}
        public:
			ExposedCoreDataMemory(Core* core, Address base, Address length = computeDataLength()) : Parent(base, length), _core(core) { }
			virtual ~ExposedCoreDataMemory() { }
			virtual void write(Address address, Data value) override {
				auto addr = computeScaledInternalAddress(tryComputeActualAddress(address));
				static_assert(computeScaleFactor() != 0, "The size of the provided data element is smaller than the iris word! Please provide a custom implementation of write!");
				static_assert(computeScaleFactor() <= 4, "The size of the provided data value is too large for the default write implementation! Please provide a custom implementation of write!");
				switch(computeScaleFactor()) {
					case 4:
						_core->writeDataMemory(addr + 3, syn::decodeBits<Data, word, static_cast<Data>(0xFFFF000000000000), 48>(value));
					case 3:
						_core->writeDataMemory(addr + 2, syn::decodeBits<Data, word, static_cast<Data>(0x0000FFFF00000000), 32>(value));
					case 2:
						_core->writeDataMemory(addr + 1, syn::decodeBits<Data, word, static_cast<Data>(0x00000000FFFF0000), 16>(value));
					case 1:
						_core->writeDataMemory(addr, syn::decodeBits<Data, word, static_cast<Data>(0x000000000000FFFF), 0>(value));
						break;
					case 0:
						throw syn::Problem("Can't have a scale factor of zero!");
					default:
						throw syn::Problem("Illegal scale factor, please make a custom implementation!");
				}
				_core->writeDataMemory(static_cast<word>(tryComputeActualAddress(address)), value);
			}
			virtual Data read(Address address) override {
				// get the address factor computed
				auto addr = computeScaledInternalAddress(tryComputeActualAddress(address));
				static_assert(computeScaleFactor() != 0, "The size of the provided data element is smaller than the iris word! Please provide a custom implementation of read!");
				static_assert(computeScaleFactor() <= 4, "The size of the provided data value is too large for the default read implementation! Please provide a custom implementation of read!");
				if (computeScaleFactor() == 1) {
					return static_cast<Data>(_core->readDataMemory(addr));
				} else if (computeScaleFactor() == 2) {
					return syn::encodeValueLE<Data>(_core->readDataMemory(addr), _core->readDataMemory(addr + 1));
				} else if (computeScaleFactor() == 3) {
					return syn::encodeValueLE<Data>(_core->readDataMemory(addr), _core->readDataMemory(addr + 1), _core->readDataMemory(addr + 2), 0);
				} else if (computeScaleFactor() == 4) {
					return syn::encodeValueLE<Data>(_core->readDataMemory(addr), _core->readDataMemory(addr + 1), _core->readDataMemory(addr + 2), _core->readDataMemory(addr + 3));
				} else {
					throw syn::Problem("Please provide a custom implementation of write!");
				}
			}
		private:
			Address tryComputeActualAddress(Address address) {
				auto actualAddress = address - this->baseAddress();
				if (actualAddress < 0) {
					throw syn::Problem("Given address is less than the base address");
				} else if (actualAddress > computeDataMemoryEnd()) {
					throw syn::Problem("Given address is beyond the memory space!");
				} else {
					return actualAddress;
				}
			}
		private:
			Core* _core;
	};
    template<typename Data, typename Address = CLIPSInteger>
    using WrappedExposedDataCoreMemory = syn::WrappedIODevice<Data, Address, ExposedCoreDataMemory>;

	Core* newCore() noexcept;
	void assemble(const std::string& inputFileName, FILE* input, std::ostream* output);
    constexpr raw_instruction encodeInstruction(byte group, byte operation, byte dest, byte src0, byte src1) noexcept;
    constexpr raw_instruction encodeInstruction(byte group, byte operation, byte dest, word immediate) noexcept;
    template<bool upper>
    inline byte encode4Bits(byte dest, byte value) noexcept {
        if (upper) {
            return encodeUpper4Bits(dest, value);
        } else {
            return encodeLower4Bits(dest, value);
        }
    }

}
#endif
