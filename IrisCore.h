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
#include "IrisCoreEncodingOperations.h"

namespace iris {
	using IOSpace = syn::CLIPSIOController<word, CLIPSInteger>;
	template<dword capacity>
	using WordMemorySpace = syn::FixedSizeLoadStoreUnit<word, dword, capacity>;
	using WordMemorySpace64k = WordMemorySpace<ArchitectureConstants::AddressMax + 1>;
	using ALU = syn::ALU::Unit<word>;
	using CompareUnit = syn::Comparator::Unit<word>;
	using RegisterFile = WordMemorySpace<ArchitectureConstants::RegisterCount>;
	using IODevice = syn::IODevice<word>;
	using PredicateRegisterFile = syn::FixedSizeLoadStoreUnit<bool, byte, ArchitectureConstants::ConditionRegisterCount>;
	using PredicateComparator = syn::Comparator::Unit<bool, bool>;
	using ErrorStorage = WordMemorySpace<ArchitectureConstants::RegistersToSaveOnError>;
    using InstructionPointer = syn::Register<QuadWord, ArchitectureConstants::AddressMax>;
    using LinkRegister = syn::Register<QuadWord, ArchitectureConstants::AddressMax>;
	class Core : public syn::Core {
        public:
            static Core* make() noexcept;
		public:
			Core() noexcept;
			virtual ~Core();
			virtual void initialize() override;
			virtual void shutdown() override;
			inline void writeInstructionMemory(word address, dword value) noexcept { instruction[address] = value; }
			inline void writeDataMemory(word address, word value) noexcept         { data[address] = value; }
			inline dword readInstructionMemory(word address) noexcept             { return instruction[address]; }
			inline word readDataMemory(word address) noexcept                     { return data[address]; }
			void writeIOMemory(word address, word value);
			word readIOMemory(word address);
			void writeRegister(byte index, word value);
			word readRegister(byte index);
			virtual bool cycle() override;
			bool handleOperation(void* env, CLIPSValue* ret);
		private:
            inline QuadWord getInstructionPointer() const noexcept { return _ip.get(); }
            inline QuadWord getLinkRegister() const noexcept { return _lr.get(); }
            void setInstructionPointer(QuadWord value) noexcept;
            void setLinkRegister(QuadWord value) noexcept;
			bool& getPredicateRegister(byte index);
            void incrementInstructionPointer() noexcept;

		private:
			void dispatch() noexcept;
            template<int index>
            inline word& getRegister() noexcept {
                return gpr[InstructionDecoder::getRegisterIndex<index>(current)];
            }
            template<int index>
            inline bool& getPredicate() noexcept {
                return getPredicateRegister(InstructionDecoder::getPredicateIndex<index>(current));
            }
            word& destinationRegister() noexcept;
            word& source0Register() noexcept;
            word& source1Register() noexcept;
            bool& predicateResult() noexcept;
			bool& predicateInverseResult() noexcept;
			bool& predicateSource0() noexcept;
			bool& predicateSource1() noexcept;
            inline word getHalfImmediate() const noexcept { return InstructionDecoder::getHalfImmediate(current); }
            inline word getImmediate() const noexcept { return InstructionDecoder::getImmediate(current); }
		private:
			void saveSystemState() noexcept;
			void restoreSystemState() noexcept;
			void dispatchInterruptHandler();
            void ioSpaceWrite(word address, word value) noexcept;
            word ioSpaceRead(word address) noexcept;
			void handleCiscInstructions();
		private:
			template<typename Unit>
			void performOperation(Unit& unit, typename Unit::Operation op, bool immediate) {
				destinationRegister() = unit(op, source0Register(), (immediate ? getHalfImmediate() : source1Register()));
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
                PredicateRegisterEncoder() = delete;
                ~PredicateRegisterEncoder() = delete;
                PredicateRegisterEncoder(const PredicateRegisterEncoder&) = delete;
                PredicateRegisterEncoder(PredicateRegisterEncoder&&) = delete;
				static_assert(index < 16, "Provided predicate register is out of range!");
				static PredicateRegisterEncoder<index - 1> next;
				static word invoke(Core* c, word mask) noexcept {
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
                PredicateRegisterDecoder() = delete;
                ~PredicateRegisterDecoder() = delete;
                PredicateRegisterDecoder(const PredicateRegisterDecoder&) = delete;
                PredicateRegisterDecoder(PredicateRegisterDecoder&&) = delete;
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
                PredicateRegisterEncoder() = delete;
                ~PredicateRegisterEncoder() = delete;
                PredicateRegisterEncoder(const PredicateRegisterEncoder&) = delete;
                PredicateRegisterEncoder(PredicateRegisterEncoder&&) = delete;
			static word invoke(Core* c, word mask) noexcept {
				if (syn::getBit<word, 0>(mask)) {
					return static_cast<word>(c->getPredicateRegister(0) ? 1 : 0);
				} else {
					return 0;
				}
			}
		};
	template<>
		struct Core::PredicateRegisterDecoder<0> {
                PredicateRegisterDecoder() = delete;
                ~PredicateRegisterDecoder() = delete;
                PredicateRegisterDecoder(const PredicateRegisterDecoder&) = delete;
                PredicateRegisterDecoder(PredicateRegisterDecoder&&) = delete;
			static void invoke(Core* c, word input, word mask) noexcept {
				if (syn::getBit<word, 0>(mask)) {
					c->getPredicateRegister(0) = syn::getBit<word, 0>(input);
				}
			}
		};
}
#endif
