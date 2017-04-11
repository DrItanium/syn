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
	class Core : public syn::Core {
        public:
            static Core* make() noexcept;
            using IOSpace = syn::CLIPSIOController<word, CLIPSInteger>;
            template<dword capacity>
            using WordMemorySpace = syn::FixedSizeLoadStoreUnit<word, dword, capacity>;
            using WordMemorySpace64k = WordMemorySpace<ArchitectureConstants::AddressMax + 1>;
            using RegisterFile = WordMemorySpace<ArchitectureConstants::RegisterCount>;
            using IODevice = syn::IODevice<word>;
            using PredicateRegisterFile = syn::FixedSizeLoadStoreUnit<bool, byte, ArchitectureConstants::ConditionRegisterCount>;
            using ErrorStorage = WordMemorySpace<ArchitectureConstants::RegistersToSaveOnError>;
            using InstructionPointer = syn::Register<QuadWord, ArchitectureConstants::AddressMax>;
            using LinkRegister = syn::Register<QuadWord, ArchitectureConstants::AddressMax>;
            using PredicateRegisterBlock = syn::Register<word, AddressMax>;
		public:
			Core() noexcept;
			virtual ~Core();
			virtual void initialize() override;
			virtual void shutdown() override;
			virtual bool cycle() override;
			bool handleOperation(void* env, CLIPSValue* ret);
		private:
            inline QuadWord getInstructionPointer() const noexcept { return _ip.get(); }
            inline QuadWord getLinkRegister() const noexcept { return _lr.get(); }
            void setInstructionPointer(QuadWord value) noexcept;
            void setLinkRegister(QuadWord value) noexcept;
			bool getPredicateRegister(byte index) const;
            void setPredicateRegister(byte index, bool value);
            void incrementInstructionPointer() noexcept;

		private:
			void dispatch() noexcept;
            template<int index>
            inline word& getRegister() noexcept {
                return gpr[InstructionDecoder::getRegisterIndex<index>(current)];
            }
            template<int index>
            inline byte getPredicateIndex() const noexcept {
                return InstructionDecoder::getPredicateIndex<index>(current);
            }

            template<int index>
            inline bool getPredicate() const noexcept {
                return getPredicateRegister(getPredicateIndex<index>());
            }
            template<int index>
            inline void setPredicate(bool value) noexcept {
                setPredicateRegister(getPredicateIndex<index>(), value);
            }
            word& destinationRegister() noexcept;
            word& source0Register() noexcept;
            word& source1Register() noexcept;

            bool getPredicateResult() const noexcept;
			bool getPredicateInverseResult() const noexcept;
			bool getPredicateSource0() const noexcept;
			bool getPredicateSource1() const noexcept;
            byte getPredicateResultIndex() const noexcept;
			byte getPredicateInverseResultIndex() const noexcept;
			byte getPredicateSource0Index() const noexcept;
			byte getPredicateSource1Index() const noexcept;
            void setPredicateResult(bool value) noexcept;
			void setPredicateInverseResult(bool value) noexcept;
			void setPredicateSource0(bool value) noexcept;
			void setPredicateSource1(bool value) noexcept;
            word getHalfImmediate() const noexcept;
            word getImmediate() const noexcept;
		private:
			void saveSystemState() noexcept;
			void restoreSystemState() noexcept;
			void dispatchInterruptHandler();
            void ioSpaceWrite(word address, word value) noexcept;
            word ioSpaceRead(word address) noexcept;
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
			RegisterFile gpr;
			WordMemorySpace64k data;
			syn::FixedSizeLoadStoreUnit<dword, dword, ArchitectureConstants::AddressMax + 1> instruction;
			WordMemorySpace64k stack;
			PredicateRegisterBlock _cr;
			ErrorStorage _onError;
			bool _saveAdvanceIp = false;
			bool _saveExecute = false;
            bool _inInterruptHandler = false;
	};
}
#endif
