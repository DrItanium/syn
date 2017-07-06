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


#ifndef _TARGET_CISC0_MODEL1_IRIS_H
#define _TARGET_CISC0_MODEL1_IRIS_H
#include <sstream>
#include <memory>
#include <vector>
#include <tuple>

#include "Problem.h"
#include "Base.h"
#include "ClipsCore.h"
#include "ExecutionUnits.h"
#include "IODevice.h"
#include "IOController.h"
#include "Cisc0Core.h"
#include "Cisc0CoreConstants.h"
#include "Cisc0CoreDecodedInstruction.h"


namespace cisc0 {
    /**
     * A reimplementation of the cisc0 instruction set with modifications to
     * the internals!
     */
	class CoreModel1 : public Core {
		public:
			using IOBus = syn::CLIPSIOController<Word, CLIPSInteger>;
            using RegisterFile = syn::FixedSizeLoadStoreUnit<RegisterValue, byte, ArchitectureConstants::RegisterCount>;
        public:
			static CoreModel1* make() noexcept;
		public:
			CoreModel1() noexcept;
			virtual ~CoreModel1() noexcept;
			virtual void initialize() override;
			virtual void shutdown() override;
			virtual bool cycle() override;
			bool shouldExecute() const noexcept { return execute; }
		private:
			void dispatch(const DecodedInstruction& inst);
			template<byte rindex>
			inline RegisterValue& registerValue() noexcept {
				static_assert(rindex < ArchitectureConstants::RegisterCount, "Not a legal register index!");
				return gpr[rindex];
			}
            template<bool readNext>
            inline Word tryReadNext() {
                if (!readNext) {
                    return 0;
                }
                incrementInstructionPointer();
                return getCurrentCodeWord();
            }
            Word tryReadNext(bool readNext);
			RegisterValue retrieveImmediate(byte bitmask) noexcept;

			RegisterValue& registerValue(byte index) override;
			RegisterValue& getInstructionPointer() noexcept override     { return registerValue<ArchitectureConstants::InstructionPointer>(); }
			RegisterValue& getStackPointer() noexcept override           { return registerValue<ArchitectureConstants::StackPointer>(); }
			RegisterValue& getCallStackPointer() noexcept 		         { return registerValue<ArchitectureConstants::CallStackPointer>(); }
			bool& getConditionRegister() noexcept 				         { return conditionRegister; }
			RegisterValue& getAddressRegister() noexcept                 { return registerValue<ArchitectureConstants::AddressRegister>(); }
			RegisterValue& getValueRegister() noexcept                   { return registerValue<ArchitectureConstants::ValueRegister>(); }
			RegisterValue& getMaskRegister() noexcept                    { return registerValue<ArchitectureConstants::MaskRegister>(); }

			RegisterValue getShiftRegister() noexcept           { return 0b11111 & registerValue<ArchitectureConstants::ShiftRegister>(); }
			RegisterValue getFieldRegister() noexcept           { return 0b11111 & registerValue<ArchitectureConstants::FieldRegister>(); }

			Word getCurrentCodeWord();
			virtual void storeWord(RegisterValue address, Word value) override;
			virtual Word loadWord(RegisterValue address) override;
		private:
			void complexOperation(const DecodedInstruction& inst);
			void encodingOperation(const DecodedInstruction& inst);
            void extendedOperation(const DecodedInstruction& inst);
			void parsingOperation(const DecodedInstruction& inst);
			void performEncodeOp(const DecodedInstruction& inst);
        private:
            void compareOperation(const DecodedInstruction& inst);
            void systemCallOperation(const DecodedInstruction& inst);
            void branchOperation(const DecodedInstruction& inst);
            void memoryOperation(const DecodedInstruction& inst);
            void logicalOperation(const DecodedInstruction& inst);
            void arithmeticOperation(const DecodedInstruction& inst);
            void shiftOperation(const DecodedInstruction& inst);
		private:
            // The actual instruction is four words wide but instructions are
            // variable width up four words! It is up to the internal code to
            // increment the address pointer as we see fit! The internal is
            // only aware of this fact, the external instruction set is not
            // aware of this fact!
			bool execute = true,
				 advanceIp = true;
			bool conditionRegister = false;
			RegisterFile gpr;
			IOBus _bus;
	};


} // end namespace cisc0

#endif // end _TARGET_CISC0_MODEL1_IRIS_H
