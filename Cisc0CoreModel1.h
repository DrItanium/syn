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
            static constexpr auto instructionCacheWidth = 3;
		public:
			CoreModel1() noexcept;
			virtual ~CoreModel1() noexcept;
			virtual void initialize() override;
			virtual void shutdown() override;
			virtual bool cycle() override;
			bool shouldExecute() const noexcept { return execute; }
		private:
			void dispatch();
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
			Word getCurrentCodeWord(int offset = 0);
        protected:
			RegisterValue& registerValue(byte index) override;
			bool& getConditionRegister() noexcept override  { return conditionRegister; }

			virtual void storeWord(RegisterValue address, Word value) override;
			virtual Word loadWord(RegisterValue address) override;
		private:
            void moveToCondition(byte index) noexcept;
            void moveFromCondition(byte index) noexcept;
			void complexOperation();
			void encodingOperation();
            void extendedOperation();
			void parsingOperation();
			void performEncodeOp();
        private:
            void compareOperation();
            void systemCallOperation();
            void branchOperation();
            void memoryOperation();
            void logicalOperation();
            void arithmeticOperation();
            void shiftOperation();
		private:
            // The actual instruction is four words wide but instructions are
            // variable width up three words! It is up to the internal code to
            // increment the address pointer as we see fit! The internal is
            // only aware of this fact, the external instruction set is not
            // aware of this fact!
            // store three words worth of data!
			bool execute = true,
				 advanceIp = true;
			bool conditionRegister = false;
			RegisterFile _gpr;
			IOBus _bus;
            DecodedInstruction _instruction[instructionCacheWidth];
	};


} // end namespace cisc0

#endif // end _TARGET_CISC0_MODEL1_IRIS_H
