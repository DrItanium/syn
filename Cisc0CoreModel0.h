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


#ifndef _TARGET_CISC0_MODEL0_IRIS_H
#define _TARGET_CISC0_MODEL0_IRIS_H
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
#include "Cisc0CoreConstants.h"
#include "Cisc0CoreDecodedInstruction.h"
#include "Cisc0Core.h"

namespace cisc0 {
	class CoreModel0 : public Core {
        public:
            using Parent = Core;
		public:
			CoreModel0() noexcept;
			virtual ~CoreModel0() noexcept;
			virtual void initialize() override;
			virtual void shutdown() override;
			virtual bool cycle() override;
		private:
			void dispatch(const DecodedInstruction& inst);
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
			Word getCurrentCodeWord();
        protected:
			virtual RegisterValue& registerValue(byte index) override;
			bool& getConditionRegister() noexcept override { return conditionRegister; }
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
			bool conditionRegister = false;
			RegisterFile gpr;
	};


} // end namespace cisc0

#endif // end _TARGET_CISC0_MODEL0_IRIS_H
