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
            using Parent = Core;
            static constexpr auto instructionCacheWidth = 3;
		public:
			CoreModel1() noexcept;
			virtual ~CoreModel1() noexcept;
			virtual void initialize() override;
			virtual void shutdown() override;
			virtual bool cycle() override;
		private:
			void dispatch();
			RegisterValue retrieveImmediate(byte bitmask) noexcept;
        protected:
			virtual RegisterValue& registerValue(byte index) override;
			virtual bool& getConditionRegister() noexcept override  { return conditionRegister; }
			virtual bool isTerminateAddress(RegisterValue address) const noexcept override;
		private:
            void moveToCondition(byte index) noexcept;
            void moveFromCondition(byte index) noexcept;
			void complexOperation();
			void encodingOperation();
            void extendedOperation();
			void parsingOperation();
			void performEncodeOp();
            void compareOperation();
            void systemCallOperation();
            void branchOperation();
            void memoryOperation();
            void logicalOperation();
            void arithmeticOperation();
            void shiftOperation();
			void featureCheckOperation();
            inline const DecodedInstruction& firstWord() const noexcept { return _instruction[0]; }
		private:
			bool conditionRegister = false;
			RegisterFile _gpr;
            // The actual instruction is four words wide but instructions are
            // variable width up three words! It is up to the internal code to
            // increment the address pointer as we see fit! The internal is
            // only aware of this fact, the external instruction set is not
            // aware of this fact!
            // store three words worth of data!
            DecodedInstruction _instruction[instructionCacheWidth];
			cisc0::Address _terminateAddress;
	};


} // end namespace cisc0

#endif // end _TARGET_CISC0_MODEL1_IRIS_H
