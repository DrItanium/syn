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
     * cisc0 implementation that reads in three words at the beginning of the
     * cycle. This is a caching implementation which prevents memory from being
     * constantly hit in the cases of reading in immediate values.
     */
	class CoreModel1 : public Core {
		public:
            using Parent = Core;
            static constexpr auto instructionCacheWidth = 3;
            /**
             * Stores the a series of words from memory and treats them as
             * though they are the parts of a variable length instruction.
             */
            class FusedInstruction {
                public:
                    FusedInstruction(RegisterValue& ip, Word first = 0, Word second = 0, Word third = 0);
                    virtual ~FusedInstruction();
                    Operation getControl() const noexcept;
                    void reset(Word first, Word second, Word third) noexcept;
                    RegisterValue retrieveImmediate(byte bitmask) const noexcept;
                    const DecodedInstruction& firstWord() const noexcept { return _first; }
                public:
                    template<Operation op>
                    using DecodedReturnSubtype = typename cisc0::DecodeType<op>::ReturnType;
                    template<ComplexSubTypes op>
                    using DecodedReturnComplexSubtype = typename cisc0::DecodeComplexSubType<op>::ReturnType;
                    template<Operation op>
                    inline DecodedReturnSubtype<op> getSubtype() const noexcept {
                        return _first.getSubtype<op>();
                    }
                    template<ComplexSubTypes op>
                    inline DecodedReturnComplexSubtype<op> getSubtype() const noexcept {
                        return _first.getComplexSubType<op>();
                    }
                    template<Operation op>
                    inline byte getDestinationRegister() const noexcept {
                        return _first.getDestinationRegister<op>();
                    }
                    template<ComplexSubTypes op>
                    inline byte getDestinationRegister() const noexcept {
                        return _first.getDestinationRegister<op>();
                    }

                    template<Operation op>
                    inline byte getSourceRegister() const noexcept {
                        return _first.getSourceRegister<op>();
                    }

                    template<Operation op>
                    inline Word getImmediate() const noexcept {
                        return _first.getImmediate<op>();
                    }

                    template<Operation op>
                    inline bool isImmediate() const noexcept {
                        return _first.getImmediateFlag<op>();
                    }

                    template<Operation op>
                    inline byte getBitmask() const noexcept {
                        return _first.getBitmask<op>();
                    }
                public:
                    template<Operation op>
                    inline RegisterValue expandedBitmask() const noexcept {
                        return mask(getBitmask<op>());
                    }
                    template<Operation op>
                    inline RegisterValue retrieveImmediate(std::true_type) const noexcept {
                        return retrieveImmediate(getBitmask<op>());
                    }
                    template<Operation op>
                    inline RegisterValue retrieveImmediate(std::false_type) const noexcept {
                        return RegisterValue(getImmediate<op>());
                    }
                    template<Operation op>
                    inline RegisterValue retrieveImmediate() const noexcept {
                        using DispatchType = std::integral_constant<bool, !cisc0::DecodedInstruction::hasImmediateValue(op)>;
                        return retrieveImmediate<op>(DispatchType { });
                    }


                private:
                    DecodedInstruction _first, _second, _third;
                    RegisterValue& _ip;
            };
		public:
			CoreModel1() noexcept;
			virtual ~CoreModel1() noexcept;
			virtual void initialize() override;
			virtual bool cycle() override;
		private:
			void dispatch();
        protected:
			virtual bool isTerminateAddress(RegisterValue address) const noexcept override;
		private:
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



        private:
            template<Operation op>
            inline void setDestinationRegister(RegisterValue value) noexcept {
                auto index = _instruction.getDestinationRegister<op>();
                advanceIp = !isInstructionPointer(index);
                registerValue(index) = value;
            }
            template<Operation op>
            inline RegisterValue getDestinationRegister() noexcept {
                return registerValue(_instruction.getDestinationRegister<op>());
            }
            template<ComplexSubTypes op>
            inline RegisterValue getDestinationRegister() noexcept {
                return registerValue(_instruction.getDestinationRegister<op>());
            }
            template<Operation op>
            inline RegisterValue& sourceRegister() noexcept {
                return registerValue(_instruction.getSourceRegister<op>());
            }
            template<Operation op>
            inline RegisterValue retrieveSourceOrImmediate() noexcept {
                if (_instruction.isImmediate<op>()) {
                    return _instruction.retrieveImmediate<op>();
                } else {
                    return sourceRegister<op>();
                }
            }
            static constexpr bool isInstructionPointer(byte index) noexcept {
                return index == ArchitectureConstants::InstructionPointer;
            }
            template<Operation op>
            inline FusedInstruction::DecodedReturnSubtype<op> getSubtype() const noexcept {
                return _instruction.getSubtype<op>();
            }
            template<ComplexSubTypes op>
            inline FusedInstruction::DecodedReturnComplexSubtype<op> getSubtype() const noexcept {
                return _instruction.getSubtype<op>();
            }
		private:
			RegisterFile _gpr;
			cisc0::Address _terminateAddress;
            // The actual instruction is four words wide but instructions are
            // variable width up three words! It is up to the internal code to
            // increment the address pointer as we see fit! The internal is
            // only aware of this fact, the external instruction set is not
            // aware of this fact!
            // store three words worth of data!
            FusedInstruction _instruction;
	};


} // end namespace cisc0

#endif // end _TARGET_CISC0_MODEL1_IRIS_H
