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


#ifndef _TARGET_CISC0_INSTRUCTION_ENCODER_H
#define _TARGET_CISC0_INSTRUCTION_ENCODER_H
#include <tuple>

#include "Base.h"
#include "Cisc0CoreConstants.h"
#include "cisc0_defines.h"

namespace cisc0 {
	struct InstructionEncoder {
        struct Encoding {
            public:
                Encoding(int numWords, Word w0 = 0, Word w2 = 0, Word w3 = 0) noexcept;
                virtual ~Encoding() noexcept;
                int getNumWords() const noexcept { return _numWords; }
                Word getWord0() const noexcept { return _word0; }
                Word getWord1() const noexcept { return _word1; }
                Word getWord2() const noexcept { return _word2; }
            private:
                int _numWords;
                Word _word0;
                Word _word1;
                Word _word2;
        };
		int numWords() const;
		Encoding encode() const;
		void clear();
		template<Operation op>
		void setType() noexcept {
			_type = op;
		}
		void setShiftDirection(bool shouldShiftLeft) noexcept { _shiftLeft = shouldShiftLeft; }
		void markImmediate(bool isImmediate = true) noexcept { _immediate = isImmediate; }
		void markIndirect(bool isIndirect = true) noexcept { _indirect = isIndirect; }
		void markConditional() noexcept { _isConditional = true; }
		void markUnconditional() noexcept { _isConditional = false; }
		void markCall(bool isCallBranch = true) noexcept { _isCall = isCallBranch; }
		template<typename T>
		void setBitmask(T value) noexcept {
			_bitmask = static_cast<decltype(_bitmask)>(value);
		}
		void setBitmask(byte value) noexcept { _bitmask = value; }

        void setFirstArg(byte value) noexcept;
        void setSecondArg(byte value) noexcept;
        void setThirdArg(byte value) noexcept;

		RegisterValue getFullImmediate() const noexcept { return _fullImmediate; }

		template<typename T>
		void setSubType(T value) noexcept {
			_subType = static_cast<decltype(_subType)>(value);
		}

		void setSubType(byte value) noexcept { _subType = value; }
		void setAddress(RegisterValue addr) noexcept { _address = addr; }

		void setFullImmediate(RegisterValue val) noexcept { _fullImmediate = val; }
		void markAsLabel() noexcept { _isLabel = true; }
		void markAsNotLabel() noexcept { _isLabel = false; }
		void setLabelName(const std::string& name) noexcept {
			_labelValue = name;
			markAsLabel();
		}

		inline bool isLabel() const noexcept { return _isLabel; }
		const std::string& getLabelValue() const noexcept { return _labelValue; }

		RegisterValue getAddress() const noexcept { return _address; }

		private:
			template<Operation op>
			Word setFlagImmediate(Word value) const noexcept {
                static_assert(hasImmediateFlag<op>(), "Provided type does not support an immediate flag!");
                return cisc0::encodeGenericImmediateFlag(value, _immediate);
			}
			template<Operation op>
			Word setDestination(Word value) const noexcept {
                static_assert(usesDestination<op>(), "Provided operation does not support a destination register index!");
                return cisc0::encodeGenericDestination(value, _arg0);
			}
			template<Operation op>
			Word setSource(Word value) const noexcept {
				return cisc0::encodeSource<op, decltype(_arg1)>(value, _arg1);
			}
			template<Operation op>
			Word setSubType(Word value) const noexcept {
				return cisc0::encodeType<op, decltype(_subType)>(value, _subType);
			}
			template<ComplexSubTypes op>
			Word setSubType(Word value, byte t) const noexcept {
				return cisc0::encodeComplexSubType<op, byte>(value, t);
			}
			template<Operation op>
			Word setBitmask(Word value) const noexcept {
				return cisc0::encodeBitmask<op, decltype(_bitmask)>(value, _bitmask);
			}
            template<Operation op>
            Word setSourceAndDestination(Word value) const noexcept {
                return setSource<op>(setDestination<op>(value));
            }

            template<Operation op>
            Word setImmediateFlagAndDestination(Word value) const noexcept {
                return setDestination<op>(setFlagImmediate<op>(value));
            }
            template<Operation op>
            Word setImmediateFlagAndDestinationAndSource(Word value) const noexcept {
                return setSourceAndDestination<op>(setFlagImmediate<op>(value));
            }
			int instructionSizeFromBitmask() const noexcept;
			Word commonEncoding() const;
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
			Encoding encodeComplexExtended(Word input) const;
			Encoding encodeComplexEncoding(Word input) const;
			Encoding encodeComplexParsing(Word input) const;
			Encoding encodeReturn() const;
		private:
			int _currentLine;
			RegisterValue _address;
			Operation _type;
			bool _immediate;
			bool _shiftLeft;
			bool _isCall;
			bool _isConditional;
			bool _indirect;
			byte _bitmask;
			byte _arg0;
			byte _arg1;
			byte _arg2;
			bool _isLabel;
			std::string _labelValue;
			byte _subType;
			RegisterValue _fullImmediate;
	};
}
#endif // end _TARGET_CISC0_INSTRUCTION_ENCODER_H
