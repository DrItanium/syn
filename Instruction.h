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


#ifndef __IRIS_INSTRUCTION_H
#define __IRIS_INSTRUCTION_H
#include "Base.h"
namespace iris {
	template<typename Word, typename Operation>
	class Instruction {
		public:
			Instruction(Word value) : _rawValue(value) { }
			~Instruction() { }
			Word getRawValue() const noexcept { return _rawValue; }
			Operation getControl() const noexcept { return getControl_impl(); }
		protected:
			virtual Operation getControl_impl() const noexcept = 0;
		protected:
			Word _rawValue;
	};
	template<typename Word, typename Operation, typename RegisterIndex = byte>
	class RegisterInstruction : public Instruction<Word, Operation> {
		public:
			RegisterInstruction(Word value) : Instruction<Word, Operation>(value) { }
			~RegisterInstruction() { }
			RegisterIndex getRegister(int index) const { return getRegisterIndex(index); }
		protected:
			virtual RegisterIndex getRegisterIndex(int index) const = 0;
	};

}

#endif // end __IRIS_INSTRUCTION_H
