#ifndef __IRIS_INSTRUCTION_H
#define __IRIS_INSTRUCTION_H
#include "syn_base.h"
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
