#include "iris17.h"
#include <functional>
#include <sstream>
#include "Problem.h"

namespace iris17 {

	Core* newCore() {
		return new Core();
	}
	word encodeWord(byte a, byte b) {
		return iris::encodeBits<word, byte, 0xFF00, 8>(iris::encodeBits<word, byte, 0x00FF>(word(0), a), b);
	}

	DecodedInstruction::DecodedInstruction() { }

	void DecodedInstruction::decode(raw_instruction input) {
#define X(title, mask, shift, type, is_register, post) \
		_ ## post = iris::decodeBits<raw_instruction, type, mask, shift>(input);
#include "iris17_instruction.def"
#undef X
	}

	Core::Core() { }
	void Core::setInstructionMemory(word address, dword value) {
		instruction[address] = value;
	}
	void Core::setDataMemory(word address, word value) {
		data[address] = value;
	}
	void Core::initialize() {

	}
	void Core::shutdown() {

	}
	template<int count>
	void populateContents(word* contents, std::istream& stream) {
		char buf[sizeof(word)] = { 0 };
		for(int i = 0; i < count; ++i) {
			stream.read(buf, sizeof(T));
			contents[i] = iris17::encodeWord(buf[0], buf[1]);
		}
	}
	void Core::installprogram(std::istream& stream) {
		populateContents<ArchitectureConstants::RegisterCount>(gpr, stream);
		populateContents<ArchitectureConstants::AddressMax>(data, stream);
		populateContents<ArchitectureConstants::AddressMax>(instruction, stream);
		populateContents<ArchitectureConstants::AddressMax>(stack, stream);
	}

	template<int count>
	void dumpContents(word* contents, std::ostream& stream) {
		char buf[sizeof(word)] = { 0 };
		for(int i = 0; i < count; ++i) {
			buf[0] = static_cast<char>(contents[i]);
			buf[1] = static_cast<char>(contents[i] >> 8);
			stream.write(buf, sizeof(word));
		}
	}
	void Core::dump(std::ostream& stream) {
		// save the registers
		dumpContents<ArchitectureConstants::RegisterCount>(gpr, stream);
		dumpContents<ArchitectureConstants::AddressMax>(data, stream);
		dumpContents<ArchitectureConstants::AddressMax>(instruction, stream);
		dumpContents<ArchitectureConstants::AddressMax>(stack, stream);
	}
	void Core::run() {
		while(execute) {
			if (!advanceIp) {
				advanceIp = true;
			}
			current.decode(instruction[registerValue<ArchitectureConstants::InstructionPointerIndex>()]);
			dispatch();
			if (advanceIp) {
				++registerValue<ArchitectureConstants::InstructionPointerIndex>();
			} 
		}
	}
	template<JumpOp op> 
		struct ConditionalStyle {
			static const bool isFalseForm = false;
		};
#define X(name, ifthenelse, conditional, iffalse, immediate, link) \
	template<> struct ConditionalStyle<JumpOp:: name> { static const bool isFalseForm = iffalse; };
#include "iris17_jump.def"
#undef X
	void Core::dispatch() {
		switch(current.getControl()) {
#define X(type, compare, mod) \
			case ControlSignature<InstructionGroup::Compare, GetAssociatedOp<InstructionGroup::Compare>::Association, GetAssociatedOp<InstructionGroup::Compare>::Association:: type>::fullSignature: \
								   getDestinationRegister() = (getSource0Register() compare getSource1Register()); \
			break;
#define Y(type, compare, mod) \
			case ControlSignature<InstructionGroup::Compare, GetAssociatedOp<InstructionGroup::Compare>::Association, GetAssociatedOp<InstructionGroup::Compare>::Association:: type>::fullSignature: \
								   getDestinationRegister() = (getSource0Register() compare static_cast<word>(current.getSource1())); \
			break;

#include "iris17_compare.def"
#undef X
#undef Y
//
#define XNone(n, op) getDestinationRegister() = ( getSource0Register() op  getSource1Register());
#define XImmediate(n, op) getDestinationRegister() = (getSource0Register() op static_cast<word>(getHalfImmediate())); 
#define XUnary(n, op) getDestinationRegister() = (op getSource0Register());
#define XDenominator(n, op) \
			if (getSource1Register() == 0) { \
				throw iris::Problem("denominator for operation " #n " is zero!"); \
				execute = false; \
			} else { \
				XNone(n, op) \
			}
#define XDenominatorImmediate(n, op) \
			if (getSource1Register() == 0) { \
				execute = false; \
				throw iris::Problem("denominator for operation " #n " is zero!"); \
			} else { \
				XImmediate(n, op) \
			}
#define X(name, op, desc) \
			case ControlSignature<InstructionGroup::Arithmetic, GetAssociatedOp<InstructionGroup::Arithmetic>::Association, GetAssociatedOp<InstructionGroup::Arithmetic>::Association:: name>::fullSignature: \
						{ \
							INDIRECTOR(X, desc)(name, op) \
							break; \
						}
#include "iris17_arithmetic.def"
#undef X
#undef XNone
#undef XDenominator
#undef XUnary
#undef XImmediate
#undef XDenominatorImmediate

#define XImmediateCond_true (current.getImmediate())
#define XImmediateCond_false (getSource0Register())
#define XIfThenElse_false(immediate) newAddr = cond ? INDIRECTOR(XImmediateCond, _ ## immediate) : ip + 1;
#define XIfThenElse_true(immediate) newAddr = cond ? getSource0Register() : getSource1Register() ;
#define XImmediateUncond_false (getDestinationRegister())
#define XImmediateUncond_true (current.getImmediate())
#define XConditional_false(name, ifthenelse, immediate) newAddr = INDIRECTOR(XImmediateUncond, _ ## immediate);
#define XConditional_true(name, ifthenelse, immediate) \
			cond = (ConditionalStyle<JumpOp:: name>::isFalseForm ? (getDestinationRegister() == 0) : (getDestinationRegister() != 0)); \
			INDIRECTOR(XIfThenElse, _ ## ifthenelse)(immediate)
#define XLink_true \
			if (cond) { \
				registerValue<ArchitectureConstants::LinkRegisterIndex>() = ip + 1; \
			}
#define XLink_false

#define X(name, ifthenelse, conditional, iffalse, immediate, link) \
			case ControlSignature<InstructionGroup::Jump, GetAssociatedOp<InstructionGroup::Jump>::Association, GetAssociatedOp<InstructionGroup::Jump>::Association:: name>::fullSignature: \
					 { \
						 word newAddr = 0; \
						 bool cond = true; \
						 advanceIp = false; \
						 word ip = registerValue<ArchitectureConstants::InstructionPointerIndex>(); \
						 INDIRECTOR(XConditional, _ ## conditional)(name, ifthenelse, immediate) \
						 registerValue<ArchitectureConstants::InstructionPointerIndex>() = newAddr; \
						 INDIRECTOR(XLink, _ ## link)  \
						 break; \
					 }
#include "iris17_jump.def"
#undef X
#undef XLink_true
#undef XLink_false
#undef XImmediateCond_true
#undef XImmediateCond_false
#undef XIfThenElse_false
#undef XIfThenElse_true
#undef XImmediateUncond_false 
#undef XImmediateUncond_true 
#undef XConditional_false
#undef XConditional_true
#define X(name, func) \
			case ControlSignature<InstructionGroup::Misc, GetAssociatedOp<InstructionGroup::Misc>::Association, GetAssociatedOp<InstructionGroup::Misc>::Association:: name>::fullSignature: \
			func (); \
			break;
#include "iris17_misc.def"
#undef X
#define GPRRegister0 (getDestinationRegister())
#define GPRRegister1 (getSource0Register())
#define GPRRegister2 (getSource1Register())
#define GPRImmediate1 (current.getImmediate())
#define DataRegister0 GPRRegister0
#define DataRegister1 GPRRegister1
#define DataImmediate1 GPRImmediate1
#define StackPushRegister0 (registerValue<ArchitectureConstants::StackPointerIndex>())
#define StackPushRegister1 GPRRegister0
#define StackPushImmediate1 GPRImmediate1
#define StackPopRegister0 GPRRegister0
#define StackPopRegister1 (registerValue<ArchitectureConstants::StackPointerIndex>())
#define StackPopImmediate1 GPRImmediate1
#define StoreRegister0  GPRRegister0
#define StoreRegister1 GPRRegister1
#define StoreImmediate1 GPRImmediate1
#define CodeRegister0 GPRRegister0
#define CodeUpperLowerRegisters1 GPRRegister1
#define CodeUpperLowerRegisters2 GPRRegister2

#define XLoadCode(type, dest, src) \
			auto result = instruction[INDIRECTOR(type, dest ## 0)]; \
			INDIRECTOR(type, src ## 1) = (word)result; \
			INDIRECTOR(type, src ## 2) = (word)(result >> 16);

#define XStoreCode(type, dest, src) \
			instruction[INDIRECTOR(type, dest ## 0)] = ((((dword)INDIRECTOR(type, src ## 2)) << 16) | ((dword)INDIRECTOR(type, src ## 1)));

#define XMove(type, dest, src) \
			INDIRECTOR(type, dest ## 0) = INDIRECTOR(type, src ## 1);
#define XSwap(type, dest, src) \
			auto a = INDIRECTOR(type, dest ##  0); \
			INDIRECTOR(type, dest ## 0) = INDIRECTOR(type, src ## 1); \
			INDIRECTOR(type, src ##  1) = a;
#define XLoad(type, dest, src) \
			INDIRECTOR(type, dest ## 0) = data[INDIRECTOR(type, src ## 1)];
#define XPop(type, dest, src) \
			INDIRECTOR(type, Pop ## dest ## 0) = stack[INDIRECTOR(type, Pop ## src ## 1)]; \
			--INDIRECTOR(type, Pop ## src ## 1);
#define XPush(type, dest, src) \
			++INDIRECTOR(type, Push ## dest ## 0); \
			stack[INDIRECTOR(type, Push ## dest ## 0)] = INDIRECTOR(type, Push ## src ## 1);
#define XStore(type, dest, src) \
			data[INDIRECTOR(type, dest ##  0)] = INDIRECTOR(type, src ## 1);
#define X(name, type, target, dest, src) \
			case ControlSignature<InstructionGroup::Move, GetAssociatedOp<InstructionGroup::Move>::Association, GetAssociatedOp<InstructionGroup::Move>::Association:: name>::fullSignature: \
					 { \
					 INDIRECTOR(X,type)(target, dest, src) \
			break; \
					 }
#include "iris17_move.def"
#undef X
#undef XMove
#undef XSwap
#undef XLoad
#undef XStore
#undef XPop
#undef XPush
#undef GPRRegister0
#undef GPRRegister1
#undef GPRImmediate1
#undef DataRegister0
#undef DataRegister1
#undef DataImmediate1
#undef StackPushRegister0
#undef StackPushRegister1
#undef StackPushImmediate1
#undef StackPopRegister0
#undef StackPopRegister1
#undef StackPopImmediate1
#undef StoreRegister0
#undef StoreRegister1
#undef StoreImmediate1
#undef XStoreCode
#undef XLoadCode
#undef CodeRegister0
#undef CodeUpperLowerRegisters1
#undef CodeUpperLowerRegisters2

			default:
				std::stringstream str;
				str << "Illegal instruction " << current.getGroup() << " " << current.getOperation();
				execute = false;
				throw iris::Problem(str.str());
		}
	}


	void Core::systemCall() {
		switch(static_cast<SystemCalls>(current.getDestination())) {
			case SystemCalls::Terminate:
				execute = false;
				advanceIp = false;
				break;
			case SystemCalls::PutC:
				// read register 0 and register 1
				std::cout.put((char)getSource0Register());
				break;
			case SystemCalls::GetC:
				byte value;
				std::cin >> std::noskipws >> value;
				getSource0Register() = (word)value;
				break;
			default:
				std::cerr << "Illegal system call " << current.getDestination() << std::endl;
				execute = false;
				advanceIp = false;
				break;
		}
	}
	enum class Segment  {
		Code, 
		Data,
		Count,
	};
	void Core::link(std::istream& input) {
		dword result = 0;
		word result0 = 0;
		char buf[8] = {0};
		for(int lineNumber = 0; input.good(); ++lineNumber) {
			input.read(buf, 8);
			if (input.gcount() < 8 && input.gcount() > 0) {
				throw iris::Problem("unaligned object file found!");
			} else if (input.gcount() == 0) {
				if (input.eof()) {
					break;
				} else {
					throw iris::Problem("something bad happened while reading input file!");
				}
			}
			//ignore the first byte, it is always zero
			byte tmp = buf[1];
			Segment target = static_cast<Segment>(buf[1]);
			word address = iris17::encodeWord(buf[2], buf[3]);
			if (debugEnabled()) {
				std::cerr << "current target = " << static_cast<int>(target) << "\tcurrent address = 0x" << std::hex << address << std::endl;
			}
			switch(target) {
				case Segment::Code:
					result = iris17::encodeDword(buf[4], buf[5], buf[6], buf[7]);
					if (debugEnabled()) {
						std::cerr << " code result: 0x" << std::hex << result << std::endl;
					}
					setInstructionMemory(address, result);
					break;
				case Segment::Data:
					result0 = iris17::encodeWord(buf[4], buf[5]);
					if (debugEnabled()) {
						std::cerr << " data result: 0x" << std::hex << result0 << std::endl;
					}
					setDataMemory(address, result0);
					break;
				default:
					std::stringstream str;
					str << "error: line " << lineNumber << ", unknown segment " << static_cast<int>(target) << "/" << static_cast<int>(tmp) << std::endl;
					str << "current address: " << std::hex << address << std::endl;
					throw iris::Problem(str.str());
			}
		}
	}
	word& Core::getDestinationRegister() {
		return registerValue(current.getDestination());
	}
	word& Core::getSource0Register() {
		return registerValue(current.getSource0());
	}
	word& Core::getSource1Register() {
		return registerValue(current.getSource1());
	}
	word Core::getHalfImmediate() {
		return current.getHalfImmediate();
	}
	word Core::getImmediate() {
		return current.getImmediate();
	}
	word& Core::registerValue(byte index) {
		return gpr[index];
	}
}
