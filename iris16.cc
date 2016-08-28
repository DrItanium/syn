#include "iris16.h"
#include "sim_registration.h"
#include <functional>
#include <sstream>


namespace iris16 {
	Core* newCore() noexcept {
		return new iris16::Core();
	}


	Core::~Core() { }

	void Core::setInstructionMemory(word address, dword value) noexcept {
		instruction[address] = value;
	}

	void Core::setDataMemory(word address, word value) noexcept {
		data[address] = value;
	}

	template<typename T, int count>
	void populateContents(T* contents, std::istream& stream, const std::function<T(char*)>& func) {
		char buf[sizeof(T)] = { 0 };
		for(int i = 0; i < count; ++i) {
			stream.read(buf, sizeof(T));
			contents[i] = func(buf);
		}
	}
	void Core::installprogram(std::istream& stream) {
		auto encodeWord = [](char buf[2]) {
			return iris16::encodeWord(buf[0], buf[1]);
		};
		populateContents<word, ArchitectureConstants::RegisterCount>(gpr, stream, encodeWord);
		populateContents<word, ArchitectureConstants::AddressMax>(data, stream, encodeWord);
		populateContents<dword, ArchitectureConstants::AddressMax>(instruction, stream, [](char buf[4]) { return iris16::encodeDword(buf[0], buf[1], buf[2], buf[3]); });
		populateContents<word, ArchitectureConstants::AddressMax>(stack, stream, encodeWord);
	}

	template<typename T, int count>
	void dumpContents(T* contents, std::ostream& stream, const std::function<char*(T,char*)>& func) {
		char buf[sizeof(T)] = { 0 };
		for(int i = 0; i < count; ++i) {
			stream.write(func(contents[i], buf), sizeof(T));
		}
	}
	void Core::dump(std::ostream& stream) {
		// save the registers
		auto decomposeWord = [](word v, char* buf) {
			iris::decodeUint16LE(v, (byte*)buf);
			return buf;
		};
		auto decomposeDword = [](dword v, char* buf) {
			iris::decodeUint32LE(v, (byte*)buf);
			return buf;
		};
		dumpContents<word, ArchitectureConstants::RegisterCount>(gpr, stream, decomposeWord);
		dumpContents<word, ArchitectureConstants::AddressMax>(data, stream, decomposeWord);
		dumpContents<dword, ArchitectureConstants::AddressMax>(instruction, stream, decomposeDword);
		dumpContents<word, ArchitectureConstants::AddressMax>(stack, stream, decomposeWord);
	}
	void Core::run() {
		while(execute) {
			if (!advanceIp) {
				advanceIp = true;
			}
			current = instruction[gpr[ArchitectureConstants::InstructionPointerIndex]];
			dispatch();
			if (advanceIp) {
				++gpr[ArchitectureConstants::InstructionPointerIndex];
			}
		}
	}
	void Core::dispatch() noexcept {
		auto group = static_cast<InstructionGroup>(getGroup());
#define X(name, operation) \
		if (group == InstructionGroup:: name) { \
			operation(); \
			return; \
		}
#include "def/iris16/groups.def"
#undef X
		std::cerr << "Illegal instruction group " << getGroup() << std::endl;
		execute = false;
	}
	void Core::compare() noexcept {
		switch(static_cast<CompareOp>(getOperation())) {
#define OpNone =
//#define OpAnd &=
//#define OpOr |=
//#define OpXor ^=
#define X(type, compare, mod) \
			case CompareOp:: type: \
								   gpr[getDestination()] INDIRECTOR(Op, mod) (gpr[getSource0()] compare gpr[getSource1()]); \
			break;
#define Y(type, compare, mod) \
			case CompareOp:: type: \
								   gpr[getDestination()] INDIRECTOR(Op, mod) (gpr[getSource0()] compare (word(getSource1()))); \
			break;

#include "def/iris16/compare.def"
#undef X
#undef Y
#undef OpNone
//#undef OpAnd
//#undef OpOr
//#undef OrXor
			default:
				std::cerr << "Illegal compare code " << getOperation() << std::endl;
				execute = false;
				advanceIp = false;
				break;
		}
	}

	void Core::arithmetic() noexcept {
		switch(static_cast<ArithmeticOp>(getOperation())) {
#define XNone(n, op) gpr[getDestination()] = ( gpr[getSource0()] op gpr[getSource1()]);
#define XImmediate(n, op) gpr[getDestination()] = (gpr[getSource0()] op static_cast<word>(getSource1()));
#define XUnary(n, op) gpr[getDestination()] = (op gpr[getSource0()]);
#define XDenominator(n, op) \
			if (gpr[getSource1()] == 0) { \
				std::cerr << "denominator in for operation " << #n << " is zero!" << std::endl; \
				execute = false; \
			} else { \
				XNone(n, op) \
			}
#define XDenominatorImmediate(n, op) \
			if (gpr[getSource1()] == 0) { \
				std::cerr << "denominator in for operation " << #n << " is zero!" << std::endl; \
				execute = false; \
			} else { \
				XImmediate(n, op) \
			}
#define X(name, op, desc) \
			case ArithmeticOp:: name: \
						{ \
							INDIRECTOR(X, desc)(name, op) \
							break; \
						}
#include "def/iris16/arithmetic.def"
#undef X
#undef XNone
#undef XDenominator
#undef XUnary
#undef XImmediate
#undef XDenominatorImmediate

			default:
				std::cerr << "Illegal arithmetic operation " << getOperation() << std::endl;
				execute = false;
				break;
		}
	}
	template<JumpOp op>
	struct ConditionalStyle {
		static constexpr bool isFalseForm = false;
	};
#define X(name, ifthenelse, conditional, iffalse, immediate, link) \
	template<> struct ConditionalStyle<JumpOp:: name> { static constexpr bool isFalseForm = iffalse; };
#include "def/iris16/jump.def"
#undef X

	void Core::jump() noexcept {
		word newAddr = 0;
		bool cond = true;
		advanceIp = false;
		word ip = gpr[ArchitectureConstants::InstructionPointerIndex];
		switch(static_cast<JumpOp>(getOperation())) {
#define XImmediateCond_true (getImmediate())
#define XImmediateCond_false (gpr[getSource0()])
#define XIfThenElse_false(immediate) \
			newAddr = cond ? INDIRECTOR(XImmediateCond, _ ## immediate) : ip + 1;
#define XIfThenElse_true(immediate) \
			newAddr = gpr[cond ? getSource0() : getSource1()];
#define XImmediateUncond_false (gpr[getDestination()])
#define XImmediateUncond_true (getImmediate())
#define XConditional_false(name, ifthenelse, immediate) \
			newAddr = INDIRECTOR(XImmediateUncond, _ ## immediate);
#define XConditional_true(name, ifthenelse, immediate) \
			cond = (ConditionalStyle<JumpOp:: name>::isFalseForm ? (gpr[getDestination()] == 0) : (gpr[getDestination()] != 0)); \
			INDIRECTOR(XIfThenElse, _ ## ifthenelse)(immediate)
#define XLink_true \
			if (cond) { \
				gpr[ArchitectureConstants::LinkRegisterIndex] = ip + 1; \
			}
#define XLink_false

#define X(name, ifthenelse, conditional, iffalse, immediate, link) \
			case JumpOp:: name: \
					 { \
						 INDIRECTOR(XConditional, _ ## conditional)(name, ifthenelse, immediate) \
						 gpr[ArchitectureConstants::InstructionPointerIndex] = newAddr; \
						 INDIRECTOR(XLink, _ ## link)  \
						 break; \
					 }
#include "def/iris16/jump.def"
#undef X
			default:
				std::cerr << "Illegal jump code " << getOperation() << std::endl;
				execute = false;
				break;
		}
	}
	void Core::misc() noexcept {
		auto op = static_cast<MiscOp>(getOperation());
#define X(name, func) \
		if (op == MiscOp:: name) { \
			func () ; \
			return; \
		}
#include "def/iris16/misc.def"
#undef X
		std::cerr << "Illegal misc code " << getOperation() << std::endl;
		execute = false;
		advanceIp = false;
	}
	void Core::systemCall() noexcept {
		switch(static_cast<SystemCalls>(getDestination())) {
			case SystemCalls::Terminate:
				execute = false;
				advanceIp = false;
				break;
			case SystemCalls::PutC:
				// read register 0 and register 1
				std::cout.put(static_cast<char>(gpr[getSource0()]));
				break;
			case SystemCalls::GetC:
				byte value;
				std::cin >> std::noskipws >> value;
				gpr[getSource0()] = static_cast<word>(value);
				break;
			default:
				std::cerr << "Illegal system call " << getDestination() << std::endl;
				execute = false;
				advanceIp = false;
				break;
		}
	}
	void Core::move() noexcept {
		word a = 0;
		switch(static_cast<MoveOp>(getOperation())) {
#define GPRRegister0 (gpr[getDestination()])
#define GPRRegister1 (gpr[getSource0()])
#define GPRRegister2 (gpr[getSource1()])
#define GPRImmediate1 (getImmediate())
#define DataRegister0 GPRRegister0
#define DataRegister1 GPRRegister1
#define DataImmediate1 GPRImmediate1
#define StackPushRegister0 (gpr[ArchitectureConstants::StackPointerIndex])
#define StackPushRegister1 GPRRegister0
#define StackPushImmediate1 GPRImmediate1
#define StackPopRegister0 GPRRegister0
#define StackPopRegister1 (gpr[ArchitectureConstants::StackPointerIndex])
#define StackPopImmediate1 GPRImmediate1
#define StoreRegister0  GPRRegister0
#define StoreRegister1 GPRRegister1
#define StoreImmediate1 GPRImmediate1
#define CodeRegister0 GPRRegister0
#define CodeUpperLowerRegisters1 GPRRegister1
#define CodeUpperLowerRegisters2 GPRRegister2

#define XLoadCode(type, dest, src) \
			auto result = instruction[INDIRECTOR(type, dest ## 0)]; \
			INDIRECTOR(type, src ## 1) = static_cast<word>(result); \
			INDIRECTOR(type, src ## 2) = static_cast<word>(result >> 16);

#define XStoreCode(type, dest, src) \
			instruction[INDIRECTOR(type, dest ## 0)] = (((static_cast<dword>(INDIRECTOR(type, src ## 2))) << 16) | (static_cast<dword>(INDIRECTOR(type, src ## 1))));

#define XMove(type, dest, src) \
			INDIRECTOR(type, dest ## 0) = INDIRECTOR(type, src ## 1);
#define XSwap(type, dest, src) \
			a = INDIRECTOR(type, dest ##  0); \
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
			case MoveOp:: name: \
								{ \
									INDIRECTOR(X,type)(target, dest, src) \
									break; \
								}
#include "def/iris16/move.def"
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
				std::cerr << "Illegal move code " << getOperation() << std::endl;
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
			word address = iris16::encodeWord(buf[2], buf[3]);
			if (debugEnabled()) {
				std::cerr << "current target = " << static_cast<int>(target) << "\tcurrent address = 0x" << std::hex << address << std::endl;
			}
			switch(target) {
				case Segment::Code:
					result = iris16::encodeDword(buf[4], buf[5], buf[6], buf[7]);
					if (debugEnabled()) {
						std::cerr << " code result: 0x" << std::hex << result << std::endl;
					}
					setInstructionMemory(address, result);
					break;
				case Segment::Data:
					result0 = iris16::encodeWord(buf[4], buf[5]);
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
}
