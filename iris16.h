#ifndef _TARGET_IRIS16_IRIS_H
#define _TARGET_IRIS16_IRIS_H
#include "iris_base.h"
#include "Core.h"
#include <cstdint>
#include <memory>
namespace iris16 {
	typedef uint16_t word;
	typedef uint32_t dword;
	typedef dword raw_instruction;
	typedef word immediate;
	enum ArchitectureConstants  {
		RegisterCount = 256,
		AddressMax = 65535,
		InstructionPointerIndex = RegisterCount - 1,
		LinkRegisterIndex = RegisterCount - 2,
		StackPointerIndex = RegisterCount - 3,
		MaxGroups = 0b00000111,
		MaxOperations = 0b00011111,
	};
	inline constexpr dword encodeDword(byte a, byte b, byte c, byte d) noexcept {
		return iris::encodeUint32LE(a, b, c, d);
	}
	inline constexpr word encodeWord(byte a, byte b) noexcept {
		return iris::encodeUint16LE(a, b);
	}
	inline constexpr dword encodeDword(word lower, word upper) noexcept {
		return iris::encodeUint32LE(lower, upper);
	}
#include "def/iris16/enums.def"

	class Core : public iris::Core {
		public:
			Core() noexcept;
			Core(std::shared_ptr<word> data, dword size) noexcept;
			virtual ~Core();
			virtual void initialize() override { }
			virtual void installprogram(std::istream& stream) override;
			virtual void shutdown() override { }
			virtual void dump(std::ostream& stream) override;
			virtual void run() override;
			virtual void link(std::istream& input) override;
			inline void setInstructionMemory(word address, dword value) noexcept { instruction[address] = value; }
			inline void setDataMemory(word address, word value) noexcept         { data[address] = value; }
			inline dword getInstructionMemory(word address) noexcept             { return instruction[address]; }
			inline word getDataMemory(word address) noexcept                     { return data[address]; }
			inline void setExtendedDataMemory(dword address, word value) { 
				if (extendedMemorySize != 0) {
					if (address < extendedMemorySize) {
						extendedData.get()[address] = value;
					} else {
						throw iris::Problem("Attempted to write to an address outside the mapped extended memory range");
					}
				} else {
					throw iris::Problem("Attempted to write to non existent extended memory!");
				}
			}

			inline word getExtendedDataMemory(dword address) {
				if (extendedMemorySize != 0) {
					if (address < extendedMemorySize) {
						return extendedData.get()[address];
					} else {
						throw iris::Problem("Attempted to read from an address outside the mapped extended memory range");
					}
				} else {
					throw iris::Problem("Attempted to read from non existent extended memory!");
				}
			}

		private:
			void dispatch();
			template<bool ifthenelse, bool conditional, bool iffalse, bool immediate, bool link>
			inline void jumpBody() noexcept {
				auto newAddr = static_cast<word>(0);
				auto cond = true;
				advanceIp = false;
				auto ip = gpr[ArchitectureConstants::InstructionPointerIndex];
				if (conditional) {
					auto dest = gpr[getDestination()];
					cond = (iffalse ? (dest == 0) : (dest != 0));
					if (ifthenelse) {
						newAddr = gpr[cond ? getSource0() : getSource1()];
					} else {
						newAddr = cond ? (immediate ? getImmediate() : gpr[getSource0()]) : ip + 1;
					}
				} else {
					newAddr = immediate ? getImmediate() : gpr[getDestination()];
				}
				gpr[ArchitectureConstants::InstructionPointerIndex] = newAddr;
				if (link && cond) {
					gpr[ArchitectureConstants::LinkRegisterIndex] = ip + 1;
				}
			}
			template<MoveOp op>
			inline void moveBody() {
				throw iris::Problem("Undefined move operation");
			}
#include "def/iris16/core_body.def"
		private:
			std::shared_ptr<word> extendedData;
			dword extendedMemorySize = 0;
			bool execute = true,
				 advanceIp = true;
			word gpr[ArchitectureConstants::RegisterCount] = {0};
			word data[ArchitectureConstants::AddressMax] = { 0 } ;
			dword instruction[ArchitectureConstants::AddressMax] = { 0 };
			word stack[ArchitectureConstants::AddressMax] = { 0 };
			raw_instruction current = 0;
	};
	template<>
	inline void Core::moveBody<MoveOp::Move>() {
		gpr[getDestination()] = gpr[getSource0()];
	}

	template<>
	inline void Core::moveBody<MoveOp::Set>() {
		gpr[getDestination()] = static_cast<word>(getImmediate());
	}

	template<>
	inline void Core::moveBody<MoveOp::Swap>() {
		auto result = instruction[gpr[getDestination()]];
		instruction[gpr[getDestination()]] = instruction[gpr[getSource0()]];
		instruction[gpr[getSource0()]] = result;
	}

	template<>
	inline void Core::moveBody<MoveOp::Load>() {
		gpr[getDestination()] = getDataMemory(gpr[getSource0()]);
	}

	template<>
	inline void Core::moveBody<MoveOp::LoadImmediate>() {
		gpr[getDestination()] = getDataMemory(getImmediate());
	}

	template<>
	inline void Core::moveBody<MoveOp::Store>() {
		setDataMemory(gpr[getDestination()], gpr[getSource0()]);
	}

	template<>
	inline void Core::moveBody<MoveOp::Memset>() {
		setDataMemory(gpr[getDestination()], getImmediate());
	}

	template<>
	inline void Core::moveBody<MoveOp::Push>() {
		++gpr[ArchitectureConstants::StackPointerIndex];
		stack[gpr[ArchitectureConstants::StackPointerIndex]] = gpr[getDestination()];
	}

	template<>
	inline void Core::moveBody<MoveOp::PushImmediate>() {
		++gpr[ArchitectureConstants::StackPointerIndex];
		stack[gpr[ArchitectureConstants::StackPointerIndex]] = getImmediate();
	}

	template<>
	inline void Core::moveBody<MoveOp::Pop>() {
		gpr[getDestination()] = stack[gpr[ArchitectureConstants::StackPointerIndex]];
		--gpr[ArchitectureConstants::StackPointerIndex];
	}

	template<>
	inline void Core::moveBody<MoveOp::LoadCode>() {
		auto result = instruction[gpr[getDestination()]];
		gpr[getSource0()] = iris::decodeBits<dword, word, 0x0000FFFF, 0>(result);
		gpr[getSource1()] = iris::decodeBits<dword, word, 0xFFFF0000, 16>(result);
	}

	template<>
	inline void Core::moveBody<MoveOp::StoreCode>() {
		instruction[getDestination()] = encodeDword(gpr[getSource0()], gpr[getSource1()]);
	}

	template<>
	inline void Core::moveBody<MoveOp::ExtendedMemoryWrite>() {
		// store destination in the address described by source0 and source1
		auto result = gpr[getDestination()];
		auto lower = gpr[getSource0()];
		auto upper = gpr[getSource1()];
		// build an address out of this
		setExtendedDataMemory(iris::encodeUint32LE(lower, upper), result);
	}

	template<>
	inline void Core::moveBody<MoveOp::ExtendedMemoryRead>() {
		auto lower = gpr[getSource0()];
		auto upper = gpr[getSource1()];
		// build an address out of this
		gpr[getDestination()] = getExtendedDataMemory(iris::encodeUint32LE(lower, upper));
	}

	Core* newCore() noexcept;
}
#endif
