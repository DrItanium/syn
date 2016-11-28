#ifndef _TARGET_IRIS20_IRIS_H
#define _TARGET_IRIS20_IRIS_H
#include "iris_base.h"
#include "iris_xunits.h"
#include "Core.h"
#include <cstdint>
#include <memory>
namespace iris20 {
	typedef uint16_t word;
	typedef uint32_t dword;
	typedef dword raw_instruction;
	typedef word immediate;
	enum ArchitectureConstants  {
		RegisterCount = 64,
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
} // end namespace iris20
#include "iris20_defines.h"

namespace iris20 {
	using ExtendedDataMemory = iris::LoadStoreUnit<word, dword>;
	using SharedExtendedDataMemory = std::shared_ptr<ExtendedDataMemory>;
	template<word capacity>
	using WordMemorySpace = iris::FixedSizeLoadStoreUnit<word, word, capacity>;
	using WordMemorySpace64k = WordMemorySpace<ArchitectureConstants::AddressMax>;
	using ALU = iris::ALU<word>;
	using CompareUnit = iris::Comparator<word>;
	using RegisterFile = WordMemorySpace<ArchitectureConstants::RegisterCount>;
	class Core : public iris::Core {
		public:
			Core() noexcept;
			Core(SharedExtendedDataMemory xData) noexcept;
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
			// TODO: add support for installing externally defined system calls
			void setExtendedDataMemory(dword address, word value);
			word getExtendedDataMemory(dword address);
		private:
			word& getStackPointer() noexcept { return gpr[ArchitectureConstants::StackPointerIndex]; }
			word& getInstructionPointer() noexcept { return gpr[ArchitectureConstants::InstructionPointerIndex]; }
			word& getLinkRegister() noexcept { return gpr[ArchitectureConstants::LinkRegisterIndex]; }
		private:
			void dispatch();
            inline byte getDestination() const noexcept { return decodeDestination(current); }
            inline byte getSource0() const noexcept { return decodeSource0(current); }
            inline byte getSource1() const noexcept { return decodeSource1(current); }
			inline word getHalfImmediate() const noexcept { return static_cast<word>(getSource1()); }
            inline word getImmediate() const noexcept { return decodeImmediate(current); }
            inline byte getOperation() const noexcept { return decodeOperation(current); }
            inline byte getGroup() const noexcept { return decodeGroup(current); }
			inline word& destinationRegister() noexcept { return gpr[getDestination()]; }
			inline word& source0Register() noexcept { return gpr[getSource0()]; }
			inline word& source1Register() noexcept { return gpr[getSource1()]; }

		private:
			template<typename Unit>
			void performOperation(Unit& unit, typename Unit::Operation op, bool immediate) {
				destinationRegister() = unit.performOperation(op, source0Register(), (immediate ? getHalfImmediate() : source1Register()));
			}
			template<typename Unit>
			inline void performOperation(Unit& unit, std::tuple<typename Unit::Operation, bool>& tuple) {
				typename Unit::Operation op;
				bool immediate = false;
				std::tie(op, immediate) = tuple;
				performOperation(unit, op, immediate);
			}

		private:
			bool execute = true,
				 advanceIp = true;
			CompareUnit _compare;
			ALU _alu;
			RegisterFile gpr;
			WordMemorySpace64k data;
			iris::FixedSizeLoadStoreUnit<dword, word, ArchitectureConstants::AddressMax> instruction;
			WordMemorySpace64k stack;
			SharedExtendedDataMemory extendedData;
			raw_instruction current = 0;
	};

	Core* newCore() noexcept;
	void assemble(FILE* input, std::ostream* output);
}
#endif
