#ifndef _TARGET_IRIS20_IRIS_H
#define _TARGET_IRIS20_IRIS_H
#include "iris_base.h"
#include "iris_xunits.h"
#include "Core.h"
#include <cstdint>
#include <memory>
namespace iris20 {
	typedef int64_t word;
	typedef word raw_instruction;
	typedef word immediate;
	enum ArchitectureConstants  {
		RegisterCount = 64,
		AddressMax = 16777216 * 4, // per space
		InstructionPointerIndex = RegisterCount - 1,
		LinkRegisterIndex = RegisterCount - 2,
		StackPointerIndex = RegisterCount - 3,
		MaxGroups = 0b00000111,
		MaxOperations = 0b00011111,
		UndefinedSection = 0b00000000,
		DataSection = 0b01000000,
		StackSection = 0b10000000,
		RegisterSection = 0b11000000,
		SectionBitsMask = 0b11000000,
	};
	inline constexpr word encodeWord(byte a, byte b, byte c, byte d, byte e, byte f, byte g, byte h) noexcept {
		return iris::encodeInt64LE(a, b, c, d, e, f, g, h);
	}
} // end namespace iris20
#include "iris20_defines.h"

namespace iris20 {
	template<word capacity>
	using WordMemorySpace = iris::FixedSizeLoadStoreUnit<word, word, capacity>;
	using ALU = iris::ALU<word>;
	using CompareUnit = iris::Comparator<word>;
	using RegisterFile = WordMemorySpace<ArchitectureConstants::RegisterCount>;
	using MemorySpace = WordMemorySpace<ArchitectureConstants::AddressMax>;
	class Core : public iris::Core {
		public:
			Core() noexcept;
			virtual ~Core();
			virtual void initialize() override { }
			virtual void installprogram(std::istream& stream) override;
			virtual void shutdown() override { }
			virtual void dump(std::ostream& stream) override;
			virtual void run() override;
			virtual void link(std::istream& input) override;
		private:
			word operandGet(byte index);
			void operandSet(byte index, word value);
		private:
			void dispatch();
			inline bool isStackSection(byte index) const noexcept { return (index & ArchitectureConstants::SectionBitsMask) == ArchitectureConstants::StackSection; }
			inline bool isDataSection(byte index) const noexcept { return (index & ArchitectureConstants::SectionBitsMask) == ArchitectureConstants::DataSection; }
			inline bool isRegisterSection (byte index) const noexcept { return (index & ArchitectureConstants::SectionBitsMask) == ArchitectureConstants::RegisterSection; }
			inline bool isUndefinedSection (byte index) const noexcept { return (index & ArchitectureConstants::SectionBitsMask) == ArchitectureConstants::CodeSection; }
            inline byte getDestination() const noexcept { return decodeDestination(current); }
            inline byte getSource0() const noexcept { return decodeSource0(current); }
            inline byte getSource1() const noexcept { return decodeSource1(current); }
			inline word getHalfImmediate() const noexcept { return static_cast<word>(getSource1()); }
            inline word getImmediate() const noexcept { return decodeImmediate(current); }
            inline byte getOperation() const noexcept { return decodeOperation(current); }
            inline byte getGroup() const noexcept { return decodeGroup(current); }

		private:
			template<typename Unit>
			void performOperation(Unit& unit, typename Unit::Operation op, bool immediate) {
				genericSet(getDestination(), unit.performOperation(op, genericGet(getSource0()), genericGet(getSource1())));
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
			MemorySpace memory; 
			raw_instruction current = 0;
	};

	Core* newCore() noexcept;
	void assemble(FILE* input, std::ostream* output);
}
#endif
