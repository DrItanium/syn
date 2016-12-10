#ifndef _TARGET_IRIS20_IRIS_H
#define _TARGET_IRIS20_IRIS_H
#include "iris_base.h"
#include "iris_xunits.h"
#include "Core.h"
#include <cstdint>
#include <memory>
namespace iris20 {
	using word = int64_t;
	using InstructionImmediate = int16_t;
	using InstructionAtom = uint32_t;
	enum ArchitectureConstants  {
		RegisterCount = 64,
		AtomsPerMolecule = 2,
		AddressMax = 0x3FFFFFF,
		InstructionPointerIndex = RegisterCount - 1,
		LinkRegisterIndex = RegisterCount - 2,
		StackPointerIndex = RegisterCount - 3,
		MaxOperations = 0b01111111,
		RegisterSection = 0b00000000,
		DataSection = 0b01000000,
		StackSection = 0b10000000,
		UndefinedSection = 0b11000000,
		SectionBitsMask = 0b11000000,
	};
	// iris20 operates on 2 atom molecules, these atoms are both executed
	// before the cycle counter continues. This means that there is an implicit
	// branch delay slot if the programmer so chooses.
	using InstructionMolecule = uint64_t;
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
	using DecodedOperand = std::tuple<SectionType, byte>;
	using DecomposedInstructionMolecule = std::tuple<InstructionAtom, InstructionAtom>;
	inline constexpr DecomposedInstructionMolecule decomposeMolecule(InstructionMolecule molecule) noexcept { return std::make_tuple(decodeFirstAtom(molecule), decodeSecondAtom(molecule)); }
	inline constexpr InstructionAtom getFirstAtom(InstructionMolecule molecule) noexcept { return decodeFirstAtom(molecule); }
	inline constexpr InstructionAtom getSecondAtom(InstructionMolecule molecule) noexcept { return decodeSecondAtom(molecule); }
	inline constexpr DecodedOperand getOperand(byte index) noexcept { return std::make_tuple(decodeSectionDescriptor(index), decodeSectionIndex(index)); }
	inline constexpr DecodedOperand getDestinationOperand(InstructionAtom atom)  noexcept { return getOperand(decodeDestination(atom)); }
	inline constexpr DecodedOperand getSource0Operand(InstructionAtom atom)  noexcept { return getOperand(decodeSource0(atom)); }
	inline constexpr DecodedOperand getSource1Operand(InstructionAtom atom)  noexcept { return getOperand(decodeSource1(atom)); }
	inline constexpr byte getDestinationRawValue(InstructionAtom atom)  noexcept { return (decodeDestination(atom)); }
	inline constexpr byte getSource0RawValue(InstructionAtom atom)  noexcept { return (decodeSource0(atom)); }
	inline constexpr byte getSource1RawValue(InstructionAtom atom)  noexcept { return (decodeSource1(atom)); }

	inline constexpr word getHalfImmediate(InstructionAtom atom) noexcept { return static_cast<word>(decodeSource0(atom)); }
	inline constexpr InstructionImmediate getImmediate(InstructionAtom atom) noexcept { return decodeImmediate(atom); }
	inline constexpr word getImmediate32(InstructionMolecule molecule) noexcept { return decodeImmediate32(molecule); }
	inline constexpr word getImmediate48(InstructionMolecule molecule) noexcept { return decodeImmediate48(molecule); }
	inline constexpr Operation getOperation(InstructionAtom atom) noexcept { return decodeOperation(atom); }

	class Core : public iris::Core {
		public:
			Core() noexcept;
			virtual ~Core();
			virtual void initialize() override;
			virtual void installprogram(std::istream& stream) override;
			virtual void shutdown() override { }
			virtual void dump(std::ostream& stream) override;
			virtual void run() override;
			virtual void link(std::istream& input) override;
		private:
			word operandGet(byte index);
			void operandSet(byte index, word value);
			inline word& getInstructionPointer() noexcept { return gpr[ArchitectureConstants::InstructionPointerIndex]; }
			inline word& getLinkRegister() noexcept { return gpr[ArchitectureConstants::LinkRegisterIndex]; }
			void executeAtom(InstructionAtom atom);
			/**
			 * Execute the given stored molecule as a single instruction!
			 */
			void executeMolecule();
		private:
			void dispatch();
		private:
			template<typename Unit>
			void performOperation(Unit& unit, typename Unit::Operation op, bool immediate, InstructionAtom atom) {
				auto dest = std::get<byte>(getDestinationOperand(atom));
				auto src0 = std::get<byte>(getSource0Operand(atom));
				auto src1 = immediate ? getHalfImmediate(atom) : operandGet(std::get<byte>(getSource1Operand(atom)));
				operandSet(dest, unit.performOperation(op, operandGet(src0), src1));
			}
			template<typename Unit>
			inline void performOperation(Unit& unit, std::tuple<typename Unit::Operation, bool>& tuple, InstructionAtom atom) {
				typename Unit::Operation op;
				bool immediate = false;
				std::tie(op, immediate) = tuple;
				performOperation(unit, op, immediate, atom);
			}

		private:
			bool execute = true,
				 advanceIp = true;
			CompareUnit _compare;
			ALU _alu;
			RegisterFile gpr;
			MemorySpace memory;
			InstructionMolecule current;
	};

	Core* newCore() noexcept;
	void assemble(FILE* input, std::ostream* output);
}
#endif
