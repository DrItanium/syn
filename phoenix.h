#ifndef _TARGET_PHOENIX_IRIS_H
#define _TARGET_PHOENIX_IRIS_H
#include "syn_base.h"
#include "syn_xunits.h"
#include "IOController.h"
#include "Core.h"
#include <cstdint>
#include <memory>
#include <vector>
namespace phoenix {
	using word = int16_t;
	using uword = uint16_t;
	using Register = int64_t;
	using Address = uint64_t;
	enum ArchitectureConstants  {
		RegisterCount = 256,
		AddressMax = 0x7FFFFFF,
		IOAddressBase = 0x7FFFFFFFFF000000, // way up above everything else, we inject our io space
        IOAddressSize = 0x0000000000FFFFFF,
        IOAddressEnd = IOAddressBase + IOAddressSize,
        // builtin addresses
		IOTerminate = 0, 		// If we write to this address then terminate the cpu, it will be an error code
        IOGetC, 		 		// read from thie address and we get a 64-bit value from the keyboard
        IOPutC,                 // write to this address and we print to the screen
		IOGetMemorySize, 		// Will always return the size of actual memory
        IOUserDeviceBegin,
	};
	using InstructionAtom = uint16_t;
	inline constexpr word encodeWord(byte lower, byte upper) noexcept {
		return syn::encodeInt16LE(lower, upper);
	}
	inline constexpr Address encodeAddress(byte a, byte b, byte c, byte d, byte e, byte f, byte g, byte h) noexcept {
		return syn::encodeInt64LE(a, b, c, d, e, f, g, h);
	}
} // end namespace phoenix
#include "phoenix_defines.h"
namespace phoenix {
	template<word capacity>
	using WordMemorySpace = syn::FixedSizeLoadStoreUnit<word, word, capacity>;
	using ALU = syn::ALU<Register>;
	using CompareUnit = syn::Comparator<Register>;
	using RegisterFile = WordMemorySpace<ArchitectureConstants::RegisterCount>;
	using MemorySpace = WordMemorySpace<ArchitectureConstants::AddressMax>;
	using DecodedOperand = std::tuple<SectionType, byte>;
	using DecomposedInstructionMolecule = std::tuple<InstructionAtom, InstructionAtom>;
	using MemoryController = syn::MemoryController<word>;
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
	using IODevice = syn::IODevice<word>;
	using GenericIODevice = syn::LambdaIODevice<word>;
	using IOController = syn::IOController<word>;
	class Core : public syn::Core {
		public:
			using word = molecule::word;
		public:
			Core() noexcept;
			virtual ~Core();
			virtual void initialize() override;
			virtual void installprogram(std::istream& stream) override;
			virtual void shutdown() override { }
			virtual void dump(std::ostream& stream) override;
			virtual void run() override;
			virtual void link(std::istream& input) override;
			virtual bool cycle() override;
			word& getRegister(byte index) noexcept;
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
			InstructionMolecule current;
			MemoryController _controller;
			Register _ip;
			// TODO: come up with storage to dump images to within the confines
			// of the system (no dump command!).
        public:
            /**
             * Install a given device at the given address as an offset of the IOBaseAddress given in the architecture constants
             */
			using IOReadFunction = GenericIODevice::ReadFunction;
			using IOWriteFunction = GenericIODevice::WriteFunction;
			using IOInitFunction = GenericIODevice::InitializeFunction;
			using IOShutdownFunction = GenericIODevice::ShutdownFunction;
            void installDevice(IOController::SharedIONodePtr device);
			void installIODevice(word start, word length, IOReadFunction read, IOWriteFunction write, IOInitFunction init = syn::initNothing<typename GenericIODevice::DataType, typename GenericIODevice::AddressType>, IOShutdownFunction shutdown = syn::shutdownNothing<typename GenericIODevice::DataType, typename GenericIODevice::AddressType>);
	};

	Core* newCore() noexcept;
}
#endif
