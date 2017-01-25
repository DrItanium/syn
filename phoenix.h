#ifndef _TARGET_PHOENIX_IRIS_H
#define _TARGET_PHOENIX_IRIS_H
#include "syn_base.h"
#include "ExecutionUnits.h"
#include "IOController.h"
#include "Core.h"
#include <cstdint>
#include <memory>
#include <vector>
namespace phoenix {
	using Bit = bool;
	using BitPair = byte;
	using Nybble = byte;
	using Byte = byte;
	using Qword = uint16_t;
	using Hword = uint32_t;
	using Word = uint64_t;
	using InstructionFragment = uint16_t;
	using RegisterIndex = uint16_t;
	using Register = int64_t;
	using Address = uint64_t;
	using MemoryWord = uint16_t;

	Bit getBit(Register value, byte index) noexcept;
	Register setBit(Register value, byte index, Bit b) noexcept;
	Nybble getNybble(Register value, byte index) noexcept;
	Register setNybble(Register value, byte index, Nybble b) noexcept;
	Byte getByte(Register value, byte index) noexcept;
	Register setByte(Register value, byte index, Byte b) noexcept;
	Qword getQword(Register value, byte index) noexcept;
	Register setQword(Register value, byte index, Qword b) noexcept;
	Hword getHword(Register value, byte index) noexcept;
	Register setHword(Register value, byte index, Hword b) noexcept;

	enum ArchitectureConstants  {
		RegisterCount = 64,
		AddressMax = 0x7FFFFFF,
		AddressSize = AddressMax + 1,
		MaxOperations = 0x0040,
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
	inline constexpr Address encodeAddress(byte a, byte b, byte c, byte d, byte e, byte f, byte g, byte h) noexcept {
		return syn::encodeUint64LE(a, b, c, d, e, f, g, h);
	}
} // end namespace phoenix
#include "phoenix_defines.h"
namespace phoenix {
	class Core : public syn::Core {
		public:
			template<Address size>
			using MemoryBlock = syn::FixedSizeLoadStoreUnit<Register, Address, size>;
			using RegisterFile = MemoryBlock<ArchitectureConstants::RegisterCount>;
			using RAM = syn::FixedSizeLoadStoreUnit<MemoryWord, Address, ArchitectureConstants::AddressSize>;
			using MemoryController = syn::MemoryController<MemoryWord, Address>;
		public:
			Core() noexcept;
			virtual ~Core();
			virtual void initialize() override;
			virtual void installprogram(std::istream& stream) override;
			virtual void shutdown() override { }
			virtual void dump(std::ostream& stream) override;
			virtual void link(std::istream& input) override;
			virtual bool cycle() override;
			Register& getRegister(byte index) noexcept;
		private:
			void dispatch();
		private:
			Bit getBitRegister(RegisterIndex index);
			void setBitRegister(RegisterIndex index, Bit value);
			Nybble getNybbleRegister(RegisterIndex index);
			void setNybbleRegister(RegisterIndex index, Nybble value);
			BitPair getBitPairRegister(RegisterIndex index);
			void setBitPairRegister(RegisterIndex index, BitPair value);
			Byte getByteRegister(RegisterIndex index);
			void setByteRegister(RegisterIndex index, Byte value);
			Hword getHwordRegister(RegisterIndex index);
			void setHwordRegister(RegisterIndex index, Hword value);
			Qword getQwordRegister(RegisterIndex index);
			void setQwordRegister(RegisterIndex index, Qword value);
			Word getWordRegister(byte index);
			void setWordRegister(byte index, Word value);

		private:
			bool advanceIp = true;
			//CompareUnit _compare;
			//ALU _alu;
			RegisterFile _gpr;
			MemoryController _controller;
			RAM _ram;
			Register _ip;
			Register _linkRegister;
			Register _countRegister;
			// TODO: come up with storage to dump images to within the confines
			// of the system (no dump command!).
	};

	Core* newCore() noexcept;
}
#endif
