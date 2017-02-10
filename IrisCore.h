#ifndef _TARGET_IRIS16_IRIS_H
#define _TARGET_IRIS16_IRIS_H
#include "Base.h"
#include "ExecutionUnits.h"
#include "Core.h"
#include "IOController.h"
#include <cstdint>
#include <memory>
namespace iris {
    using word = uint16_t;
    using dword = uint32_t;
    using raw_instruction = dword;
    using immediate = word;
    using QuadWord = uint64_t;
	enum ArchitectureConstants  {
		RegisterCount = 256,
		AddressMax = 0xFFFF,
        AddressCount = AddressMax + 1,
		RegisterMax = 0xFF,
		ConditionRegisterCount = 16,
		StackPointerIndex = RegisterCount - 1,
		MaxGroups = 8,
		MaxOperations = 32,
		ErrorDispatchVectorBase = 0x00FF,
		RegistersToSaveOnError = 18,
		ErrorRegisterStart = 255,
	};
	inline constexpr dword encodeDword(byte a, byte b, byte c, byte d) noexcept {
		return syn::encodeUint32LE(a, b, c, d);
	}
	inline constexpr word encodeWord(byte a, byte b) noexcept {
		return syn::encodeUint16LE(a, b);
	}
	inline constexpr dword encodeDword(word lower, word upper) noexcept {
		return syn::encodeUint32LE(lower, upper);
	}
} // end namespace iris
#include "iris_defines.h"

template<typename Data, typename Address>
class ExposedCoreDataMemory;
namespace iris {
	using IOSpace = syn::IOController<word>;
	template<dword capacity>
	using WordMemorySpace = syn::FixedSizeLoadStoreUnit<word, dword, capacity>;
	using WordMemorySpace64k = WordMemorySpace<ArchitectureConstants::AddressMax + 1>;
	using ALU = syn::ALU<word>;
	using CompareUnit = syn::Comparator<word>;
	using RegisterFile = WordMemorySpace<ArchitectureConstants::RegisterCount>;
	using IODevice = syn::IODevice<word>;
	using LambdaIODevice = syn::LambdaIODevice<word>;
	using PredicateRegisterFile = syn::FixedSizeLoadStoreUnit<bool, byte, 16>;
	using PredicateComparator = syn::Comparator<bool, bool>;
	using ErrorStorage = WordMemorySpace<ArchitectureConstants::RegistersToSaveOnError>;
    using InstructionPointer = syn::Register<QuadWord, ArchitectureConstants::AddressMax>;
    using LinkRegister = syn::Register<QuadWord, ArchitectureConstants::AddressMax>;
	class Core : public syn::Core {
		public:
			Core() noexcept;
			virtual ~Core();
			virtual void initialize() override;
			virtual void installprogram(std::istream& stream) override;
			virtual void shutdown() override;
			virtual void dump(std::ostream& stream) override;
			virtual void link(std::istream& input) override;
			inline void writeInstructionMemory(word address, dword value) noexcept { instruction[address] = value; }
			inline void writeDataMemory(word address, word value) noexcept         { data[address] = value; }
			inline dword readInstructionMemory(word address) noexcept             { return instruction[address]; }
			inline word readDataMemory(word address) noexcept                     { return data[address]; }
			void writeIOMemory(word address, word value);
			word readIOMemory(word address);
			void installIODevice(std::shared_ptr<IODevice> dev);
			void writeRegister(byte index, word value);
			word readRegister(byte index);
			virtual bool cycle() override;
		private:
            QuadWord getInstructionPointer() const noexcept { return _ip.get(); }
            void setInstructionPointer(QuadWord value) noexcept { _ip.set(value); }
            QuadWord getLinkRegister() const noexcept { return _lr.get(); }
            void setLinkRegister(QuadWord value) noexcept { _lr.set(value); }
			bool& getPredicateRegister(byte index);
            void incrementInstructionPointer() noexcept { setInstructionPointer(getInstructionPointer() + 1); }

		private:
			void dispatch() noexcept;
            inline byte getDestination() const noexcept { return decodeDestination(current); }
            inline byte getSource0() const noexcept { return decodeSource0(current); }
            inline byte getSource1() const noexcept { return decodeSource1(current); }
			inline word getHalfImmediate() const noexcept { return decodeHalfImmediate(current); }
            inline word getImmediate() const noexcept { return decodeImmediate(current); }
            inline byte getOperationByte() const noexcept { return decodeOperation(current); }
			template<typename T>
			inline T getOperation() const noexcept {
				return static_cast<T>(getOperationByte());
			}
            inline byte getGroup() const noexcept { return decodeGroup(current); }
			inline byte getPredicateResult() const noexcept { return decodePredicateResult(current); }
			inline byte getPredicateInverse() const noexcept { return decodePredicateInverseResult(current); }
			inline byte getPredicateSource0() const noexcept { return decodePredicateSource0(current); }
			inline byte getPredicateSource1() const noexcept { return decodePredicateSource1(current); }
			inline word& destinationRegister() noexcept { return gpr[getDestination()]; }
			inline word& source0Register() noexcept { return gpr[getSource0()]; }
			inline word& source1Register() noexcept { return gpr[getSource1()]; }
			inline bool& predicateResult() noexcept { return getPredicateRegister(getPredicateResult()); }
			inline bool& predicateInverseResult() noexcept { return getPredicateRegister(getPredicateInverse()); }
			inline bool& predicateSource0() noexcept { return getPredicateRegister(getPredicateSource0()); }
			inline bool& predicateSource1() noexcept { return getPredicateRegister(getPredicateSource1()); }
		private:
			void saveSystemState() noexcept;
			void restoreSystemState() noexcept;
			void dispatchInterruptHandler() noexcept;

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
			template<word index>
			struct PredicateRegisterEncoder {
				static_assert(index < 16, "Provided predicate register is out of range!");
				static PredicateRegisterEncoder<index - 1> next;
				static word invoke(Core* c, word mask) {
					auto result = next.invoke(c, mask);
					if (syn::getBit<word,index>(mask)) {
						return syn::setBit<word, index>(result, c->getPredicateRegister(index));
					} else {
						return result;
					}
				}
			};
			template<word index>
			struct PredicateRegisterDecoder {
				static_assert(index < 16, "Provided predicate register index is too large!");
				static PredicateRegisterDecoder<index - 1> next;
				static void invoke(Core* c, word input, word mask) noexcept {
					if (syn::getBit<word, index>(mask)) {
						c->getPredicateRegister(index) = syn::getBit<word, index>(input);
					}
					next.invoke(c, input, mask);
				}
			};
			void restorePredicateRegisters(word input, word mask) noexcept;
			word savePredicateRegisters(word mask) noexcept;

		private:
			bool execute;
			bool advanceIp;
			raw_instruction current;
            InstructionPointer _ip;
            LinkRegister _lr;
			word _error;
			IOSpace _io;
			CompareUnit _compare;
			ALU _alu;
			RegisterFile gpr;
			WordMemorySpace64k data;
			syn::FixedSizeLoadStoreUnit<dword, word, ArchitectureConstants::AddressMax> instruction;
			WordMemorySpace64k stack;
			PredicateRegisterFile _cr;
			PredicateComparator _pcompare;
			ErrorStorage _onError;
			bool _saveAdvanceIp = false;
			bool _saveExecute = false;
            bool _inInterruptHandler = false;
	};
	template<>
		struct Core::PredicateRegisterEncoder<0> {
			static word invoke(Core* c, word mask) {
				if (syn::getBit<word, 0>(mask)) {
					return static_cast<word>(c->getPredicateRegister(0) ? 1 : 0);
				} else {
					return 0;
				}
			}
		};
	template<>
		struct Core::PredicateRegisterDecoder<0> {
			static void invoke(Core* c, word input, word mask) noexcept {
				if (syn::getBit<word, 0>(mask)) {
					c->getPredicateRegister(0) = syn::getBit<word, 0>(input);
				}
			}
		};
	template<typename Data, typename Address>
	class ExposedCoreDataMemory : public syn::IODevice<Data, Address> {
		public:
			static constexpr Address computeScaleFactor() noexcept {
				return sizeof(Data) / sizeof(word);
			}
			static constexpr Address computeDataLength() noexcept {
				return (ArchitectureConstants::AddressMax + 1) / computeScaleFactor();
			}
			static constexpr Address computeDataMemoryEnd() noexcept {
				return computeDataLength() - 1;
			}
			static constexpr word computeInternalAddress(Address addr) noexcept {
				return static_cast<word>(addr * computeScaleFactor());
			}
			ExposedCoreDataMemory(Core* core, Address base, Address length = computeDataLength()) : syn::IODevice<Data, Address>(base, length), _core(core) { }
			virtual ~ExposedCoreDataMemory() { }
			virtual void write(Address address, Data value) override {
				auto addr = computeInternalAddress(tryComputeActualAddress(address));
				static_assert(computeScaleFactor() != 0, "The size of the provided data element is smaller than the iris word! Please provide a custom implementation of write!");
				static_assert(computeScaleFactor() <= 4, "The size of the provided data value is too large for the default write implementation! Please provide a custom implementation of write!");
				switch(computeScaleFactor()) {
					case 4:
						_core->writeDataMemory(addr + 3, syn::decodeBits<Data, word, static_cast<Data>(0xFFFF000000000000), 48>(value));
					case 3:
						_core->writeDataMemory(addr + 2, syn::decodeBits<Data, word, static_cast<Data>(0x0000FFFF00000000), 32>(value));
					case 2:
						_core->writeDataMemory(addr + 1, syn::decodeBits<Data, word, static_cast<Data>(0x00000000FFFF0000), 16>(value));
					case 1:
						_core->writeDataMemory(addr, syn::decodeBits<Data, word, static_cast<Data>(0x000000000000FFFF), 0>(value));
						break;
					case 0:
						throw syn::Problem("Can't have a scale factor of zero!");
					default:
						throw syn::Problem("Illegal scale factor, please make a custom implementation!");
				}
				_core->writeDataMemory(static_cast<word>(tryComputeActualAddress(address)), value);
			}
			virtual Data read(Address address) override {
				// get the address factor computed
				auto addr = computeInternalAddress(tryComputeActualAddress(address));
				static_assert(computeScaleFactor() != 0, "The size of the provided data element is smaller than the iris word! Please provide a custom implementation of read!");
				static_assert(computeScaleFactor() <= 4, "The size of the provided data value is too large for the default read implementation! Please provide a custom implementation of read!");
				if (computeScaleFactor() == 1) {
					return static_cast<Data>(_core->readDataMemory(addr));
				} else if (computeScaleFactor() == 2) {
					return syn::encodeValueLE<Data>(_core->readDataMemory(addr), _core->readDataMemory(addr + 1));
				} else if (computeScaleFactor() == 3) {
					return syn::encodeValueLE<Data>(_core->readDataMemory(addr), _core->readDataMemory(addr + 1), _core->readDataMemory(addr + 2), 0);
				} else if (computeScaleFactor() == 4) {
					return syn::encodeValueLE<Data>(_core->readDataMemory(addr), _core->readDataMemory(addr + 1), _core->readDataMemory(addr + 2), _core->readDataMemory(addr + 3));
				} else {
					throw syn::Problem("Please provide a custom implementation of write!");
				}
			}
		private:
			Address tryComputeActualAddress(Address address) {
				auto actualAddress = address - this->baseAddress();
				if (actualAddress < 0) {
					throw syn::Problem("Given address is less than the base address");
				} else if (actualAddress > computeDataMemoryEnd()) {
					throw syn::Problem("Given address is beyond the memory space!");
				} else {
					return actualAddress;
				}
			}
		private:
			Core* _core;
	};
	template<typename Data, typename Address>
	std::shared_ptr<ExposedCoreDataMemory<Data, Address>> mapData(Core* core, Address base) {
		return std::make_shared<ExposedCoreDataMemory<Data, Address>>(core, base);
	}
	template<typename Data, typename Address>
	std::shared_ptr<ExposedCoreDataMemory<Data, Address>> mapData(Core* core, Address base, Address length) {
		return std::make_shared<ExposedCoreDataMemory<Data, Address>>(core, base, length);
	}
	Core* newCore() noexcept;
	void assemble(const std::string& inputFileName, FILE* input, std::ostream* output);
}
#endif
