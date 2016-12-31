// define Execution Units to cut down on the amount of repeated actions
#ifndef _IRIS_XUNITS_H
#define _IRIS_XUNITS_H
#include "iris_base.h"
#include "IODevice.h"
#include <iostream>
#include <cmath>
namespace stdiris {

template<typename Word>
class FPU {
    public:
        using WordType = Word;
        enum class Operation {
            Add,
            Subtract,
            Multiply,
            Divide,
            SquareRoot,
            Count,
        };
    public:
        FPU() { }
        virtual ~FPU() { }
        inline Word performOperation(Operation op, Word a, Word b) const {
            switch(op) {
                case Operation::Add:
                    return stdiris::add<Word>(a, b);
                case Operation::Subtract:
                    return stdiris::sub<Word>(a, b);
                case Operation::Multiply:
                    return stdiris::mul<Word>(a, b);
                case Operation::Divide:
                    return stdiris::div<Word>(a, b);
                case Operation::SquareRoot:
                    return static_cast<Word>(sqrt(static_cast<double>(a)));
                default:
                    throw stdiris::Problem("Undefined fpu operation!");
            }
        }
};

template<typename Word>
class ALU {
    public:
		using WordType = Word;
        enum class Operation {
            Add,
            Subtract,
            Multiply,
            Divide,
            Remainder,
            ShiftLeft,
            ShiftRight,
            BinaryAnd,
            BinaryOr,
            UnaryNot,
            BinaryXor,
            BinaryNand,
            CircularShiftLeft,
            CircularShiftRight,
            Count,
        };
    public:
        ALU() { }
        virtual ~ALU() { }
        inline Word performOperation(Operation op, Word a, Word b) const {
            switch(op) {
                case Operation::Add:
                    return stdiris::add<Word>(a, b);
                case Operation::Subtract:
                    return stdiris::sub<Word>(a, b);
                case Operation::Multiply:
                    return stdiris::mul<Word>(a, b);
                case Operation::Divide:
                    return stdiris::div<Word>(a, b);
                case Operation::Remainder:
                    return stdiris::rem<Word>(a, b);
                case Operation::ShiftLeft:
                    return stdiris::shiftLeft<Word>(a, b);
                case Operation::ShiftRight:
                    return stdiris::shiftRight<Word>(a, b);
                case Operation::BinaryAnd:
                    return stdiris::binaryAnd<Word>(a, b);
                case Operation::BinaryOr:
                    return stdiris::binaryOr<Word>(a, b);
                case Operation::UnaryNot:
                    return stdiris::binaryNot<Word>(a);
                case Operation::BinaryXor:
                    return stdiris::binaryXor<Word>(a, b);
                case Operation::BinaryNand:
                    return stdiris::binaryNand<Word>(a, b);
                case Operation::CircularShiftLeft:
                    return stdiris::circularShiftLeft<Word>(a, b);
                case Operation::CircularShiftRight:
                    return stdiris::circularShiftRight<Word>(a, b);
                default:
                    throw stdiris::Problem("Undefined ALU operation!");
            }
        }
};

template<typename Word, typename Return = Word>
class Comparator {
    public:
		using WordType = Word;
		using ReturnType = Return;
        enum class Operation {
            Eq,
            Neq,
            LessThan,
            GreaterThan,
            LessThanOrEqualTo,
            GreaterThanOrEqualTo,
            // extended operations!
            BinaryAnd,
            BinaryOr,
            UnaryNot,
            BinaryXor,
            BinaryNand,
            ShiftLeft,
            ShiftRight,
            CircularShiftLeft,
            CircularShiftRight,
			BinaryNor,
            Count,
        };
    public:
        Comparator() { }
        virtual ~Comparator() { }
        inline Return performOperation(Operation op, Word a, Word b) const {
            switch(op) {
                case Operation::Eq:
                    return stdiris::eq<Word, Return>(a, b);
                case Operation::Neq:
                    return stdiris::neq<Word, Return>(a, b);
                case Operation::LessThan:
                    return stdiris::lt<Word, Return>(a, b);
                case Operation::GreaterThan:
                    return stdiris::gt<Word, Return>(a, b);
                case Operation::LessThanOrEqualTo:
                    return stdiris::le<Word, Return>(a, b);
                case Operation::GreaterThanOrEqualTo:
                    return stdiris::ge<Word, Return>(a, b);
                case Operation::BinaryAnd:
                    return stdiris::binaryAnd<Word, Return>(a, b);
                case Operation::BinaryOr:
                    return stdiris::binaryOr<Word, Return>(a, b);
                case Operation::UnaryNot:
                    return stdiris::binaryNot<Word, Return>(a);
                case Operation::BinaryXor:
                    return stdiris::binaryXor<Word, Return>(a, b);
                case Operation::BinaryNand:
                    return stdiris::binaryNand<Word, Return>(a, b);
                case Operation::ShiftLeft:
                    return stdiris::shiftLeft<Word, Return>(a, b);
                case Operation::ShiftRight:
                    return stdiris::shiftRight<Word, Return>(a, b);
				case Operation::BinaryNor:
					return stdiris::binaryNor<Word, Return>(a, b);
                case Operation::CircularShiftLeft:
                    return stdiris::circularShiftLeft<Word, Return>(a, b);
                case Operation::CircularShiftRight:
                    return stdiris::circularShiftRight<Word, Return>(a, b);
                default:
                    throw stdiris::Problem("Undefined Comparison operation!");
            }
        }
};

template<>
class Comparator<bool, bool> {
	public:
		using WordType = bool;
		using ReturnType = bool;
		enum class Operation {
			Eq,
			Neq,
			BinaryAnd,
			BinaryOr,
			UnaryNot,
			BinaryXor,
			BinaryNand,
			BinaryNor,
			Count,
		};
	public:
		Comparator() { }
		virtual ~Comparator() { }
		inline bool performOperation(Operation op, bool a, bool b) const {
			switch(op) {
				case Operation::Eq:
					return stdiris::eq<bool>(a, b);
				case Operation::Neq:
					return stdiris::neq<bool>(a, b);
				case Operation::BinaryAnd:
					return stdiris::binaryAnd<bool>(a, b);
				case Operation::BinaryOr:
					return stdiris::binaryOr<bool>(a, b);
				case Operation::BinaryXor:
					return stdiris::binaryXor<bool>(a, b);
				case Operation::UnaryNot:
					return stdiris::binaryNot<bool>(a);
				case Operation::BinaryNand:
					return stdiris::binaryNand<bool>(a, b);
				case Operation::BinaryNor:
					return stdiris::binaryNor<bool>(a, b);
				default:
					throw stdiris::Problem("Undefined boolean comparison operation!");
			}
		}
};

template<typename Word, typename Address = Word>
class LoadStoreUnit : public IODevice<Word, Address> {
	public:
		using WordType = Word;
		using AddressType = Address;
	public:
		LoadStoreUnit(Address size, Address base = 0) : IODevice<Word, Address>(base, size), _memory(std::move(std::make_unique<Word[]>(size))), _size(size) { }
		LoadStoreUnit() : LoadStoreUnit(0) { }
		virtual ~LoadStoreUnit() { }
		inline void zero() noexcept {
			for (Address addr = 0; addr < _size; ++addr) {
				_memory[addr] = 0;
			}
		}
		inline Address getSize() const noexcept { return _size; }
		inline bool legalAddress(Address addr) const noexcept {
			return addr >= 0 && addr < _size;
		}
		void set(Address addr, Word value) {
			if (legalAddress(addr)) {
				_memory[addr] = value;
			} else {
				throw stdiris::Problem("Provided address is not legal");
			}
		}
		inline Word& retrieveMemory(Address addr) {
			if (legalAddress(addr)) {
				return _memory[addr];
			} else {
				throw stdiris::Problem("Provided address is not legal");
			}
		}
		virtual Word read(Address addr) override {
			return retrieveMemory(addr);
		}
		virtual void write(Address addr, Word value) override {
			retrieveMemory(addr) = value;
		}
		Word& operator[](Address addr) {
			return retrieveMemory(addr);
		}
		void swap(Address a, Address b) {
			stdiris::swap<Word>(_memory[a], _memory[b]);
		}
		void copy(Address a, Address b) {
			_memory[a] = _memory[b];
		}
		void install(std::istream& stream, std::function<Word(char*)> decode) {
			char buf[sizeof(Word)] = { 0 };
			for (Address i = 0; i < _size; ++i) {
				stream.read(buf, sizeof(Word));
				_memory[i] = decode(buf);
			}
		}
		void dump(std::ostream& stream, std::function<void(Word, char*)> encode) {
			char buf[sizeof(Word)] = { 0 };
			for (Address i = 0; i < _size; ++i) {
				encode(_memory[i], buf);
				stream.write(buf, sizeof(Word));
			}
		}
	private:
		std::unique_ptr<Word[]> _memory;
		Address _size;
};

template<typename Word, typename Address, Address capacity>
class FixedSizeLoadStoreUnit : public LoadStoreUnit<Word, Address> {
	public:
		FixedSizeLoadStoreUnit() : LoadStoreUnit<Word, Address>(capacity) { }
		virtual ~FixedSizeLoadStoreUnit() { }
};

class BooleanCombineUnit {
	public:
		enum class Operation {
			None,
			And,
			Or,
			Xor,
			Nand,
			Nor,
			Xnor,
		};
	public:
		bool performOperation(Operation op, bool newValue, bool oldValue) {
			switch(op) {
				case Operation::None:
					return newValue;
				case Operation::And:
					return oldValue && newValue;
				case Operation::Or:
					return oldValue || newValue;
				case Operation::Xor:
					return oldValue ^ newValue;
				case Operation::Nand:
					return !(oldValue && newValue);
				case Operation::Nor:
					return !(oldValue || newValue);
				case Operation::Xnor:
					return !(oldValue ^ newValue);
				default:
					throw stdiris::Problem("Undefined boolean operation!");
			}
		}
};

} // end namespace stdiris
#endif // end _IRIS_XUNITS_H
