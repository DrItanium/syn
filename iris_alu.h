// define an ALU to cut down on the amount of repeated actions
#ifndef _IRIS_ALU_H
#define _IRIS_ALU_H
#include "iris_base.h"
#include <iostream>
namespace iris {

template<typename Word>
class ALU  {
    public:
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
            Count,
        };
    public:
        ALU() { }
        virtual ~ALU() { }
        inline Word performOperation(Operation op, Word a, Word b) const {
            switch(op) {
                case Operation::Add:
                    return iris::add<Word>(a, b);
                case Operation::Subtract:
                    return iris::sub<Word>(a, b);
                case Operation::Multiply:
                    return iris::mul<Word>(a, b);
                case Operation::Divide:
                    return iris::div<Word>(a, b);
                case Operation::Remainder:
                    return iris::rem<Word>(a, b);
                case Operation::ShiftLeft:
                    return iris::shiftLeft<Word>(a, b);
                case Operation::ShiftRight:
                    return iris::shiftRight<Word>(a, b);
                case Operation::BinaryAnd:
                    return iris::binaryAnd<Word>(a, b);
                case Operation::BinaryOr:
                    return iris::binaryOr<Word>(a, b);
                case Operation::UnaryNot:
                    return iris::binaryNot<Word>(a);
                case Operation::BinaryXor:
                    return iris::binaryXor<Word>(a, b);
                case Operation::BinaryNand:
                    return iris::binaryNand<Word>(a, b);
                default:
                    throw iris::Problem("Undefined ALU operation!");
            }
        }
};

template<typename Word>
class Comparator {
    public:
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
            Count,
        };
    public:
        Comparator() { }
        virtual ~Comparator() { }
        inline Word performOperation(Operation op, Word a, Word b) const {
            switch(op) {
                case Operation::Eq:
                    return iris::eq<Word>(a, b);
                case Operation::Neq:
                    return iris::neq<Word>(a, b);
                case Operation::LessThan:
                    return iris::lt<Word>(a, b);
                case Operation::GreaterThan:
                    return iris::gt<Word>(a, b);
                case Operation::LessThanOrEqualTo:
                    return iris::le<Word>(a, b);
                case Operation::GreaterThanOrEqualTo:
                    return iris::ge<Word>(a, b);
                case Operation::BinaryAnd:
                    return iris::binaryAnd<Word>(a, b);
                case Operation::BinaryOr:
                    return iris::binaryOr<Word>(a, b);
                case Operation::UnaryNot:
                    return iris::binaryNot<Word>(a);
                case Operation::BinaryXor:
                    return iris::binaryXor<Word>(a, b);
                case Operation::BinaryNand:
                    return iris::binaryNand<Word>(a, b);
                case Operation::ShiftLeft:
                    return iris::shiftLeft<Word>(a, b);
                case Operation::ShiftRight:
                    return iris::shiftRight<Word>(a, b);
                default:
                    throw iris::Problem("Undefined Comparison operation!");
            }
        }
};

template<typename Word, typename Address = Word>
class LoadStoreUnit {
	public:
		LoadStoreUnit(Address size) : _memory(std::move(std::make_unique<Word[]>(size))), _size(size) { }
		virtual ~LoadStoreUnit() { }
		inline Address getSize() const noexcept { return _size; }
		inline bool legalAddress(Address addr) const noexcept {
			return addr >= 0 && addr < _size; 
		}
		Word& operator[](Address addr) {
			if (legalAddress(addr)) {
				return _memory[addr];
			} else {
				throw iris::Problem("Provided address is not legal!");
			}
		}
		void set(Address addr, Word value) {
			if (legalAddress(addr)) {
				_memory[addr] = value;
			} else {
				throw iris::Problem("Provided address is not legal");
			}
		}
		void swap(Address a, Address b) {
			iris::swap<Word>(_memory[a], _memory[b]);
		}
		void copy(Address a, Address b) {
			_memory[a] = _memory[b];
		}
		void install(std::istream& stream, std::function<Word(char*)> decode) {
			char buf[sizeof(Word)] = { 0 };
			for (auto i = 0; i < _size; ++i) {
				stream.read(buf, sizeof(Word));
				_memory[i] = decode(buf);
			}
		}
		void dump(std::ostream& stream, std::function<void(Word, char*)> encode) {
			char buf[sizeof(Word)] = { 0 };
			for (auto i = 0; i < _size; ++i) {
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


} // end namespace iris
#endif // end _IRIS_ALU_H
