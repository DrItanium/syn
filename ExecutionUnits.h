/*
 * syn
 * Copyright (c) 2013-2017, Joshua Scoggins and Contributors
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */


// define Execution Units to cut down on the amount of repeated actions
#ifndef _SYN_XUNITS_H
#define _SYN_XUNITS_H
#include "Base.h"
#include "IODevice.h"
#include <cmath>
namespace syn {

template<typename T, typename Op, typename R = T>
class BinaryOperationUnit {
    public:
        using WordType = T;
        using ReturnType = R;
        using Operation = Op;
    public:
        BinaryOperationUnit() { }
        virtual ~BinaryOperationUnit() { }
        virtual ReturnType performOperation(Operation op, WordType a, WordType b) const = 0;
        inline ReturnType operator()(Operation op, WordType a, WordType b) const {
            return this->performOperation(op, a, b);
        }
};

namespace FPU {
    enum class StandardOperations {
        Add,
        Subtract,
        Multiply,
        Divide,
        SquareRoot,
        Count,
    };
    template<typename Word>
        class Unit : public BinaryOperationUnit<Word, StandardOperations>{
            public:
                using Parent = BinaryOperationUnit<Word, StandardOperations>;
                using Operation = typename Parent::Operation;
            public:
                using Parent::Parent;
                virtual Word performOperation(Operation op, Word a, Word b) const override {
                    switch(op) {
                        case Operation::Add:
                            return syn::add<Word>(a, b);
                        case Operation::Subtract:
                            return syn::sub<Word>(a, b);
                        case Operation::Multiply:
                            return syn::mul<Word>(a, b);
                        case Operation::Divide:
                            return syn::div<Word>(a, b);
                        case Operation::SquareRoot:
                            return static_cast<Word>(sqrt(static_cast<double>(a)));
                        default:
                            throw syn::Problem("Undefined fpu operation!");
                    }
                }
        };
}

namespace ALU {
    enum class StandardOperations {
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
    template<typename Word>
    class Unit : public BinaryOperationUnit<Word, StandardOperations> {
        public:
            using Parent = BinaryOperationUnit<Word, StandardOperations>;
            using Operation = typename Parent::Operation;
        public:
            using Parent::Parent;
            virtual Word performOperation(Operation op, Word a, Word b) const override {
                switch(op) {
                    case Operation::Add:
                        return syn::add<Word>(a, b);
                    case Operation::Subtract:
                        return syn::sub<Word>(a, b);
                    case Operation::Multiply:
                        return syn::mul<Word>(a, b);
                    case Operation::Divide:
                        return syn::div<Word>(a, b);
                    case Operation::Remainder:
                        return syn::rem<Word>(a, b);
                    case Operation::ShiftLeft:
                        return syn::shiftLeft<Word>(a, b);
                    case Operation::ShiftRight:
                        return syn::shiftRight<Word>(a, b);
                    case Operation::BinaryAnd:
                        return syn::binaryAnd<Word>(a, b);
                    case Operation::BinaryOr:
                        return syn::binaryOr<Word>(a, b);
                    case Operation::UnaryNot:
                        return syn::binaryNot<Word>(a);
                    case Operation::BinaryXor:
                        return syn::binaryXor<Word>(a, b);
                    case Operation::BinaryNand:
                        return syn::binaryNand<Word>(a, b);
                    case Operation::CircularShiftLeft:
                        return syn::circularShiftLeft<Word>(a, b);
                    case Operation::CircularShiftRight:
                        return syn::circularShiftRight<Word>(a, b);
                    default:
                        throw syn::Problem("Undefined ALU operation!");
                }
        }
    };
} // end namespace ALU

namespace Comparator {
    enum class StandardOperations {
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
    template<typename Word, typename Return = Word>
    class Unit : public BinaryOperationUnit<Word, StandardOperations, Return> {
        public:
            using Parent = BinaryOperationUnit<Word, StandardOperations, Return>;
            using Operation = typename Parent::Operation;
        public:
            using Parent::Parent;
            virtual Return performOperation(Operation op, Word a, Word b) const override {
                switch(op) {
                    case Operation::Eq:
                        return syn::eq<Word, Return>(a, b);
                    case Operation::Neq:
                        return syn::neq<Word, Return>(a, b);
                    case Operation::LessThan:
                        return syn::lt<Word, Return>(a, b);
                    case Operation::GreaterThan:
                        return syn::gt<Word, Return>(a, b);
                    case Operation::LessThanOrEqualTo:
                        return syn::le<Word, Return>(a, b);
                    case Operation::GreaterThanOrEqualTo:
                        return syn::ge<Word, Return>(a, b);
                    case Operation::BinaryAnd:
                        return syn::binaryAnd<Word, Return>(a, b);
                    case Operation::BinaryOr:
                        return syn::binaryOr<Word, Return>(a, b);
                    case Operation::UnaryNot:
                        return syn::binaryNot<Word, Return>(a);
                    case Operation::BinaryXor:
                        return syn::binaryXor<Word, Return>(a, b);
                    case Operation::BinaryNand:
                        return syn::binaryNand<Word, Return>(a, b);
                    case Operation::ShiftLeft:
                        return syn::shiftLeft<Word, Return>(a, b);
                    case Operation::ShiftRight:
                        return syn::shiftRight<Word, Return>(a, b);
    				case Operation::BinaryNor:
    					return syn::binaryNor<Word, Return>(a, b);
                    case Operation::CircularShiftLeft:
                        return syn::circularShiftLeft<Word, Return>(a, b);
                    case Operation::CircularShiftRight:
                        return syn::circularShiftRight<Word, Return>(a, b);
                    default:
                        throw syn::Problem("Undefined Comparison operation!");
                }
            }
    };

    template<>
    class Unit<bool, bool> {
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
    		Unit() { }
    		virtual ~Unit() { }
    		inline bool performOperation(Operation op, bool a, bool b) const {
    			switch(op) {
    				case Operation::Eq:
    					return syn::eq<bool>(a, b);
    				case Operation::Neq:
    					return syn::neq<bool>(a, b);
    				case Operation::BinaryAnd:
    					return syn::binaryAnd<bool>(a, b);
    				case Operation::BinaryOr:
    					return syn::binaryOr<bool>(a, b);
    				case Operation::BinaryXor:
    					return syn::binaryXor<bool>(a, b);
    				case Operation::UnaryNot:
    					return syn::binaryNot<bool>(a);
    				case Operation::BinaryNand:
    					return syn::binaryNand<bool>(a, b);
    				case Operation::BinaryNor:
    					return syn::binaryNor<bool>(a, b);
    				default:
    					throw syn::Problem("Undefined boolean comparison operation!");
    			}
    		}
            inline bool operator()(Operation op, bool a, bool b) const {
                return this->performOperation(op, a, b);
            }
    };
} // end namespace Comparator

template<typename Word, typename Address = Word>
class LoadStoreUnit : public AddressableIODevice<Word, Address> {
	public:
		using WordType = Word;
		using AddressType = Address;
		using Parent = AddressableIODevice<Word, Address>;
        enum class Operation {
            Zero,
            Swap,
            Load,
            Store,
            Copy,
            Count,
        };
	public:
		LoadStoreUnit(Address size, Address base = 0) : Parent(base, size), _memory(std::move(std::make_unique<Word[]>(size))), _size(size) { }
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
				throw syn::Problem("Provided address is not legal");
			}
		}
		inline Word& retrieveMemory(Address addr) {
			if (legalAddress(addr)) {
				return _memory[addr];
			} else {
				throw syn::Problem("Provided address is not legal");
			}
		}
		virtual Word read(Address addr) override {
			return retrieveMemory(addr);
		}
		virtual void write(Address addr, Word value) override {
			set(addr, value);
		}
		Word& operator[](Address addr) {
			return retrieveMemory(addr);
		}
		void swap(Address a, Address b) {
			syn::swap<Word>(_memory[a], _memory[b]);
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
        static constexpr auto count = capacity;
        using Parent = LoadStoreUnit<Word, Address>;
	public:
		FixedSizeLoadStoreUnit(Address base = 0) : Parent(capacity, base) { }
		virtual ~FixedSizeLoadStoreUnit() { }
};

template<typename T, T addressMask>
class Register {
    public:
        using AddressType = T;
        using Self = Register<T, addressMask>;
        static constexpr T getAddressMask() noexcept { return addressMask; }
        static constexpr T generateProperAddress(T value) noexcept { return syn::decodeBits<T, T, getAddressMask(), 0>(value); }
    public:
        Register(T value) noexcept : _value(value) { }
        virtual ~Register() { }
        inline T get() const noexcept { return _value; }
        inline void set(T value) noexcept { _value = generateProperAddress(value); }
        inline void increment(T value = 1) noexcept { set(get() + value); }
        inline void decrement(T value = 1) noexcept { set(get() - value); }
        inline Self& operator++() {
            increment();
            return *this;
        }
        inline Self& operator--() {
            decrement();
            return *this;
        }
    private:
        T _value;

};

} // end namespace syn
#endif // end _SYN_XUNITS_H
