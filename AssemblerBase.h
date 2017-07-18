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


// Basic assembler routines that all syn targets use
#ifndef IRIS_ASM_BASE_H
#define IRIS_ASM_BASE_H
#include <functional>
#include <sstream>
#include <bitset>
#include <map>
#include <pegtl.hh>
#include <pegtl/contrib/raw_string.hh>
#include <pegtl/contrib/abnf.hh>

#include "BaseArithmetic.h"
namespace syn {
	using ErrorReportingFunction = std::function<void(const std::string&)>;
	template<typename T, T count>
	T getRegister(const char* text, ErrorReportingFunction onError) noexcept {
        static_assert(count > 0, "Can't have zero registers!");
		T value = 0;
		std::string wrapper(text);
		wrapper[0] = '0';
		std::istringstream ss(wrapper);
		ss >> std::dec >> value;
		if (value >= count) {
			onError("target register is too large!");
			return static_cast<T>(0);
		} else if (value < 0) {
			onError("target register is less than zero!");
			return static_cast<T>(0);
		} else {
			return static_cast<T>(value);
		}
	}
    template<typename T, T count>
    T getRegister(const std::string& text, ErrorReportingFunction onError) noexcept {
        return getRegister<T,count>(text.c_str(), onError);
    }
	template<typename T>
	T getBinaryImmediate(const char* text, ErrorReportingFunction onError) noexcept {
		std::string temp(text);
		// always replace the second element as it is always the b, x, etc
		temp[1] = '0';
		constexpr auto width = syn::bitwidth<T>;
		static_assert(width <= syn::bitwidth<unsigned long long>, "Please provide custom implementation of getBinaryImmediate since type is too large to fit in a unsigned long long!");
		std::bitset<width> bits(temp);
		return static_cast<T>(bits.to_ullong());
	}
    template<typename T>
    T getBinaryImmediate(const std::string& text, ErrorReportingFunction onError) noexcept {
        return getBinaryImmediate<T>(text.c_str(), onError);
    }


	template<typename T>
	T getHexImmediate(const char* text, ErrorReportingFunction onError) noexcept {
		T value = 0;
		std::string wrapper(text);
		wrapper[1] = '0';
		std::istringstream ss(wrapper);
		ss >> std::hex >> value;
		return static_cast<T>(value);
	}
    template<typename T>
    T getHexImmediate(const std::string& text, ErrorReportingFunction onError) noexcept {
        return getHexImmediate<T>(text.c_str(), onError);
    }

	template<typename T>
	T getOctalImmediate(const char* text, ErrorReportingFunction onError) noexcept {
		T value = 0;
		std::string wrapper(text);
		wrapper[1] = '0';
		std::istringstream ss(wrapper);
		ss >> std::oct >> value;
		return static_cast<T>(value);
	}
    template<typename T>
    T getOctalImmediate(const std::string& text, ErrorReportingFunction onError) noexcept {
        return getOctalImmediate<T>(text.c_str(), onError);
    }

	template<typename T>
	T getDecimalImmediate(const char* text, ErrorReportingFunction onError) noexcept {
		T value = 0;
		std::string wrapper(text);
		std::istringstream ss(wrapper);
		ss >> std::dec >> value;
		return static_cast<T>(value);
	}
    template<typename T>
    T getDecimalImmediate(const std::string& text, ErrorReportingFunction onError) noexcept {
        return getDecimalImmediate<T>(text.c_str(), onError);
    }
	template<char prefix>
	struct GenericRegister : pegtl::if_must<pegtl::one<prefix>, pegtl::plus<pegtl::digit>> { };

    using GPR = GenericRegister<'r'>;
    using FloatRegister = GenericRegister<'f'>;
    using PredicateRegister = GenericRegister<'p'>;

    struct EndOfLineComment : pegtl::until<pegtl::eolf> { };
    template<char tag>
    struct SingleLineComment : pegtl::disable<pegtl::one<tag>, EndOfLineComment> { };

	struct AsmSeparator : pegtl::plus<pegtl::ascii::space> { };

    template<typename Rule>
    struct SingleEntrySequence : pegtl::seq<Rule> { };

    template<typename C0, typename C1, typename Separator = AsmSeparator>
    struct TwoPartComponent : pegtl::seq<C0, Separator, C1> { };

    template<typename State, typename C0, typename C1, typename Separator = AsmSeparator>
    struct StatefulTwoPartComponent : pegtl::state<State, TwoPartComponent<C0, C1, Separator>> { };


	template<typename First, typename Second, typename Third, typename Sep0 = AsmSeparator, typename Sep1 = AsmSeparator>
	struct ThreePartComponent : pegtl::seq<First, Sep0, Second, Sep1, Third> { };

	template<typename State, typename First, typename Second, typename Third, typename Sep0 = AsmSeparator, typename Sep1 = AsmSeparator>
	struct StatefulThreePartComponent : pegtl::state<State, ThreePartComponent<First, Second, Third, Sep0, Sep1>> { };


    template<typename Register>
    struct OneRegister : pegtl::seq<Register> { };
    template<typename R0, typename R1, typename Separator = AsmSeparator>
    struct TwoRegister : TwoPartComponent<R0, R1, Separator> { };
    template<typename R0, typename R1, typename R2, typename Separator0 = AsmSeparator, typename Separator1 = AsmSeparator>
	struct ThreeRegister : ThreePartComponent<R0, R1, R2, Separator0, Separator1> { };


    template<char delimiter, typename SymbolClass>
    struct GenericNumeral : pegtl::if_must<pegtl::istring<'0', delimiter>, pegtl::plus<SymbolClass>> { };

	template<char delim = 'x'>
	struct Base16Number : GenericNumeral<delim, pegtl::xdigit> { };
	using HexadecimalNumber = Base16Number<'x'>;

	template<char delim = 'b'>
	struct Base2Number : GenericNumeral<delim, pegtl::abnf::BIT> { };
	using BinaryNumber = Base2Number<'b'>;

	struct Base10Number : pegtl::plus<pegtl::digit> { };

    template<typename Src0, typename Src1, typename Separator = AsmSeparator>
    struct SourceRegisters : TwoRegister<Src0, Src1, Separator> { };

    struct Lexeme : pegtl::identifier { };

    template<typename Other>
    struct LexemeOr : pegtl::sor<Lexeme, Other> { };

    template<typename Operation, typename Operands, typename Separator = AsmSeparator>
    struct Instruction : pegtl::seq<Operation, Separator, Operands> { };


    template<typename End, typename Entry>
    struct MainParser : pegtl::until<End, pegtl::must<Entry>> { };
    template<typename Entry>
    struct MainFileParser :  MainParser<pegtl::eof, Entry> { };


	template<typename S, typename ... NumberTypes>
	struct StatefulNumber : pegtl::state<S, pegtl::sor<NumberTypes...>> { };

	template<typename S>
	struct StatefulNumberAll : StatefulNumber<S, HexadecimalNumber, Base10Number, BinaryNumber> { };
	/**
	 * Used to store the final numeric representation of a system word,
	 * instructions are a multiple of the system word so this class makes it
	 * simpler to install instructions into a core or whatever else.
	 */
	template<typename Word, typename Address = Word>
	class AssemblerWord {
		public:
			AssemblerWord(Address currAddress, Word value, int width = 1) : _width(width), _currAddress(currAddress), _value(value), _isLabel(false) { }
			AssemblerWord(Address currAddress, const std::string& label, int width = 1) : _width(width), _currAddress(currAddress), _value(0), _isLabel(true), _label(label) { }
			virtual ~AssemblerWord() { }
			inline Address getAddress() const noexcept { return _currAddress; }
			inline Word getValue() const noexcept { return _value; }
			inline void setValue(Word value) noexcept { _value = value; }
			inline bool isLabel() const noexcept { return _isLabel; }
			inline const std::string& getLabel() const noexcept { return _label; }
			inline int getWidth() const noexcept { return _width; }
		protected:
			int _width;
			Address _currAddress;
			Word _value;
			bool _isLabel;
			bool _resolveLabel;
			std::string _label;
	};

    template<typename Word>
    class NumberContainer {
        public:
            template<typename Input, typename ... States>
                NumberContainer(const Input& in, States&& ...) { }
            virtual ~NumberContainer() { }
            Word getValue() const noexcept { return _value; }
            void setValue(Word value) noexcept { _value = value; }
        private:
            Word _value;
    };
	template<typename Address>
	class NameToAddressMapping : public NumberContainer<Address> {
		public:
			using NumberContainer<Address>::NumberContainer;
			const std::string& getTitle() const noexcept { return _title; }
			void setTitle(const std::string& value) noexcept { _title = value; }
		private:
			std::string _title;
	};

#define DefSymbol(title, str) \
    struct Symbol ## title : public pegtl_string_t ( #str ) { }

    DefSymbol(OrgDirective, .org);
    DefSymbol(LabelDirective, .label);
    DefSymbol(WordDirective, .word);
    DefSymbol(DwordDirective, .dword);
    DefSymbol(AndKeyword, and);
    DefSymbol(OrKeyword, or);
    DefSymbol(NotKeyword, not);
    DefSymbol(AddKeyword, add);
    DefSymbol(SubKeyword, sub);
    DefSymbol(MulKeyword, mul);
    DefSymbol(DivKeyword, div);
    DefSymbol(RemKeyword, rem);


    template<typename Symbol, typename Value, typename Separator = AsmSeparator>
    struct OneArgumentDirective : TwoPartComponent<Symbol, Value, Separator> { };

    template<typename State, typename Symbol, typename Value, typename Separator = AsmSeparator>
    struct StatefulOneArgumentDirective : StatefulTwoPartComponent<State, Symbol, Value, Separator> { };

	template<typename State, typename Data>
	struct WordDirective : syn::StatefulOneArgumentDirective<State, SymbolWordDirective, Data> { };

	template<typename State, typename Data>
	struct DwordDirective : syn::StatefulOneArgumentDirective<State, SymbolDwordDirective, Data> { };

    template<typename State, typename Number, typename Separator = AsmSeparator>
    struct StatefulOrgDirective : StatefulOneArgumentDirective<State, SymbolOrgDirective, Number, Separator> { };

    template<typename State, typename Lexeme, typename Separator = AsmSeparator>
    struct StatefulLabelDirective : StatefulOneArgumentDirective<State, SymbolLabelDirective, Lexeme, Separator> { };

    template<typename State, typename C>
    struct StatefulSingleEntrySequence : pegtl::state<State, SingleEntrySequence<C>> { };


	template<typename R>
	struct Action : pegtl::nothing<R> { };

	void reportError(const std::string& msg);
	template<typename T>
	T parseHex(const std::string& str) {
		return getHexImmediate<T>(str, reportError);
	}

	template<typename T>
	T parseBinary(const std::string& str) {
		return getBinaryImmediate<T>(str, reportError);
	}

	template<typename T>
	T parseDecimal(const std::string& str) {
		return getDecimalImmediate<T>(str, reportError);
	}

	enum class KnownNumberTypes {
		Decimal,
		Binary,
		Hexadecimal,
	};
	template<typename T, KnownNumberTypes type>
	void populateContainer(const std::string& str, NumberContainer<T>& parent) {
		switch(type) {
			case KnownNumberTypes::Decimal:
				parent.setValue(parseDecimal<T>(str));
				break;
			case KnownNumberTypes::Hexadecimal:
				parent.setValue(parseHex<T>(str));
				break;
			case KnownNumberTypes::Binary:
				parent.setValue(parseBinary<T>(str));
				break;
			default:
				reportError("Unimplemented known number type!");
		}
	}

#define DefAction(rule) template<> struct Action< rule >
#define DefApplyGeneric(type) template<typename Input> static void apply(const Input& in, type & state)
#define DefApplyGenericEmpty(type) DefApplyGeneric(type) { }

	template<typename Address>
	class LabelTracker {
		public:
			using LabelMap = std::map<std::string, Address>;
			using LabelMapIterator = typename LabelMap::iterator;
			using ConstLabelMapIterator = typename LabelMap::const_iterator;
		public:
			LabelTracker() { }
			virtual ~LabelTracker() { }
			LabelMapIterator labelsEnd() noexcept { return _labels.end(); }
			LabelMapIterator labelsBegin() noexcept { return _labels.begin(); }
			ConstLabelMapIterator labelsCBegin() const noexcept { return _labels.cbegin(); }
			ConstLabelMapIterator labelsCEnd() const noexcept { return _labels.cend(); }

			LabelMapIterator findLabel(const std::string& name) noexcept {
				return _labels.find(name);
			}

			ConstLabelMapIterator findLabel(const std::string& name) const noexcept {
				return _labels.find(name);
			}

			void registerLabel(const std::string& name, Address value) {
				_labels.emplace(name, value);
			}
		protected:
			LabelMap _labels;
	};

	template<typename Address>
	class AddressTracker {
		public:
			AddressTracker(Address initialVal) noexcept : _currentAddress(initialVal) { }
			AddressTracker() noexcept : _currentAddress(0) { }
			virtual ~AddressTracker() { _currentAddress = 0; }
			void setCurrentAddress(Address address) noexcept { _currentAddress = address; }
			void incrementCurrentAddress() noexcept { ++_currentAddress; }
			void incrementCurrentAddress(Address val) noexcept { _currentAddress += val; }
			Address getCurrentAddress() const noexcept { return _currentAddress; }
			void reset() noexcept { _currentAddress = 0; }
		protected:
			Address _currentAddress = 0;
	};

	template<typename T>
	class FinishedDataTracker {
		public:
			using FinishedDataWithIndexFunc = std::function<void(T&, size_t)>;
			using FinishedDataFunc = std::function<void(T&)>;
			using FinishedDataStorage = std::vector<T>;
			FinishedDataTracker() { }
			virtual ~FinishedDataTracker() { }
			void applyToFinishedData(FinishedDataWithIndexFunc fn) {
				size_t index = 0;
				for (auto& value : _finishedData) {
					fn(value, index);
					++index;
				}
			}
			void applyToFinishedData(FinishedDataFunc fn) {
				for (auto& value : _finishedData) {
					fn(value);
				}
			}
			void addToFinishedData(T& value) {
				_finishedData.emplace_back(value);
			}
			void copyToFinishedData(T value) {
				_finishedData.push_back(value);
			}

			size_t numberOfFinishedItems() const {
				return _finishedData.size();
			}
			void reset() noexcept {
				_finishedData.clear();
			}
		private:
			FinishedDataStorage _finishedData;
	};


} // end namespace syn


#endif // end IRIS_ASM_BASE_H
