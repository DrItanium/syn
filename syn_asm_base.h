// Basic assembler routines that all syn targets use
#ifndef IRIS_ASM_BASE_H
#define IRIS_ASM_BASE_H
#include <functional>
#include <sstream>
#include "Base.h"
#include <bitset>
#include <pegtl.hh>
#include <pegtl/contrib/raw_string.hh>
#include <pegtl/contrib/abnf.hh>
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
		constexpr auto width = syn::bitwidth<T>();
		static_assert(width <= syn::bitwidth<unsigned long long>(), "Please provide custom implementation of getBinaryImmediate since type is too large to fit in a unsigned long long!");
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
	struct GenericRegister : public pegtl::if_must<pegtl::one<prefix>, pegtl::plus<pegtl::digit>> { };

    struct EndOfLineComment : public pegtl::until<pegtl::eolf> { };
    template<char tag>
    struct SingleLineComment : public pegtl::disable<pegtl::one<tag>, EndOfLineComment> { };

	struct AsmSeparator : public pegtl::plus<pegtl::ascii::space> { };

    template<typename Rule>
    struct Indirection : public pegtl::seq<Rule> { };

    template<typename C0, typename C1, typename Separator = AsmSeparator>
    struct TwoPartComponent : public pegtl::seq<C0, Separator, C1> { };

    template<typename Register>
    struct OneRegister : public pegtl::seq<Register> { };
    template<typename R0, typename R1, typename Separator = AsmSeparator>
    struct TwoRegister : public TwoPartComponent<R0, R1, Separator> { };
    template<typename R0, typename R1, typename R2, typename Separator0 = AsmSeparator, typename Separator1 = AsmSeparator>
    struct ThreeRegister : public pegtl::seq<R0, Separator0, R1, Separator1, R2> { };


    template<char delimiter, typename SymbolClass>
    struct GenericNumeral : public pegtl::if_must<pegtl::istring<'0', delimiter>, pegtl::plus<SymbolClass>> { };

    template<typename Src0, typename Src1, typename Separator = AsmSeparator>
    struct SourceRegisters : public TwoRegister<Src0, Src1, Separator> { };

    struct Lexeme : public pegtl::identifier { };

    template<typename Other>
    struct LexemeOr : public pegtl::sor<Lexeme, Other> { };

    template<typename Operation, typename Operands, typename Separator = AsmSeparator>
    struct Instruction : public pegtl::seq<Operation, Separator, Operands> { };


    template<typename Entry>
    struct MainFileParser :  public pegtl::until<pegtl::eof, pegtl::must<Entry>> { };

} // end namespace syn


#endif // end IRIS_ASM_BASE_H
