// Basic assembler routines that all iris targets use
#ifndef IRIS_ASM_BASE_H
#define IRIS_ASM_BASE_H
#include <functional>
#include <sstream>
#include "iris_base.h"
#include <bitset>
namespace iris { 
	using ErrorReportingFunction = std::function<void(const std::string&)>;
	template<typename T, T count> 
	T getRegister(const char* text, ErrorReportingFunction onError) noexcept {
		T value = 0;
		std::string wrapper(text);
		wrapper[0] = '0';
		std::istringstream ss(wrapper);
		ss >> value;
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
	template<typename T>
	T getBinaryImmediate(const char* text, ErrorReportingFunction onError) noexcept {
		std::string temp(text);
		// always replace the second element as it is always the b, x, etc
		temp[1] = '0';
		constexpr auto width = iris::bitwidth<T>();
		static_assert(width <= iris::bitwidth<unsigned long long>(), "Please provide custom implementation of getBinaryImmediate since type is too large to fit in a unsigned long long!");
		std::bitset<width> bits(temp);
		return static_cast<T>(bits.to_ullong());
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
	T getOctalImmediate(const char* text, ErrorReportingFunction onError) noexcept {
		T value = 0;
		std::string wrapper(text);
		wrapper[1] = '0';
		std::istringstream ss(wrapper);
		ss >> std::oct >> value;
		return static_cast<T>(value);
	}

	template<typename T>
	T getDecimalImmediate(const char* text, ErrorReportingFunction onError) noexcept {
		T value = 0;
		std::string wrapper(text);
		std::istringstream ss(wrapper);
		ss >> std::dec >> value;
		return static_cast<T>(value);
	}
} // end namespace iris


#endif // end IRIS_ASM_BASE_H
