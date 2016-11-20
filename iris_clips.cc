#include "iris_clips.h"
#include "iris_base.h"
#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/trim.hpp>
#include <cstdint>
#include <climits>
#include <sstream>
#include <memory>
#include <map>
#include <iostream>

extern "C" {
#include "clips.h"
}

namespace iris {
	void CLIPS_translateBitmask(UDFContext* context, CLIPSValue* ret) {
		CLIPSValue value;
		if (!UDFFirstArgument(context, LEXEME_TYPES, &value)) {
			CVSetBoolean(ret, false);
		} else {
			std::string str(CVToString(&value));
			if (boost::starts_with(str, "0m")) {
				str.at(1) = '0';
				auto tmp = strtoul(str.c_str(), NULL, 2);
				if (tmp == ULONG_MAX && errno == ERANGE) {
					UDFInvalidArgumentMessage(context, "number is too large and overflowed");
					CVSetBoolean(ret, false);
				} else {
					if (tmp > 0xFF) {
						UDFInvalidArgumentMessage(context, "provided number is larger than 8-bits!");
						CVSetBoolean(ret, false);
					} else {
						CVSetInteger(ret, static_cast<CLIPSInteger>(static_cast<byte>(tmp)));
					}
				}
			} else {
				UDFInvalidArgumentMessage(context, "Bitmask must start with 0m");
				CVSetBoolean(ret, false);
			}
		}
	}
	void CLIPS_translateBinary(UDFContext* context, CLIPSValue* ret) {
		CLIPSValue value;
		if (!UDFFirstArgument(context, LEXEME_TYPES, &value)) {
			CVSetBoolean(ret, false);
		} else {
			std::string str(CVToString(&value));
			if (boost::starts_with(str, "0b")) {
				str.at(1) = '0';
				auto tmp = strtoull(str.c_str(), NULL, 2);
				if (tmp == ULLONG_MAX && errno == ERANGE) {
					UDFInvalidArgumentMessage(context, "number is too large and overflowed");
					CVSetBoolean(ret, false);
				} else {
					if (tmp > 0xFFFFFFFFFFFFFFFF) {
						UDFInvalidArgumentMessage(context, "provided number is larger than 64-bits!");
						CVSetBoolean(ret, false);
					} else {
						CVSetInteger(ret, static_cast<CLIPSInteger>(tmp));
					}
				}
			} else {
				UDFInvalidArgumentMessage(context, "Binary must start with 0b");
				CVSetBoolean(ret, false);
			}
		}
	}

	void CLIPS_translateHex(UDFContext* context, CLIPSValue* ret) {
		CLIPSValue value;
		if (!UDFFirstArgument(context, LEXEME_TYPES, &value)) {
			CVSetBoolean(ret, false);
		} else {
			std::string str(CVToString(&value));
			if (boost::starts_with(str, "0x")) {
				str.at(1) = '0';
				auto tmp = strtoull(str.c_str(), NULL, 16);
				if (tmp == ULLONG_MAX && errno == ERANGE) {
					UDFInvalidArgumentMessage(context, "number is too large and overflowed");
					CVSetBoolean(ret, false);
				} else {
					if (tmp > 0xFFFFFFFFFFFFFFFF) {
						UDFInvalidArgumentMessage(context, "provided number is larger than 64-bits!");
						CVSetBoolean(ret, false);
					} else {
						CVSetInteger(ret, static_cast<CLIPSInteger>(tmp));
					}
				}
			} else {
				UDFInvalidArgumentMessage(context, "Hex must start with 0x");
				CVSetBoolean(ret, false);
			}
		}
	}

	void CLIPS_binaryNot(UDFContext* context, CLIPSValue* ret) {
		CLIPSValue number;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &number)) {
			CVSetBoolean(ret, false);
		} else {
			CVSetInteger(ret, binaryNot<CLIPSInteger>(CVToInteger(&number)));
		}
	}
	void CLIPS_binaryAnd(UDFContext* context, CLIPSValue* ret) {
		CLIPSValue a, b;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &a)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &b)) {
			CVSetBoolean(ret, false);
		} else {
			CVSetInteger(ret, binaryAnd<CLIPSInteger>(CVToInteger(&a), CVToInteger(&b)));
		}
	}
	void CLIPS_binaryOr(UDFContext* context, CLIPSValue* ret) {
		CLIPSValue a, b;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &a)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &b)) {
			CVSetBoolean(ret, false);
		} else {
			CVSetInteger(ret, binaryOr<CLIPSInteger>(CVToInteger(&a), CVToInteger(&b)));
		}
	}
	void CLIPS_binaryXor(UDFContext* context, CLIPSValue* ret) {
		CLIPSValue a, b;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &a)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &b)) {
			CVSetBoolean(ret, false);
		} else {
			CVSetInteger(ret, binaryXor<CLIPSInteger>(CVToInteger(&a), CVToInteger(&b)));
		}
	}
	void CLIPS_binaryNand(UDFContext* context, CLIPSValue* ret) {
		CLIPSValue a, b;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &a)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &b)) {
			CVSetBoolean(ret, false);
		} else {
			CVSetInteger(ret, binaryNand<CLIPSInteger>(CVToInteger(&a), CVToInteger(&b)));
		}
	}
	void CLIPS_expandBit(UDFContext* context, CLIPSValue* ret) {
		CLIPSValue a;
		if (!UDFFirstArgument(context, ANY_TYPE, &a)) {
			CVSetBoolean(ret, false);
		} else {
			CVSetInteger(ret, static_cast<CLIPSInteger>(expandBit(CVIsTrueSymbol(&a))));
		}
	}
	void CLIPS_decodeBits(UDFContext* context, CLIPSValue* ret) {
		CLIPSValue value, mask, shift;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &value)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &mask)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &shift)) {
			CVSetBoolean(ret, false);
		} else {
			CVSetInteger(ret, decodeBits<CLIPSInteger, CLIPSInteger>(CVToInteger(&value), CVToInteger(&mask), CVToInteger(&shift)));
		}
	}
	void CLIPS_encodeBits(UDFContext* context, CLIPSValue* ret) {
		CLIPSValue input, value, mask, shift;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &input)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &value)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &mask)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &shift)) {
			CVSetBoolean(ret, false);
		} else {
			CVSetInteger(ret, encodeBits<CLIPSInteger, CLIPSInteger>(CVToInteger(&input), CVToInteger(&value), CVToInteger(&mask), CVToInteger(&shift)));
		}
	}
	void CLIPS_shiftLeft(UDFContext* context, CLIPSValue* ret) {
		CLIPSValue input, shift;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &input)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &shift)) {
			CVSetBoolean(ret, false);
		} else {
			CVSetInteger(ret, shiftLeft<CLIPSInteger>(CVToInteger(&input), CVToInteger(&shift)));
		}
	}
	void CLIPS_shiftRight(UDFContext* context, CLIPSValue* ret) {
		CLIPSValue input, shift;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &input)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &shift)) {
			CVSetBoolean(ret, false);
		} else {
			CVSetInteger(ret, shiftRight<CLIPSInteger>(CVToInteger(&input), CVToInteger(&shift)));
		}
	}

	void CLIPS_basePrintAddress(void* env, const char* logicalName, void* theValue, const char* func, const char* majorType) {
		std::stringstream ss;
		void* ptr = EnvValueToExternalAddress(env, theValue);
		ss << "<" << majorType << "-" << func << "-" << std::hex << ((ptr) ? ptr : theValue) << ">";
		auto str = ss.str();
		EnvPrintRouter(env, logicalName, str.c_str());
	}
	inline void CLIPS_basePrintAddress_Pointer(void* env, const char* logicalName, void* theValue, const char* func) noexcept {
		CLIPS_basePrintAddress(env, logicalName, theValue, func, "Pointer");
	}

	bool errorMessage(void* env, const std::string& idClass, int idIndex, const std::string& msgPrefix, const std::string& msg) noexcept {
		PrintErrorID(env, idClass.c_str(), idIndex, false);
		EnvPrintRouter(env, WERROR, msgPrefix.c_str());
		EnvPrintRouter(env, WERROR, msg.c_str());
		EnvPrintRouter(env, WERROR, "\n");
		EnvSetEvaluationError(env, true);
		return false;
	}

	enum class ArithmeticOperations {
		Add,
		Sub,
		Mul,
		Div,
		Rem,
	};

	template<typename T, ArithmeticOperations op>
		void CLIPS_arithmeticOperation(UDFContext* context, CLIPSValue* ret) {
			CLIPSValue arg0, arg1;
			if (!UDFFirstArgument(context, INTEGER_TYPE, &arg0)) {
				CVSetBoolean(ret, false);
			} else if (!UDFNextArgument(context, INTEGER_TYPE, &arg1)) {
				CVSetBoolean(ret, false);
			} else {
				try {
					auto v0 = static_cast<T>(CVToInteger(&arg0));
					auto v1 = static_cast<T>(CVToInteger(&arg1));
					decltype(v0) result = 0;
					switch(op) {
						case ArithmeticOperations::Add:
							result = add<T>(v0, v1);
							break;
						case ArithmeticOperations::Sub:
							result = sub<T>(v0, v1);
							break;
						case ArithmeticOperations::Mul:
							result = mul<T>(v0, v1);
							break;
						case ArithmeticOperations::Div:
							result = div<T>(v0, v1);
							break;
						case ArithmeticOperations::Rem:
							result = rem<T>(v0, v1);
							break;
						default:
							throw iris::Problem("Unimplemented arithmetic operation!");
					}
					CVSetInteger(ret, static_cast<CLIPSInteger>(result));
				} catch(iris::Problem p) {
					auto env = UDFContextEnvironment(context);
					PrintErrorID(env, "CALL", 3, false);
					std::stringstream stm;
					stm << "Arithmetic Operation: " << p.what() << std::endl;
					auto msg = stm.str();
					EnvPrintRouter(env, WERROR, msg.c_str());
					EnvSetEvaluationError(env, true);
					CVSetBoolean(ret, false);
				}
			}
		}

#define X(type, id) \
	void CLIPS_Add_ ## id (UDFContext* context, CLIPSValue* ret) { CLIPS_arithmeticOperation< type , ArithmeticOperations::Add > (context, ret); } \
	void CLIPS_Sub_ ## id (UDFContext* context, CLIPSValue* ret) { CLIPS_arithmeticOperation< type , ArithmeticOperations::Sub > (context, ret); } \
	void CLIPS_Mul_ ## id (UDFContext* context, CLIPSValue* ret) { CLIPS_arithmeticOperation< type , ArithmeticOperations::Mul > (context, ret); } \
	void CLIPS_Div_ ## id (UDFContext* context, CLIPSValue* ret) { CLIPS_arithmeticOperation< type , ArithmeticOperations::Div > (context, ret); } \
	void CLIPS_Rem_ ## id (UDFContext* context, CLIPSValue* ret) { CLIPS_arithmeticOperation< type , ArithmeticOperations::Rem > (context, ret); }
	X(byte, word8u)
	X(int8_t, word8s)
	X(uint16, word16u)
	X(int16_t, word16s)
	X(uint32, word32u)
	X(int32_t, word32s)
	X(uint64, word64u)
	X(int64_t, word64s)
#undef X

		void installExtensions(void* theEnv) {
			Environment* env = static_cast<Environment*>(theEnv);

			EnvAddUDF(env, "bitmask->int", "l", CLIPS_translateBitmask, "CLIPS_translateBitmask", 1, 1, "sy", nullptr);
			EnvAddUDF(env, "binary->int", "l", CLIPS_translateBinary, "CLIPS_translateBinary", 1, 1, "sy", nullptr);
			EnvAddUDF(env, "hex->int", "l", CLIPS_translateHex, "CLIPS_translateHex", 1, 1, "sy", nullptr);
			EnvAddUDF(env, "binary-not", "l", CLIPS_binaryNot, "CLIPS_binaryNot", 1, 1, "l", nullptr);
			EnvAddUDF(env, "binary-and", "l", CLIPS_binaryAnd, "CLIPS_binaryAnd", 2, 2, "l;l", nullptr);
			EnvAddUDF(env, "binary-or", "l", CLIPS_binaryOr, "CLIPS_binaryOr", 2, 2, "l;l", nullptr);
			EnvAddUDF(env, "binary-xor", "l", CLIPS_binaryXor, "CLIPS_binaryXor", 2, 2, "l;l", nullptr);
			EnvAddUDF(env, "binary-nand", "l", CLIPS_binaryNand, "CLIPS_binaryNand", 2, 2, "l;l", nullptr);
			EnvAddUDF(env, "expand-bit", "l", CLIPS_expandBit, "CLIPS_expandBit", 1, 1,  nullptr, nullptr);
			EnvAddUDF(env, "decode-bits", "l", CLIPS_decodeBits, "CLIPS_decodeBits", 3, 3, "l;l;l", nullptr);
			EnvAddUDF(env, "encode-bits", "l", CLIPS_encodeBits, "CLIPS_encodeBits", 4, 4, "l;l;l;l", nullptr);
			EnvAddUDF(env, "left-shift", "l", CLIPS_shiftLeft, "CLIPS_shiftLeft", 2, 2, "l;l", nullptr);
			EnvAddUDF(env, "right-shift", "l", CLIPS_shiftRight, "CLIPS_shiftRight", 2, 2, "l;l", nullptr);

#define X(title, id, type) \
			ManagedMemoryBlock<type>::registerWithEnvironment(theEnv, #title); \
			EnvAddUDF(env, "add-" #title , "l", CLIPS_Add_ ## title , "CLIPS_Add_" #title , 2, 2, "l;l", nullptr); \
			EnvAddUDF(env, "sub-" #title , "l", CLIPS_Sub_ ## title , "CLIPS_Sub_" #title , 2, 2, "l;l", nullptr); \
			EnvAddUDF(env, "mul-" #title , "l", CLIPS_Mul_ ## title , "CLIPS_Mul_" #title , 2, 2, "l;l", nullptr); \
			EnvAddUDF(env, "div-" #title , "l", CLIPS_Div_ ## title , "CLIPS_Div_" #title , 2, 2, "l;l", nullptr); \
			EnvAddUDF(env, "rem-" #title , "l", CLIPS_Rem_ ## title , "CLIPS_Rem_" #title , 2, 2, "l;l", nullptr)
			X(word8u, Word8u, uint8_t);
			X(word16u, Word16u, uint16_t);
			X(word32u, Word32u, uint32_t);
			X(word64u, Word64u, uint64_t);
			X(word8s, Word8s, int8_t);
			X(word16s, Word16s, int16_t);
			X(word32s, Word32s, int32_t);
			X(word64s, Word64s, int64_t);
#undef X
		}
}
