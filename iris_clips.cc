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
#include <SFML/Graphics.hpp>

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
	DefWrapperSymbolicName(sf::RenderWindow, window);
	class WindowWrapper : public ExternalAddressWrapper<sf::RenderWindow> {
		public:
			static void newFunction(void* env, DATA_OBJECT* ret);
			static bool callFunction(void* env, DATA_OBJECT* value, DATA_OBJECT* ret);
			static void registerWithEnvironment(void* env) { BaseClass::registerWithEnvironment(env, "window", newFunction, callFunction); }
		public:
			WindowWrapper(unsigned int width, unsigned int height, unsigned int depth, const std::string& title) :
				ExternalAddressWrapper<InternalType>(std::move(std::make_unique<InternalType>(sf::VideoMode(width, height, depth), title))),
							_mode(width, height, depth),
							_title(title) { }
			~WindowWrapper() { }
			std::string getTitle() const noexcept { return _title; }
			sf::VideoMode getVideoMode() noexcept { return _mode; }
			unsigned int getWidth() noexcept { return _mode.width; }
			unsigned int getHeight() noexcept { return _mode.height; }
			unsigned int bitsPerPixel() noexcept { return _mode.bitsPerPixel; }
			bool legalMode() const noexcept { return _mode.isValid(); }
		private:
			sf::VideoMode _mode;
			std::string _title;
			static std::string _type;
	};
	std::string WindowWrapper::_type = WindowWrapper::getType();

	void WindowWrapper::newFunction(void* env, DATA_OBJECT* ret) {
		static bool init = false;
		static std::string funcStr;
		static std::string funcErrorPrefix;
		if (init) {
			init = false;
			std::stringstream ss, ss2;
			ss << "new (" << _type << ")";
			funcStr = ss.str();
			ss2 << "Function " << funcStr;
			funcErrorPrefix = ss2.str();
		}
		try {
			auto count = EnvRtnArgCount(env);
			if (count == 4 || count == 5) {
				CLIPSValue _height, _width, _title;
				auto bitDepth = 32u;
				if (!EnvArgTypeCheck(env, funcStr.c_str(), 2, INTEGER, &_width)) {
					CVSetBoolean(ret, false);
					errorMessage(env, "NEW", 1, funcErrorPrefix, " expected an unsigned int for width");
					return;
				} else if (!EnvArgTypeCheck(env, funcStr.c_str(), 3, INTEGER, &_height)) {
					CVSetBoolean(ret, false);
					errorMessage(env, "NEW", 1, funcErrorPrefix, " expected an unsigned int for height");
					return;
				} else if (!EnvArgTypeCheck(env, funcStr.c_str(), 4, SYMBOL_OR_STRING, &_title)) {
					CVSetBoolean(ret, false);
					errorMessage(env, "NEW", 1, funcErrorPrefix, " expected an lexme for the title of the window");
					return;
				} else { 
					if (count == 5) {
						CLIPSValue _depth;
						if (!EnvArgTypeCheck(env, funcStr.c_str(), 5, INTEGER, &_depth)) {
							CVSetBoolean(ret, false);
							errorMessage(env, "NEW", 1, funcErrorPrefix, " expected an integer for the bit depth!");
							return;
						} else {
							bitDepth = static_cast<unsigned int>(EnvDOToLong(env, _depth));
						}
					}
				}
				auto height = static_cast<unsigned int>(EnvDOToLong(env, _height));
				auto width = static_cast<unsigned int>(EnvDOToLong(env, _width));
				std::string title(EnvDOToString(env, _title));
				ret->bitType = EXTERNAL_ADDRESS_TYPE;
				SetpType(ret, EXTERNAL_ADDRESS);
				SetpValue(ret, EnvAddExternalAddress(env, new WindowWrapper(height, width, bitDepth, title), WindowWrapper::getAssociatedEnvironmentId(env)));
			} else {
				errorMessage(env, "NEW", 1, funcErrorPrefix, " function new expected three or four arguments!");
				CVSetBoolean(ret, false);
			}
		} catch(iris::Problem p) {
			CVSetBoolean(ret, false);
			std::stringstream s;
			s << "an exception was thrown: " << p.what();
			auto str = s.str();
			errorMessage(env, "NEW", 2, funcErrorPrefix, str);
		}
	}
	bool WindowWrapper::callFunction(void* env, DATA_OBJECT* value, DATA_OBJECT* ret) {
		static bool init = true;
		static std::string funcStr;
		static std::string funcErrorPrefix;
		if (init) {
			init = false;
			std::stringstream ss, ss2;
			ss << "call (" << _type << ")";
			funcStr = ss.str();
			ss2 << "Function " << funcStr;
			funcErrorPrefix = ss2.str();
		}
		if (GetpType(value) == EXTERNAL_ADDRESS) {
			auto window = static_cast<WindowWrapper*>(DOPToExternalAddress(value));
			CLIPSValue operation;
			if (!EnvArgTypeCheck(env, funcStr.c_str(), 2, SYMBOL, &operation)) {
				return errorMessage(env, "CALL", 2, funcErrorPrefix, "expected a symbol to call!");
			} else {
				CVSetBoolean(ret, true);
				std::string op(EnvDOToString(env, operation));
				if (op == "width") {
					CVSetInteger(ret, window->getWidth());
				} else if (op == "height") {
					CVSetInteger(ret, window->getHeight());
				} else if (op == "bit-depth" || op == "depth") {
					CVSetInteger(ret, window->bitsPerPixel());
				} else if (op == "title") {
					CVSetString(ret, window->getTitle().c_str());
				} else if (op == "validp") {
					CVSetBoolean(ret, window->legalMode());
				} else if (op == "openp") {
					CVSetBoolean(ret, window->get()->isOpen());
				} else if (op == "close") {
					window->get()->close();
				} else if (op == "has-focusp") {
					CVSetBoolean(ret, window->get()->hasFocus());
				} else if (op == "request-focus") {
					window->get()->requestFocus();
				} else if (op == "display") {
					window->get()->display();
				} else if (op == "clear") {
					// right now just clear it with a black background
					// TODO: add support for different colors
					window->get()->clear();
				} else {
					std::stringstream ss;
					ss << "unknown operation " << op;
					std::string str = ss.str();
					return errorMessage(env, "CALL", 3, funcErrorPrefix, str);
				}
			}
			return true;
		} else {
			return errorMessage(env, "CALL", 1, funcErrorPrefix, "Function call expected an external address as the first argument!");
		}
	}
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
			WindowWrapper::registerWithEnvironment(theEnv);
		}
}
