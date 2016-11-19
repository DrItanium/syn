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
#include "iris_sfml_network.h"

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

	inline bool errorMessage(void* env, const std::string& idClass, int idIndex, const std::string& msgPrefix, const std::string& msg) noexcept {
		PrintErrorID(env, idClass.c_str(), idIndex, false);
		EnvPrintRouter(env, WERROR, msgPrefix.c_str());
		EnvPrintRouter(env, WERROR, msg.c_str());
		EnvPrintRouter(env, WERROR, "\n");
		EnvSetEvaluationError(env, true);
		return false;
	}



	template<typename Word>
		void CLIPS_newPtr(void* env, DATA_OBJECT* ret) {
			static bool init = false;
			static std::string funcStr;
			static std::string funcErrorPrefix;
			static std::string type;
			if (init) {
				init = false;
				type = TypeToName<typename ManagedMemoryBlock<Word>::InternalType>::getSymbolicName();
				std::stringstream ss, ss2;
				ss << "call (" << type << " memory block)";
				funcStr = ss.str();
				ss2 << "Function " << funcStr;
				funcErrorPrefix = ss2.str();
			}

			try {
				if (EnvRtnArgCount(env) == 2) {
					CLIPSValue capacity;
					if (EnvArgTypeCheck(env, funcStr.c_str(), 2, INTEGER, &capacity) == FALSE) {
						CVSetBoolean(ret, false);
						errorMessage(env, "NEW", 1, funcErrorPrefix, " expected an integer for capacity!");
					} else {
						auto size = EnvDOToLong(env, capacity);
						auto idIndex = ExternalAddressRegistrar<typename ManagedMemoryBlock<Word>::InternalType>::getExternalAddressId(env);
						ret->bitType = EXTERNAL_ADDRESS_TYPE;
						SetpType(ret, EXTERNAL_ADDRESS);
						SetpValue(ret, EnvAddExternalAddress(env, ManagedMemoryBlock<Word>::make(size), idIndex));
					}
				} else {
					errorMessage(env, "NEW", 1, funcErrorPrefix, " function new expected no arguments besides type!");
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

	template<typename Word>
		bool CLIPS_callPtr(void* env, DATA_OBJECT* value, DATA_OBJECT* ret) {
			static bool init = true;
			static std::string funcStr;
			static std::string funcErrorPrefix;
			static std::string type;
			if (init) {
				init = false;
				type = TypeToName<typename ManagedMemoryBlock<Word>::InternalType>::getSymbolicName();
				std::stringstream ss, ss2;
				ss << "call (" << type << " memory block)";
				funcStr = ss.str();
				ss2 << "Function " << funcStr;
				funcErrorPrefix = ss2.str();
			}
			if (GetpType(value) == EXTERNAL_ADDRESS) {
				auto ptr = static_cast<ManagedMemoryBlock_Ptr<Word>>(DOPToExternalAddress(value));
#define argCheck(storage, position, type) EnvArgTypeCheck(env, funcStr.c_str(), position, type, storage)
				auto callErrorMessage = [env, ret](const std::string& subOp, const std::string& rest) {
					CVSetBoolean(ret, false);
					std::stringstream stm;
					stm << " " << subOp << ": " << rest << std::endl;
					auto msg = stm.str();
					return errorMessage(env, "CALL", 3, funcErrorPrefix, msg);
				};
				auto errOutOfRange = [callErrorMessage, env, ret](const std::string& subOp, CLIPSInteger capacity, CLIPSInteger address) {
					std::stringstream ss;
					ss << funcErrorPrefix << ": Provided address " << std::hex << address << " is either less than zero or greater than " << std::hex << capacity << std::endl;
					return callErrorMessage(subOp, ss.str());
				};
				CLIPSValue operation;
				if (EnvArgTypeCheck(env, funcStr.c_str(), 2, SYMBOL, &operation) == FALSE) {
					return errorMessage(env, "CALL", 2, funcErrorPrefix, "expected a function name to call!");
				} else {
					std::string str(EnvDOToString(env, operation));
					CVSetBoolean(ret, true);
					auto argc = EnvRtnArgCount(env);
					if (argc == 2) {
						if (str == "clear") {
							ptr->setMemoryToSingleValue(static_cast<Word>(0));
						} else if (str == "type") {
							// get the type of the current thing!
							CVSetSymbol(ret, type.c_str());
						} else if (str == "size") {
							CVSetInteger(ret, ptr->size());
						} else {
							return callErrorMessage(str, "<- unknown operation requested!");
						}
					} else if (argc == 3) {
						CLIPSValue arg0;
						auto oneCheck = [callErrorMessage, &arg0, env, &str](unsigned int type, const std::string& msg) {
							if (!argCheck(&arg0, 3, type)) {
								return callErrorMessage(str, msg);
							} else {
								return true;
							}
						};
						if (str == "get") {
							if (oneCheck(INTEGER, "Argument 0 must be an address!")) {
								auto addr = EnvDOToLong(env, arg0);
								if (!ptr->legalAddress(addr)) {
									errOutOfRange("get", ptr->size(), addr);
								} else {
									CVSetInteger(ret, static_cast<CLIPSInteger>(ptr->getMemoryCellValue(addr)));
								}
							} else {
								return false;
							}
						} else if (str == "populate") {
							if (oneCheck(INTEGER, "First argument must be a value to populate all of the memory cells with!")) {
								ptr->setMemoryToSingleValue(static_cast<Word>(EnvDOToLong(env, arg0)));
							} else {
								return false;
							}
						} else if (str == "increment") {
							if (oneCheck(INTEGER, "First argument must an address")) {
								auto addr = EnvDOToLong(env, arg0);
								if (!ptr->legalAddress(addr)) {
									errOutOfRange("increment", ptr->size(), addr);
								} else {
									ptr->decrementMemoryCell(addr);
								}
							} else {
								return false;
							}
						} else if (str == "decrement") {
							if (oneCheck(INTEGER, "First argument must be an address")) {
								auto addr = EnvDOToLong(env, arg0);
								if (!ptr->legalAddress(addr)) {
									errOutOfRange("decrement", ptr->size(), addr);
								} else {
									ptr->incrementMemoryCell(addr);
								}
							} else {
								return false;
							}
						} else {
							return callErrorMessage(str, "<- unknown operation requested!");
						}
					} else if (argc == 4) {
						CLIPSValue arg0, arg1;
						auto twoCheck = [callErrorMessage, &arg0, &arg1, env, &str](unsigned int type0, const std::string& msg0, unsigned int type1, const std::string& msg1) {
							if (!argCheck(&arg0, 3, type0)) {
								return callErrorMessage(str, msg0);
							} else if (!argCheck(&arg1, 4, type1)) {
								return callErrorMessage(str, msg1);
							} else {
								return true;
							}
						};
						if (str == "set") {
							if (twoCheck(INTEGER, "First argument must be address!", INTEGER, "Second argument must be an integer!")) {
								auto addr = EnvDOToLong(env, arg0);
								auto value = static_cast<Word>(EnvDOToLong(env, arg1));
								if (!ptr->legalAddress(addr)) {
									errOutOfRange("set", ptr->size(), addr);
								} else {
									ptr->setMemoryCell(addr, value);
								}
							} else {
								return false;
							}
						} else if (str == "swap") {
							if (twoCheck(INTEGER, "First argument must be an address", INTEGER, "Second argument must be an address")) {
								auto addr0 = EnvDOToLong(env, arg0);
								auto addr1 = EnvDOToLong(env, arg1);
								if (!ptr->legalAddress(addr0)) {
									errOutOfRange("swap", ptr->size(), addr0);
								} else if (!ptr->legalAddress(addr1)) {
									errOutOfRange("swap", ptr->size(), addr1);
								} else {
									ptr->swapMemoryCells(addr0, addr1);
								}
							} else {
								return false;
							}
						} else if (str == "move") {
							// move the contents of one address to another in the same
							// memory space
							if (twoCheck(INTEGER, "First argument must be a source address", INTEGER, "Second argument must be a destination address")) {
								auto srcAddr = EnvDOToLong(env, arg0);
								auto destAddr = EnvDOToLong(env, arg1);
								if (!ptr->legalAddress(srcAddr)) {
									errOutOfRange("move", ptr->size(), srcAddr);
								} else if (!ptr->legalAddress(destAddr)) {
									errOutOfRange("move", ptr->size(), destAddr);
								} else {
									ptr->copyMemoryCell(srcAddr, destAddr);
								}
							} else {
								return false;
							}
						} else {
							return callErrorMessage(str, "<- unknown operation requested!");
						}
					} else {
						return callErrorMessage(str, "<- unknown operation requested!");
					}
				}
				return true;
			} else {
				return errorMessage(env, "CALL", 1, funcErrorPrefix, "Function call expected an external address as the first argument!");
			}
#undef argCheck
		}

	template<typename Word>
		bool CLIPS_deletePtr(void* env, void* obj) {
			if (obj != nullptr) {
				auto result = static_cast<ManagedMemoryBlock<Word>*>(obj);
				delete result;
			}
			return true;
		}
#define defFunctions(id, type) \
	void CLIPS_new ## id ## Ptr (void* env, DATA_OBJECT* ret) { CLIPS_newPtr< type > (env, ret); } \
	bool CLIPS_delete ## id ## Ptr (void* env, void* ret) { return CLIPS_deletePtr< type > (env, ret); } \
	bool CLIPS_call ## id ## Ptr (void* env, DATA_OBJECT* theValue, DATA_OBJECT* ret) { return CLIPS_callPtr< type >(env, theValue, ret); } \
	void CLIPS_print ## id ## Ptr (void* env, const char* logicalName, void* theValue) { \
		static bool init = true; \
		static std::string funcName; \
		if (init) { \
			init = false; \
			funcName = TypeToName<typename ManagedMemoryBlock< type >::InternalType>::getSymbolicName(); \
		} \
		CLIPS_basePrintAddress_Pointer(env, logicalName, theValue, funcName.c_str()); \
	}
	defFunctions(Word8u, uint8_t);
	defFunctions(Word16u, uint16_t);
	defFunctions(Word32u, uint32_t);
	defFunctions(Word64u, uint64_t);
	defFunctions(Word8s, int8_t);
	defFunctions(Word16s, int16_t);
	defFunctions(Word32s, int32_t);
	defFunctions(Word64s, int64_t);
#undef defFunctions


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
			externalAddressType title = { \
				#title , \
				CLIPS_print ## id ## Ptr, \
				CLIPS_print ## id ## Ptr, \
				CLIPS_delete ## id ## Ptr, \
				CLIPS_new ## id ## Ptr, \
				CLIPS_call ## id ## Ptr, \
			}; \
			ExternalAddressRegistrar< ManagedMemoryBlock< type >::InternalType > ::registerExternalAddressId(theEnv, InstallExternalAddressType(theEnv, & title )); \
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
			installSfmlNetworkExtensions(theEnv);
		}
}
