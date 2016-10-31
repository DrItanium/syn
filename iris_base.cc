#include "iris_base.h"
#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/trim.hpp>
#include <cstdint>
#include <climits>
#include <sstream>
#include <memory>

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
	unsigned int ptrWord8u_externalAddressID = 0u;
	unsigned int ptrWord16u_externalAddressID = 0u;
	unsigned int ptrWord32u_externalAddressID = 0u;
	unsigned int ptrWord64u_externalAddressID = 0u;
	unsigned int getExternalAddressID(AddressIDs id) {
		switch(id) {
			case AddressIDs::Ptr_Word8u:
				return ptrWord8u_externalAddressID;
			case AddressIDs::Ptr_Word16u:
				return ptrWord16u_externalAddressID;
			case AddressIDs::Ptr_Word32u:
				return ptrWord32u_externalAddressID;
			case AddressIDs::Ptr_Word64u:
				return ptrWord64u_externalAddressID;
			default:
				throw iris::Problem("Attempted to retrieve an unimplemented address id!");
		}
	}
	template<typename T>
	AddressIDs getExternalAddressIdFromType() {
		throw iris::Problem("unspecified type!");
	}
	template<> AddressIDs getExternalAddressIdFromType<byte*>() { return AddressIDs::Ptr_Word8u; }
	template<> AddressIDs getExternalAddressIdFromType<uint16*>() { return AddressIDs::Ptr_Word16u; }
	template<> AddressIDs getExternalAddressIdFromType<uint32*>() { return AddressIDs::Ptr_Word32u; }
	template<> AddressIDs getExternalAddressIdFromType<uint64*>() { return AddressIDs::Ptr_Word64u; }
	template<AddressIDs id>
	std::string getNameFromExternalAddressId() {
		throw iris::Problem("unimplemented type!");
	}

	template<> std::string getNameFromExternalAddressId<AddressIDs::Ptr_Word8u>() { return "word8u-ptr"; }
	template<> std::string getNameFromExternalAddressId<AddressIDs::Ptr_Word16u>() { return "word16u-ptr"; }
	template<> std::string getNameFromExternalAddressId<AddressIDs::Ptr_Word32u>() { return "word32u-ptr"; }
	template<> std::string getNameFromExternalAddressId<AddressIDs::Ptr_Word64u>() { return "word64u-ptr"; }
	std::string getNameFromExternalAddressId(AddressIDs id) {
		switch(id) {
			case AddressIDs::Ptr_Word8u:
				return getNameFromExternalAddressId<AddressIDs::Ptr_Word8u>();
			case AddressIDs::Ptr_Word16u:
				return getNameFromExternalAddressId<AddressIDs::Ptr_Word16u>();
			case AddressIDs::Ptr_Word32u:
				return getNameFromExternalAddressId<AddressIDs::Ptr_Word32u>();
			case AddressIDs::Ptr_Word64u:
				return getNameFromExternalAddressId<AddressIDs::Ptr_Word64u>();
			default:
				throw iris::Problem("Unimplemented type!");
		}
	}

	void CLIPS_basePrintAddress(void* env, const char* logicalName, void* theValue, const char* func) {
		std::stringstream ss;
		void* ptr = EnvValueToExternalAddress(env, theValue);
		ss << "<Pointer-" << func << "-" << std::hex << ((ptr) ? ptr : theValue) << ">";
		auto str = ss.str();
		EnvPrintRouter(env, logicalName, str.c_str());
	}
	void CLIPS_printWord8uPtr(void* env, const char* logicalName, void* theValue) {
		CLIPS_basePrintAddress(env, logicalName, theValue, getNameFromExternalAddressId<AddressIDs::Ptr_Word8u>().c_str());
	}
	void CLIPS_printWord16uPtr(void* env, const char* logicalName, void* theValue) {
		CLIPS_basePrintAddress(env, logicalName, theValue, getNameFromExternalAddressId<AddressIDs::Ptr_Word16u>().c_str());
	}
	void CLIPS_printWord32uPtr(void* env, const char* logicalName, void* theValue) {
		CLIPS_basePrintAddress(env, logicalName, theValue, getNameFromExternalAddressId<AddressIDs::Ptr_Word32u>().c_str());
	}
	void CLIPS_printWord64uPtr(void* env, const char* logicalName, void* theValue) {
		CLIPS_basePrintAddress(env, logicalName, theValue, getNameFromExternalAddressId<AddressIDs::Ptr_Word64u>().c_str());
	}
	template<typename Word>
	using WordMemoryBlock = std::tuple<std::unique_ptr<Word[]>, CLIPSInteger>;
	template<typename Word>
		void CLIPS_newPtr(void* env, DATA_OBJECT* ret) {
			try {
				auto id = getExternalAddressIdFromType<Word*>();
				std::stringstream ss;
				ss << "new (" << getNameFromExternalAddressId(id) << ")";
				auto funcStr = ss.str();
				if (EnvRtnArgCount(env) == 2) {
					CLIPSValue capacity;
					if (EnvArgTypeCheck(env, funcStr.c_str(), 2, INTEGER, &capacity) == FALSE) {
						PrintErrorID(env, "NEW", 1, false);
						EnvPrintRouter(env, WERROR, "Function ");
						EnvPrintRouter(env, WERROR, funcStr.c_str());
						EnvPrintRouter(env, WERROR, " expected an integer for capacity\n");
						EnvSetEvaluationError(env, true);
						CVSetBoolean(ret, false);
					} else {
						auto size = EnvDOToLong(env, capacity);
						auto idIndex = getExternalAddressID(id);
						ret->bitType = EXTERNAL_ADDRESS_TYPE;
						SetpType(ret, EXTERNAL_ADDRESS);
						auto ptr = new WordMemoryBlock<Word>(std::make_unique<Word[]>(size), size);
						SetpValue(ret, EnvAddExternalAddress(env, ptr, idIndex));
					}
				} else {
					PrintErrorID(env, "NEW", 1, false);
					EnvPrintRouter(env, WERROR, "Function new expected no arguments besides type!\n");
					EnvSetEvaluationError(env, true);
					CVSetBoolean(ret, false);
				}
			} catch(iris::Problem p) {
				PrintErrorID(env, "NEW", 2, false);
				std::stringstream s;
				s << "Function new threw an exception: " << p.what();
				auto str = s.str();
				EnvPrintRouter(env, WERROR, str.c_str());
				EnvSetEvaluationError(env, true);
				CVSetBoolean(ret, false);
			}
		}
	template<typename Word>
	bool CLIPS_callPtr(void* env, DATA_OBJECT* value, DATA_OBJECT* ret) {
		auto inRange = [](CLIPSInteger capacity, CLIPSInteger address) { return address >= 0 && address < capacity; };
		if (GetpType(value) == EXTERNAL_ADDRESS) {
			auto id = getExternalAddressIdFromType<Word*>();
			std::stringstream ss;
			ss << "call (" << getNameFromExternalAddressId(id) << ")";
			auto funcStr = ss.str();
			auto argCheck = [env, &funcStr](CLIPSValue* storage, int position, int type) { return EnvArgTypeCheck(env, funcStr.c_str(), position, type, storage); };
			auto callErrorMessage = [env, &funcStr, ret](const std::string& subOp, const std::string& rest) {
				PrintErrorID(env, "CALL", 3, false);
				std::stringstream stm;
				stm << "Function " << funcStr.c_str() << " " << subOp << ": " << rest << std::endl;
				auto msg = stm.str();
				EnvPrintRouter(env, WERROR, msg.c_str());
				EnvSetEvaluationError(env, true);
				CVSetBoolean(ret, false);
				return false;
			};
			auto errOutOfRange = [callErrorMessage, env, ret, &funcStr](const std::string& subOp, CLIPSInteger capacity, CLIPSInteger address) {
				std::stringstream ss;
				ss << "Function " << funcStr << ": Provided address " << std::hex << address << " is either less than zero or greater than " << std::hex << capacity << std::endl;
				return callErrorMessage(subOp, ss.str());
			};
			CLIPSValue operation;
			if (EnvArgTypeCheck(env, funcStr.c_str(), 2, SYMBOL, &operation) == FALSE) {
				PrintErrorID(env, "CALL", 2, false);
				EnvPrintRouter(env, WERROR, "Function ");
				EnvPrintRouter(env, WERROR, funcStr.c_str());
				EnvPrintRouter(env, WERROR, " expected a function name to call!\n");
				EnvSetEvaluationError(env, true);
				return false;
			} else {
				// now figure out what method we are looking at
				auto tup = static_cast<WordMemoryBlock<Word>*>(DOPToExternalAddress(value));
				auto ptr = std::get<0>(*tup).get();
				auto size = std::get<1>(*tup);
				std::string str(EnvDOToString(env, operation));
				CVSetBoolean(ret, true);
				if (str == "clear") {
					for(CLIPSInteger i = 0; i < size; ++i) {
						ptr[i] = static_cast<Word>(0);
					}
					CVSetBoolean(ret, true);
				} else if (str == "size") {
					CVSetInteger(ret, size);
				} else if (str == "get") {
					CLIPSValue address;
					if (!argCheck(&address, 3, INTEGER)) {
						return callErrorMessage("get", "Requires an address!");
					} else {
						auto addr = EnvDOToLong(env, address);
						if (!inRange(size, addr)) {
							errOutOfRange("get", size, addr);
						} else {
							CVSetInteger(ret, static_cast<CLIPSInteger>(ptr[EnvDOToLong(env, address)]));
						}
					}
				} else if (str == "set") {
					CLIPSValue address, value;
					if (!argCheck(&address, 3, INTEGER)) {
						return callErrorMessage("set", "First argument must be address!");
					} else if (!argCheck(&value, 4, INTEGER)) {
						return callErrorMessage("set", "Second argument must be an integer!");
					} else {
						auto addr = EnvDOToLong(env, address);
						if (!inRange(size, addr)) {
							errOutOfRange("set", size, addr);
						} else {
							auto num = EnvDOToLong(env, value);
							ptr[addr] = static_cast<Word>(num);
							CVSetBoolean(ret, true);
						}
					}
				} else if (str == "swap") {
					CLIPSValue address0, address1;
					if (!argCheck(&address0, 3, INTEGER)) {
						return callErrorMessage("swap", "First argument must be an address");
					} else if (!argCheck(&address1, 4, INTEGER)) {
						return callErrorMessage("swap", "Second argument must be an address");
					} else {
						auto addr0 = EnvDOToLong(env, address0);
						auto addr1 = EnvDOToLong(env, address1);
						if (!inRange(size, addr0)) {
							errOutOfRange("swap", size, addr0);
						} else if(!inRange(size, addr1)) {
							errOutOfRange("swap", size, addr1);
						} else {
							swap<Word>(ptr[addr0], ptr[addr1]);
							CVSetBoolean(ret, true);
						}
					}
				} else {
					return callErrorMessage(str, "<- unknown operation requested!");
				}
			}
			return true;
		} else {
			PrintErrorID(env, "CALL", 1, false);
			EnvPrintRouter(env, WERROR, "Function call expected an external address as the first argument!\n");
			EnvSetEvaluationError(env, true);
			return false;
		}
	}

	template<typename Word>
		bool CLIPS_deletePtr(void* env, void* obj) {
			if (obj != nullptr) {
				auto result = static_cast<WordMemoryBlock<Word>*>(obj);
				delete result;
			}
			return true;
		}
	void CLIPS_newWord8uPtr(void* env, DATA_OBJECT* ret) { CLIPS_newPtr<byte>(env, ret); }
	void CLIPS_newWord16uPtr(void* env, DATA_OBJECT* ret) { CLIPS_newPtr<uint16>(env, ret); }
	void CLIPS_newWord32uPtr(void* env, DATA_OBJECT* ret) { CLIPS_newPtr<uint32>(env, ret); }
	void CLIPS_newWord64uPtr(void* env, DATA_OBJECT* ret) { CLIPS_newPtr<uint64>(env, ret); }
	bool CLIPS_deleteWord8uPtr(void* env, void* ret) { return CLIPS_deletePtr<byte>(env, ret); }
	bool CLIPS_deleteWord16uPtr(void* env, void* ret) { return CLIPS_deletePtr<uint16>(env, ret); }
	bool CLIPS_deleteWord32uPtr(void* env, void* ret) { return CLIPS_deletePtr<uint32>(env, ret); }
	bool CLIPS_deleteWord64uPtr(void* env, void* ret) { return CLIPS_deletePtr<uint64>(env, ret); }
	bool CLIPS_callWord8uPtr(void* env,  DATA_OBJECT* theValue, DATA_OBJECT* ret) { return CLIPS_callPtr<byte>(env, theValue, ret); }
	bool CLIPS_callWord16uPtr(void* env, DATA_OBJECT* theValue, DATA_OBJECT* ret) { return CLIPS_callPtr<uint16>(env, theValue, ret); }
	bool CLIPS_callWord32uPtr(void* env, DATA_OBJECT* theValue, DATA_OBJECT* ret) { return CLIPS_callPtr<uint32>(env, theValue, ret); }
	bool CLIPS_callWord64uPtr(void* env, DATA_OBJECT* theValue, DATA_OBJECT* ret) { return CLIPS_callPtr<uint64>(env, theValue, ret); }
	enum class ArithmeticOperations {
		Add,
		Sub,
		Mul,
		Div,
		Rem,
	};

	template<typename T, ArithmeticOperations op>
	void CLIPS_arithmeticOperation(UDFContext* context, CLIPSValue* ret) {
		auto env = UDFContextEnvironment(context);
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

	template<typename T>
	void CLIPS_Add(UDFContext* context, CLIPSValue* ret) {
		CLIPS_arithmeticOperation<T, ArithmeticOperations::Add>(context, ret);
	}
	template<typename T>
	void CLIPS_Sub(UDFContext* context, CLIPSValue* ret) {
		CLIPS_arithmeticOperation<T, ArithmeticOperations::Sub>(context, ret);
	}
	template<typename T>
	void CLIPS_Mul(UDFContext* context, CLIPSValue* ret) {
		CLIPS_arithmeticOperation<T, ArithmeticOperations::Mul>(context, ret);
	}
	template<typename T>
	void CLIPS_Div(UDFContext* context, CLIPSValue* ret) {
		CLIPS_arithmeticOperation<T, ArithmeticOperations::Div>(context, ret);
	}
	template<typename T>
	void CLIPS_Rem(UDFContext* context, CLIPSValue* ret) {
		CLIPS_arithmeticOperation<T, ArithmeticOperations::Rem>(context, ret);
	}

#define X(type, id) \
	void CLIPS_Add_ ## id (UDFContext* context, CLIPSValue* ret) { CLIPS_Add<type > (context, ret); } \
	void CLIPS_Sub_ ## id (UDFContext* context, CLIPSValue* ret) { CLIPS_Sub<type > (context, ret); } \
	void CLIPS_Mul_ ## id (UDFContext* context, CLIPSValue* ret) { CLIPS_Mul<type > (context, ret); } \
	void CLIPS_Div_ ## id (UDFContext* context, CLIPSValue* ret) { CLIPS_Div<type > (context, ret); } \
	void CLIPS_Rem_ ## id (UDFContext* context, CLIPSValue* ret) { CLIPS_Rem<type > (context, ret); }
X(byte, word8u)
X(uint16, word16u)
X(uint32, word32u)
X(uint64, word64u)
X(CLIPSInteger, integer)
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
#define X(id) \
		EnvAddUDF(env, "add-" #id , "l", CLIPS_Add_ ## id , "CLIPS_Add_" #id , 2, 2, "l;l", nullptr); \
		EnvAddUDF(env, "sub-" #id , "l", CLIPS_Sub_ ## id , "CLIPS_Sub_" #id , 2, 2, "l;l", nullptr); \
		EnvAddUDF(env, "mul-" #id , "l", CLIPS_Mul_ ## id , "CLIPS_Mul_" #id , 2, 2, "l;l", nullptr); \
		EnvAddUDF(env, "div-" #id , "l", CLIPS_Div_ ## id , "CLIPS_Div_" #id , 2, 2, "l;l", nullptr); \
		EnvAddUDF(env, "rem-" #id , "l", CLIPS_Rem_ ## id , "CLIPS_Rem_" #id , 2, 2, "l;l", nullptr)
X(word8u);
X(word16u);
X(word32u);
X(word64u);
X(integer);
#undef X

		externalAddressType word8u = {
			"word8u",
			CLIPS_printWord8uPtr,
			CLIPS_printWord8uPtr,
			CLIPS_deleteWord8uPtr,
			CLIPS_newWord8uPtr,
			CLIPS_callWord8uPtr,
		};
		ptrWord8u_externalAddressID = InstallExternalAddressType(theEnv, &word8u);
		externalAddressType word16u = {
			"word16u",
			CLIPS_printWord16uPtr,
			CLIPS_printWord16uPtr,
			CLIPS_deleteWord16uPtr,
			CLIPS_newWord16uPtr,
			CLIPS_callWord16uPtr,
		};
		ptrWord16u_externalAddressID = InstallExternalAddressType(theEnv, &word16u);
		externalAddressType word32u = {
			"word32u",
			CLIPS_printWord32uPtr,
			CLIPS_printWord32uPtr,
			CLIPS_deleteWord32uPtr,
			CLIPS_newWord32uPtr,
			CLIPS_callWord32uPtr,
		};
		ptrWord32u_externalAddressID = InstallExternalAddressType(theEnv, &word32u);
		externalAddressType word64u = {
			"word64u",
			CLIPS_printWord64uPtr,
			CLIPS_printWord64uPtr,
			CLIPS_deleteWord64uPtr,
			CLIPS_newWord64uPtr,
			CLIPS_callWord64uPtr,
		};
		ptrWord64u_externalAddressID = InstallExternalAddressType(theEnv, &word64u);

	}
}
