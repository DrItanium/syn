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
	unsigned int sharedPtrWord8u_externalAddressID = 0u;
	unsigned int sharedPtrWord16u_externalAddressID = 0u;
	unsigned int sharedPtrWord32u_externalAddressID = 0u;
	unsigned int sharedPtrWord64u_externalAddressID = 0u;
	unsigned int getExternalAddressID(AddressIDs id) {
		switch(id) {
			case AddressIDs::SharedPtr_Word8u:
				return sharedPtrWord8u_externalAddressID;
			case AddressIDs::SharedPtr_Word16u:
				return sharedPtrWord16u_externalAddressID;
			case AddressIDs::SharedPtr_Word32u:
				return sharedPtrWord32u_externalAddressID;
			case AddressIDs::SharedPtr_Word64u:
				return sharedPtrWord64u_externalAddressID;
			default:
				throw iris::Problem("Attempted to retrieve an unimplemented address id!");
		}
	}
	template<typename T>
	AddressIDs getExternalAddressIdFromType() {
		throw iris::Problem("unspecified type!");
	}
	template<> AddressIDs getExternalAddressIdFromType<std::shared_ptr<byte>>() { return AddressIDs::SharedPtr_Word8u; }
	template<> AddressIDs getExternalAddressIdFromType<std::shared_ptr<uint16>>() { return AddressIDs::SharedPtr_Word16u; }
	template<> AddressIDs getExternalAddressIdFromType<std::shared_ptr<uint32>>() { return AddressIDs::SharedPtr_Word32u; }
	template<> AddressIDs getExternalAddressIdFromType<std::shared_ptr<uint64>>() { return AddressIDs::SharedPtr_Word64u; }
	template<AddressIDs id>
	std::string getNameFromExternalAddressId() {
		throw iris::Problem("unimplemented type!");
	}

	template<> std::string getNameFromExternalAddressId<AddressIDs::SharedPtr_Word8u>() { return "word8u-shared-ptr"; }
	template<> std::string getNameFromExternalAddressId<AddressIDs::SharedPtr_Word16u>() { return "word16u-shared-ptr"; }
	template<> std::string getNameFromExternalAddressId<AddressIDs::SharedPtr_Word32u>() { return "word32u-shared-ptr"; }
	template<> std::string getNameFromExternalAddressId<AddressIDs::SharedPtr_Word64u>() { return "word64u-shared-ptr"; }
	std::string getNameFromExternalAddressId(AddressIDs id) {
		switch(id) {
			case AddressIDs::SharedPtr_Word8u:
				return getNameFromExternalAddressId<AddressIDs::SharedPtr_Word8u>();
			case AddressIDs::SharedPtr_Word16u:
				return getNameFromExternalAddressId<AddressIDs::SharedPtr_Word16u>();
			case AddressIDs::SharedPtr_Word32u:
				return getNameFromExternalAddressId<AddressIDs::SharedPtr_Word32u>();
			case AddressIDs::SharedPtr_Word64u:
				return getNameFromExternalAddressId<AddressIDs::SharedPtr_Word64u>();
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
	void CLIPS_printWord8uSharedPtr(void* env, const char* logicalName, void* theValue) {
		CLIPS_basePrintAddress(env, logicalName, theValue, getNameFromExternalAddressId<AddressIDs::SharedPtr_Word8u>().c_str());
	}
	void CLIPS_printWord16uSharedPtr(void* env, const char* logicalName, void* theValue) {
		CLIPS_basePrintAddress(env, logicalName, theValue, getNameFromExternalAddressId<AddressIDs::SharedPtr_Word16u>().c_str());
	}
	void CLIPS_printWord32uSharedPtr(void* env, const char* logicalName, void* theValue) {
		CLIPS_basePrintAddress(env, logicalName, theValue, getNameFromExternalAddressId<AddressIDs::SharedPtr_Word32u>().c_str());
	}
	void CLIPS_printWord64uSharedPtr(void* env, const char* logicalName, void* theValue) {
		CLIPS_basePrintAddress(env, logicalName, theValue, getNameFromExternalAddressId<AddressIDs::SharedPtr_Word64u>().c_str());
	}
	
	template<typename Word>
		void CLIPS_newSharedPtr(void* env, DATA_OBJECT* ret) {
			try {
				auto id = getExternalAddressIdFromType<std::shared_ptr<Word>>();
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
						auto size = EnvDOToInteger(env, capacity);
						auto idIndex = getExternalAddressID(id);
						ret->bitType = EXTERNAL_ADDRESS_TYPE;
						SetpType(ret, EXTERNAL_ADDRESS);
						auto ptr = std::make_shared<Word>(size);
						SetpValue(ret, EnvAddExternalAddress(env, &ptr, idIndex));
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
	void CLIPS_newWord8uSharedPtr(void* env, DATA_OBJECT* ret) { CLIPS_newSharedPtr<byte>(env, ret); }
	void CLIPS_newWord16uSharedPtr(void* env, DATA_OBJECT* ret) { CLIPS_newSharedPtr<uint16>(env, ret); }
	void CLIPS_newWord32uSharedPtr(void* env, DATA_OBJECT* ret) { CLIPS_newSharedPtr<uint32>(env, ret); }
	void CLIPS_newWord64uSharedPtr(void* env, DATA_OBJECT* ret) { CLIPS_newSharedPtr<uint64>(env, ret); }
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
	}
}
