#include "iris_base.h"
#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/trim.hpp>
#include <cstdint>
#include <climits>

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

	void installExtensions(void* theEnv) {
		Environment* env = static_cast<Environment*>(theEnv);
		EnvAddUDF(env, "bitmask->int", "l", CLIPS_translateBitmask, "CLIPS_translateBitmask", 1, 1, "sy", nullptr);
		EnvAddUDF(env, "binary->int", "l", CLIPS_translateBinary, "CLIPS_translateBinary", 1, 1, "sy", nullptr);
		EnvAddUDF(env, "hex->int", "l", CLIPS_translateHex, "CLIPS_translateHex", 1, 1, "sy", nullptr);
	}
}
