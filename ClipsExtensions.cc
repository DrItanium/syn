/**
 * @file
 * implementation of methods described in ClipsExtensions.h
 * @copyright
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


#include "BaseTypes.h"
#include "Base.h"
#include "ClipsExtensions.h"
#include "ExternalAddressWrapper.h"
#include "MultifieldBuilder.h"

#include <cstdint>
#include <climits>
#include <sstream>
#include <memory>
#include <map>
#include <iostream>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/trim.hpp>

extern "C" {
#include "clips.h"
}

namespace syn {
	void CLIPS_errorMessageGeneric(UDFContext* context, UDFValue* ret, const char* msg) noexcept {
		UDFInvalidArgumentMessage(context, msg);
		CVSetBoolean(ret, false);
	}
	void CLIPS_errorMessageGeneric(UDFContext* context, UDFValue* ret, const std::string& msg) noexcept {
		CLIPS_errorMessageGeneric(context, ret, msg.c_str());
	}
	void CLIPS_errorOverflowedNumber(UDFContext* context, UDFValue* ret) noexcept {
		CLIPS_errorMessageGeneric(context, ret, "number is too large and overflowed");
	}
	void CLIPS_translateBitmask(UDFContext* context, UDFValue* ret) noexcept {
		UDFValue value;
		if (!UDFFirstArgument(context, LEXEME_TYPES, &value)) {
			CVSetBoolean(ret, false);
		} else {
			std::string str(CVToString(&value));
			if (boost::starts_with(str, "0m")) {
				str.at(1) = '0';
				auto tmp = strtoul(str.c_str(), NULL, 2);
				if (tmp == ULONG_MAX && errno == ERANGE) {
					CLIPS_errorOverflowedNumber(context, ret);
				} else {
					if (tmp > 0xFF) {
						CLIPS_errorMessageGeneric(context, ret, "provided number is larger than 8-bits!");
					} else {
						CVSetInteger(ret, static_cast<int64_t>(static_cast<byte>(tmp)));
					}
				}
			} else {
				CLIPS_errorMessageGeneric(context, ret, "Bitmask must start with 0m");
			}
		}
	}
	void CLIPS_errorNumberLargerThan64Bits(UDFContext* context, UDFValue* ret) noexcept {
		CLIPS_errorMessageGeneric(context, ret, "provided number is larger than 64-bits!");
	}


	void CLIPS_expandBit(UDFContext* context, UDFValue* ret) noexcept {
		UDFValue number;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &number)) {
			CVSetBoolean(ret, false);
			return;
		}
		auto value = CVToInteger(&number);
		CVSetInteger(ret, int64_t(expandBit(value != 0)));
	}

	void CLIPS_basePrintAddress(Environment* env, const char* logicalName, void* theValue, const char* func, const char* majorType) {
		std::stringstream ss;
		void* ptr = EnvValueToExternalAddress(env, theValue);
		ss << "<" << majorType << "-" << func << "-" << std::hex << ((ptr) ? ptr : theValue) << ">";
		auto str = ss.str();
        clips::printRouter(env, logicalName, str);
	}
	void CLIPS_basePrintAddress_Pointer(Environment* env, const char* logicalName, void* theValue, const char* func) noexcept {
		CLIPS_basePrintAddress(env, logicalName, theValue, func, "Pointer");
	}
	void CLIPS_decodeBits(UDFContext* context, UDFValue* ret) {
		UDFValue value, mask, shift;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &value)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &mask)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &shift)) {
			CVSetBoolean(ret, false);
		} else {
			CVSetInteger(ret, decodeBits<int64_t, int64_t>(CVToInteger(&value), CVToInteger(&mask), CVToInteger(&shift)));
		}
	}
	void CLIPS_encodeBits(UDFContext* context, UDFValue* ret) noexcept {
		UDFValue input, value, mask, shift;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &input)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &value)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &mask)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &shift)) {
			CVSetBoolean(ret, false);
		} else {
			auto i = CVToInteger(&input);
			auto v = CVToInteger(&value);
			auto m = CVToInteger(&mask);
			auto s = CVToInteger(&shift);
			CVSetInteger(ret, encodeBits<int64_t, int64_t>(i, v, m, s));
		}
	}
    template<typename I, typename O, I mask, int index>
    O performDecode(I input) noexcept {
        return syn::decodeBits<I, O, mask << (8 * index), (8 * index)>(input);
    }
	void CLIPS_breakApartNumber(UDFContext* context, UDFValue* ret) {
		UDFValue number;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &number)) {
			CVSetBoolean(ret, false);
            return;
		}
        auto env = UDFContextEnvironment(context);
        auto integer = CVToInteger(&number);
        constexpr int integerWidth = byteCount<decltype(integer)>;
        constexpr auto baseMask = static_cast<decltype(integer)>(0xFF);
        using IType = decltype(integer);
        using OType = decltype(integer);
        if (integerWidth == 8) {
            createMultifield(env, ret,
                    performDecode<IType, OType, baseMask, 0>(integer),
                    performDecode<IType, OType, baseMask, 1>(integer),
                    performDecode<IType, OType, baseMask, 2>(integer),
                    performDecode<IType, OType, baseMask, 3>(integer),
                    performDecode<IType, OType, baseMask, 4>(integer),
                    performDecode<IType, OType, baseMask, 5>(integer),
                    performDecode<IType, OType, baseMask, 6>(integer),
                    performDecode<IType, OType, baseMask, 7>(integer));
        } else {
            FixedSizeMultifieldBuilder<integerWidth> mf(env);
            for (int i = 0, j = 1; i < integerWidth; ++i, ++j) {
                mf.setField(j, MayaType::Integer, EnvAddLong(env, syn::decodeBits<IType, OType>(integer, baseMask << (8 * i), (8 * i))));
            }
            mf.assign(ret);
        }
	}

	bool errorMessage(Environment* env, const std::string& idClass, int idIndex, const std::string& msgPrefix, const std::string& msg) noexcept {
		PrintErrorID(env, idClass.c_str(), idIndex, false);
        clips::printRouter(env, WERROR, msgPrefix);
        clips::printRouter(env, WERROR, msg);
        clips::printLine(env, WERROR);
		EnvSetEvaluationError(env, true);
		return false;
	}
	bool errorMessage(UDFContext* env, const std::string& idClass, int idIndex, const std::string& msgPrefix, const std::string& msg) noexcept {
        return errorMessage(UDFContextEnvironment(env), idClass, idIndex, msgPrefix, msg);
    }
    template<bool shiftLeft>
    void CLIPS_circularShiftBase(UDFContext* context, UDFValue* ret) {
        UDFValue a, b;
        if (!UDFFirstArgument(context, INTEGER_TYPE, &a)) {
            CVSetBoolean(ret, false);
        } else if (!UDFNextArgument(context, INTEGER_TYPE, &b)) {
            CVSetBoolean(ret, false);
        } else {
            auto firstValue = CVToInteger(&a);
            auto secondValue = CVToInteger(&b);
            auto result = shiftLeft ?
                circularShiftLeft<int64_t>(firstValue, secondValue) :
                circularShiftRight<int64_t>(firstValue, secondValue);
            CVSetInteger(ret, result);
        }
    }
    void CLIPS_circularShiftLeft(UDFContext* context, UDFValue* ret) {
        CLIPS_circularShiftBase<true>(context, ret);
    }
    void CLIPS_circularShiftRight(UDFContext* context, UDFValue* ret) {
        CLIPS_circularShiftBase<false>(context, ret);
    }
    void CLIPS_onesComplement(UDFContext* context, UDFValue* ret) {
        UDFValue val;
        if (!UDFFirstArgument(context, INTEGER_TYPE, &val)) {
            CVSetBoolean(ret, false);
        } else {
            CVSetInteger(ret, onesComplement(CVToInteger(&val)));
        }
    }
    void CLIPS_twosComplement(UDFContext* context, UDFValue* ret) {
        UDFValue val;
        if (!UDFFirstArgument(context, INTEGER_TYPE, &val)) {
            CVSetBoolean(ret, false);
        } else {
            CVSetInteger(ret, twosComplement(CVToInteger(&val)));
        }
    }
    void CLIPS_multiplyAdd(UDFContext* context, UDFValue* ret) {
        UDFValue a, b, c;
        if (!UDFFirstArgument(context, INTEGER_TYPE, &a)) {
            CVSetBoolean(ret, false);
        } else if (!UDFNextArgument(context, INTEGER_TYPE, &b)) {
            CVSetBoolean(ret, false);
        } else if (!UDFNextArgument(context, INTEGER_TYPE, &c)) {
            CVSetBoolean(ret, false);
        } else {
            CVSetInteger(ret, multiplyAdd(CVToInteger(&a), CVToInteger(&b), CVToInteger(&c)));
        }
    }
    void CLIPS_getEndianness(UDFContext* context, UDFValue* ret) {
        static bool init = true;
        static std::string storage;
        if (init) {
            init = false;
            if (syn::isBigEndian()) {
                storage = "big";
            } else if (syn::isLittleEndian()) {
                storage = "little";
            } else {
                storage = "unknown";
            }
        }
        // only compute this once!
        CVSetSymbol(ret, storage.c_str());
    }

    template<bool upperHalf>
    void upperLowerHalfManip(Environment* env, UDFContext* context, UDFValue* ret) {
        UDFValue num;
        if (!UDFFirstArgument(context, INTEGER_TYPE, &num)) {
            CVSetBoolean(ret, false);
        } else {
            int64_t n = CVToInteger(&num);
            if (upperHalf) {
                CVSetInteger(ret, syn::decodeBits<int64_t, int64_t, static_cast<int64_t>(0xFFFFFFFF00000000), getShiftCount<int64_t>()>(n));
            } else {
                CVSetInteger(ret, decodeBits<int64_t, int64_t, 0x00000000FFFFFFFF, 0>(n));
            }
        }
    }

    void CLIPS_getUpperHalf(Environment* env, UDFContext* c, UDFValue* ret) { upperLowerHalfManip<true>(env, c, ret); }
    void CLIPS_getLowerHalf(Environment* env, UDFContext* c, UDFValue* ret) { upperLowerHalfManip<false>(env, c, ret); }

	void installExtensions(Environment* theEnv) {

		AddUDF(theEnv, "bitmask->int", "l", 1, 1, "sy", CLIPS_translateBitmask, "CLIPS_translateBitmask", nullptr);
		AddUDF(theEnv, "expand-bit", "l", 1, 1,  nullptr,  CLIPS_expandBit, "CLIPS_expandBit",  nullptr);
		AddUDF(theEnv, "decode-bits", "l", 3, 3, "l;l;l",   CLIPS_decodeBits, "CLIPS_decodeBits",nullptr);
		AddUDF(theEnv, "encode-bits", "l", 4, 4, "l;l;l;l", CLIPS_encodeBits, "CLIPS_encodeBits",nullptr);
		AddUDF(theEnv, "break-apart-number", "m",    1, 1, "l",   CLIPS_breakApartNumber, "CLIPS_breakApartNumber",    nullptr);
        AddUDF(theEnv, "circular-shift-right", "l",  2, 2, "l;l", CLIPS_circularShiftRight, "CLIPS_circularShiftRight",nullptr);
        AddUDF(theEnv, "circular-shift-left", "l",   2, 2, "l;l", CLIPS_circularShiftLeft, "CLIPS_circularShiftLeft",  nullptr);
        AddUDF(theEnv, "ones-complement", "l",  1, 1, "l",     CLIPS_onesComplement, "CLIPS_onesComplement",nullptr);
        AddUDF(theEnv, "twos-complement", "l",  1, 1, "l",     CLIPS_twosComplement, "CLIPS_twosComplement",nullptr);
        AddUDF(theEnv, "multiply-add", "l",     3, 3, "l;l;l", CLIPS_multiplyAdd, "CLIPS_multiplyAdd",      nullptr);
        AddUDF(theEnv, "get-endian", "sy",      0, 0, nullptr, CLIPS_getEndianness, "CLIPS_getEndianness",  nullptr);
        AddUDF(theEnv, "upper-half", "l",  1, 1, "l", CLIPS_getUpperHalf, "CLIPS_getUpperHalf",nullptr);
        AddUDF(theEnv, "lower-half", "l",  1, 1, "l", CLIPS_getLowerHalf, "CLIPS_getLowerHalf",nullptr);
	}

    bool isExternalAddress(DataObjectPtr value) noexcept {
        return GetpType(value) == EXTERNAL_ADDRESS;
    }
    int64_t extractInteger(Environment* env, DataObjectPtr value) noexcept {
        return EnvDOPToLong(env, value);
    }
    int64_t extractInteger(Environment* env, DataObject& value) noexcept {
        return EnvDOToLong(env, value);
    }

    const char* extractLexeme(Environment* env, DataObjectPtr value) noexcept {
        return EnvDOPToString(env, value);
    }
    const char* extractLexeme(Environment* env, DataObject& value) noexcept {
        return EnvDOToString(env, value);
    }

	CLIPSFloat extractFloat(Environment* env, DataObject& value) noexcept {
		return EnvDOToDouble(env, value);
	}
	CLIPSFloat extractFloat(Environment* env, DataObjectPtr value) noexcept {
		return EnvDOPToDouble(env, value);
	}

    bool checkThenGetArgument(Environment* env, const std::string& function, int position, MayaType type, DataObjectPtr saveTo) noexcept {
        return EnvArgTypeCheck(env, function.c_str(), position, static_cast<int>(type), saveTo);
    }

    bool tryGetArgumentAsInteger(Environment* env, const std::string& function, int position, DataObjectPtr saveTo) noexcept {
        return checkThenGetArgument(env, function, position, MayaType::Integer, saveTo);
    }

    bool tryGetArgumentAsSymbol(Environment* env, const std::string& function, int position, DataObjectPtr saveTo) noexcept {
        return checkThenGetArgument(env, function, position, MayaType::Symbol, saveTo);
    }
    bool tryGetArgumentAsString(Environment* env, const std::string& function, int position, DataObjectPtr saveTo) noexcept {
        return checkThenGetArgument(env, function, position, MayaType::String, saveTo);
    }
    bool hasCorrectArgCount(Environment* env, int compare) noexcept {
        return compare == getArgCount(env);
    }

    int getArgCount(Environment* env) noexcept {
        return EnvRtnArgCount(env);
    }

    void buildFunctionErrorString(std::ostream& stream, const std::string& action, const std::string& name) noexcept {
        stream << "Function ";
        buildFunctionString(stream, action, name);
    }
    void buildFunctionString(std::ostream& stream, const std::string& action, const std::string& name) noexcept {
        stream << "Function " << action << " (" << name << ")";
    }

}
