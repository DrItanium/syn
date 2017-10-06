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
#include "ClipsExtensions.h"
#include "Base.h"
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
	void CLIPS_errorMessageGeneric(UDFContext* context, CLIPSValue* ret, const char* msg) noexcept {
		UDFInvalidArgumentMessage(context, msg);
		CVSetBoolean(ret, false);
	}
	void CLIPS_errorMessageGeneric(UDFContext* context, CLIPSValue* ret, const std::string& msg) noexcept {
		CLIPS_errorMessageGeneric(context, ret, msg.c_str());
	}
	void CLIPS_errorOverflowedNumber(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPS_errorMessageGeneric(context, ret, "number is too large and overflowed");
	}
	void CLIPS_translateBitmask(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPSValue value;
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
						CVSetInteger(ret, static_cast<CLIPSInteger>(static_cast<byte>(tmp)));
					}
				}
			} else {
				CLIPS_errorMessageGeneric(context, ret, "Bitmask must start with 0m");
			}
		}
	}
	void CLIPS_errorNumberLargerThan64Bits(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPS_errorMessageGeneric(context, ret, "provided number is larger than 64-bits!");
	}


	void CLIPS_expandBit(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPSValue number;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &number)) {
			CVSetBoolean(ret, false);
			return;
		}
		auto value = CVToInteger(&number);
		CVSetInteger(ret, CLIPSInteger(expandBit(value != 0)));
	}

	void CLIPS_basePrintAddress(void* env, const char* logicalName, void* theValue, const char* func, const char* majorType) {
		std::stringstream ss;
		void* ptr = EnvValueToExternalAddress(env, theValue);
		ss << "<" << majorType << "-" << func << "-" << std::hex << ((ptr) ? ptr : theValue) << ">";
		auto str = ss.str();
		EnvPrintRouter(env, logicalName, str.c_str());
	}
	void CLIPS_basePrintAddress_Pointer(void* env, const char* logicalName, void* theValue, const char* func) noexcept {
		CLIPS_basePrintAddress(env, logicalName, theValue, func, "Pointer");
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
	void CLIPS_encodeBits(UDFContext* context, CLIPSValue* ret) noexcept {
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
			auto i = CVToInteger(&input);
			auto v = CVToInteger(&value);
			auto m = CVToInteger(&mask);
			auto s = CVToInteger(&shift);
			CVSetInteger(ret, encodeBits<CLIPSInteger, CLIPSInteger>(i, v, m, s));
		}
	}
    template<typename I, typename O, I mask, int index>
    O performDecode(I input) noexcept {
        return syn::decodeBits<I, O, mask << (8 * index), (8 * index)>(input);
    }
	void CLIPS_breakApartNumber(UDFContext* context, CLIPSValue* ret) {
		CLIPSValue number;
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

	bool errorMessage(void* env, const std::string& idClass, int idIndex, const std::string& msgPrefix, const std::string& msg) noexcept {
		PrintErrorID(env, idClass.c_str(), idIndex, false);
		EnvPrintRouter(env, WERROR, msgPrefix.c_str());
		EnvPrintRouter(env, WERROR, msg.c_str());
		EnvPrintRouter(env, WERROR, "\n");
		EnvSetEvaluationError(env, true);
		return false;
	}
    template<bool shiftLeft>
    void CLIPS_circularShiftBase(UDFContext* context, CLIPSValuePtr ret) {
        CLIPSValue a, b;
        if (!UDFFirstArgument(context, INTEGER_TYPE, &a)) {
            CVSetBoolean(ret, false);
        } else if (!UDFNextArgument(context, INTEGER_TYPE, &b)) {
            CVSetBoolean(ret, false);
        } else {
            auto firstValue = CVToInteger(&a);
            auto secondValue = CVToInteger(&b);
            auto result = shiftLeft ?
                circularShiftLeft<CLIPSInteger>(firstValue, secondValue) :
                circularShiftRight<CLIPSInteger>(firstValue, secondValue);
            CVSetInteger(ret, result);
        }
    }
    void CLIPS_circularShiftLeft(UDFContext* context, CLIPSValuePtr ret) {
        CLIPS_circularShiftBase<true>(context, ret);
    }
    void CLIPS_circularShiftRight(UDFContext* context, CLIPSValuePtr ret) {
        CLIPS_circularShiftBase<false>(context, ret);
    }
    void CLIPS_onesComplement(UDFContext* context, CLIPSValuePtr ret) {
        CLIPSValue val;
        if (!UDFFirstArgument(context, INTEGER_TYPE, &val)) {
            CVSetBoolean(ret, false);
        } else {
            CVSetInteger(ret, onesComplement(CVToInteger(&val)));
        }
    }
    void CLIPS_twosComplement(UDFContext* context, CLIPSValuePtr ret) {
        CLIPSValue val;
        if (!UDFFirstArgument(context, INTEGER_TYPE, &val)) {
            CVSetBoolean(ret, false);
        } else {
            CVSetInteger(ret, twosComplement(CVToInteger(&val)));
        }
    }
    void CLIPS_multiplyAdd(UDFContext* context, CLIPSValuePtr ret) {
        CLIPSValue a, b, c;
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

    void CLIPS_binaryNor(UDFContext* context, CLIPSValuePtr ret) {
        CLIPSValue a, b;
        if (!UDFFirstArgument(context, INTEGER_TYPE, &a)) {
            CVSetBoolean(ret, false);
        } else if (!UDFNextArgument(context, INTEGER_TYPE, &b)) {
            CVSetBoolean(ret, false);
        } else {
            auto firstValue = CVToInteger(&a);
            auto secondValue = CVToInteger(&b);
            CVSetInteger(ret, binaryNor<decltype(firstValue)>(firstValue, secondValue));
        }
    }
	void installExtensions(void* theEnv) {
		Environment* env = static_cast<Environment*>(theEnv);

		EnvAddUDF(env, "bitmask->int", "l", CLIPS_translateBitmask, "CLIPS_translateBitmask", 1, 1, "sy", nullptr);
		EnvAddUDF(env, "expand-bit", "l", CLIPS_expandBit, "CLIPS_expandBit", 1, 1,  nullptr, nullptr);
		EnvAddUDF(env, "decode-bits", "l", CLIPS_decodeBits, "CLIPS_decodeBits", 3, 3, "l;l;l", nullptr);
		EnvAddUDF(env, "encode-bits", "l", CLIPS_encodeBits, "CLIPS_encodeBits", 4, 4, "l;l;l;l", nullptr);
		EnvAddUDF(env, "break-apart-number", "m", CLIPS_breakApartNumber, "CLIPS_breakApartNumber", 1, 1, "l", nullptr);
        EnvAddUDF(env, "circular-shift-right", "l", CLIPS_circularShiftRight, "CLIPS_circularShiftRight", 2, 2, "l;l", nullptr);
        EnvAddUDF(env, "circular-shift-left", "l", CLIPS_circularShiftLeft, "CLIPS_circularShiftLeft", 2, 2, "l;l", nullptr);
        EnvAddUDF(env, "ones-complement", "l", CLIPS_onesComplement, "CLIPS_onesComplement", 1, 1, "l", nullptr);
        EnvAddUDF(env, "two-complement", "l", CLIPS_twosComplement, "CLIPS_twosComplement",1, 1, "l", nullptr);
        EnvAddUDF(env, "multiply-add", "l", CLIPS_multiplyAdd, "CLIPS_multiplyAdd", 3, 3, "l;l;l", nullptr);
        EnvAddUDF(env, "binary-nor", "l", CLIPS_binaryNor, "CLIPS_binaryNor", 2, 2, "l;l", nullptr);
	}

    bool isExternalAddress(DataObjectPtr value) noexcept {
        return GetpType(value) == EXTERNAL_ADDRESS;
    }
    CLIPSInteger extractCLIPSInteger(void* env, DataObjectPtr value) noexcept {
        return EnvDOPToLong(env, value);
    }
    CLIPSInteger extractCLIPSInteger(void* env, DataObject& value) noexcept {
        return EnvDOToLong(env, value);
    }

    const char* extractLexeme(void* env, DataObjectPtr value) noexcept {
        return EnvDOPToString(env, value);
    }
    const char* extractLexeme(void* env, DataObject& value) noexcept {
        return EnvDOToString(env, value);
    }

	CLIPSFloat extractFloat(void* env, DataObject& value) noexcept {
		return EnvDOToDouble(env, value);
	}
	CLIPSFloat extractFloat(void* env, DataObjectPtr value) noexcept {
		return EnvDOPToDouble(env, value);
	}

    bool checkThenGetArgument(void* env, const std::string& function, int position, MayaType type, DataObjectPtr saveTo) noexcept {
        return EnvArgTypeCheck(env, function.c_str(), position, static_cast<int>(type), saveTo);
    }

    bool tryGetArgumentAsInteger(void* env, const std::string& function, int position, DataObjectPtr saveTo) noexcept {
        return checkThenGetArgument(env, function, position, MayaType::Integer, saveTo);
    }

    bool tryGetArgumentAsSymbol(void* env, const std::string& function, int position, DataObjectPtr saveTo) noexcept {
        return checkThenGetArgument(env, function, position, MayaType::Symbol, saveTo);
    }
    bool tryGetArgumentAsString(void* env, const std::string& function, int position, DataObjectPtr saveTo) noexcept {
        return checkThenGetArgument(env, function, position, MayaType::String, saveTo);
    }
    bool hasCorrectArgCount(void* env, int compare) noexcept {
        return compare == getArgCount(env);
    }

    int getArgCount(void* env) noexcept {
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
