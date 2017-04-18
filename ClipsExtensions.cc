/*
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


#include "ClipsExtensions.h"
#include "Base.h"
#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/trim.hpp>
#include <cstdint>
#include <climits>
#include <sstream>
#include <memory>
#include <map>
#include <iostream>
#include "ExecutionUnits.h"

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

	void CLIPS_translateNumberBase(UDFContext* context, CLIPSValue* ret, const std::string& prefix, int base, const std::string& badPrefix) noexcept {
        constexpr unsigned long long maximumIntegerValue = 0xFFFFFFFFFFFFFFFF;
		CLIPSValue value;
		if (!UDFFirstArgument(context, LEXEME_TYPES, &value)) {
			CVSetBoolean(ret, false);
		} else {
			std::string str(CVToString(&value));
			if (boost::starts_with(str, prefix)) {
				auto tmp = strtoull(str.c_str(), nullptr, base);
				if (tmp == ULLONG_MAX && errno == ERANGE) {
					CLIPS_errorOverflowedNumber(context, ret);
				} else {
					if (tmp > maximumIntegerValue) {
						CLIPS_errorNumberLargerThan64Bits(context, ret);
					} else {
						CVSetInteger(ret, static_cast<CLIPSInteger>(tmp));
					}
				}
			} else {
				CLIPS_errorMessageGeneric(context, ret, badPrefix);
			}
		}
	}
	void CLIPS_translateBinary(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPS_translateNumberBase(context, ret, "0b", 2, "Binary must start with 0b");
	}

	void CLIPS_translateHex(UDFContext* context, CLIPSValue* ret) {
		CLIPS_translateNumberBase(context, ret, "0x", 16, "Hex must start with 0x");
	}


	enum CLIPS_UnaryOperations {
		Not,
		ExpandBits,
	};
	template<CLIPS_UnaryOperations op>
	void CLIPS_genericUnaryIntegerOperation(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPSValue number;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &number)) {
			CVSetBoolean(ret, false);
		} else {
			switch(op) {
				case CLIPS_UnaryOperations::Not:
					CVSetInteger(ret, binaryNot<CLIPSInteger>(CVToInteger(&number)));
					break;
				case CLIPS_UnaryOperations::ExpandBits:
					CVSetInteger(ret, static_cast<CLIPSInteger>(expandBit(CVIsTrueSymbol(&number))));
					break;
				default:
					CVSetBoolean(ret, false);
					break;
			}
		}
	}

    template<syn::ALU::StandardOperations op>
    constexpr bool isBinaryIntegerOperation = false;
    template<> constexpr bool isBinaryIntegerOperation<syn::ALU::StandardOperations::BinaryAnd> = true;
    template<> constexpr bool isBinaryIntegerOperation<syn::ALU::StandardOperations::BinaryOr> = true;
    template<> constexpr bool isBinaryIntegerOperation<syn::ALU::StandardOperations::BinaryXor> = true;
    template<> constexpr bool isBinaryIntegerOperation<syn::ALU::StandardOperations::BinaryNand> = true;
    template<> constexpr bool isBinaryIntegerOperation<syn::ALU::StandardOperations::ShiftLeft> = true;
    template<> constexpr bool isBinaryIntegerOperation<syn::ALU::StandardOperations::ShiftRight> = true;

	template<syn::ALU::StandardOperations op>
	inline void CLIPS_genericBinaryIntegerOperation(UDFContext* context, CLIPSValue* ret) noexcept {
        static_assert(isBinaryIntegerOperation<op>, "Illegal clips binary operation!");
		CLIPSValue a, b;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &a)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &b)) {
			CVSetBoolean(ret, false);
		} else {
			auto first = CVToInteger(&a);
			auto second = CVToInteger(&b);
            CVSetInteger(ret, syn::ALU::performOperation<op, CLIPSInteger>(first, second));
		}
	}
	void CLIPS_binaryAnd(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPS_genericBinaryIntegerOperation<syn::ALU::StandardOperations::BinaryAnd>(context, ret);
	}
	void CLIPS_binaryOr(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPS_genericBinaryIntegerOperation<syn::ALU::StandardOperations::BinaryOr>(context, ret);
	}
	void CLIPS_binaryXor(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPS_genericBinaryIntegerOperation<syn::ALU::StandardOperations::BinaryXor>(context, ret);
	}
	void CLIPS_binaryNand(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPS_genericBinaryIntegerOperation<syn::ALU::StandardOperations::BinaryNand>(context, ret);
	}

	void CLIPS_shiftLeft(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPS_genericBinaryIntegerOperation<syn::ALU::StandardOperations::ShiftLeft>(context, ret);
	}

	void CLIPS_shiftRight(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPS_genericBinaryIntegerOperation<syn::ALU::StandardOperations::ShiftRight>(context, ret);
	}

	void CLIPS_binaryNot(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPS_genericUnaryIntegerOperation<CLIPS_UnaryOperations::Not>(context, ret);
	}

	void CLIPS_expandBit(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPS_genericUnaryIntegerOperation<CLIPS_UnaryOperations::ExpandBits>(context, ret);
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
			CVSetInteger(ret, encodeBits<CLIPSInteger, CLIPSInteger>(CVToInteger(&input), CVToInteger(&value), CVToInteger(&mask), CVToInteger(&shift)));
		}
	}

	void CLIPS_breakApartNumber(UDFContext* context, CLIPSValue* ret) {
		CLIPSValue number;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &number)) {
			CVSetBoolean(ret, false);
		} else {
			auto env = UDFContextEnvironment(context);
			auto integer = CVToInteger(&number);
            static constexpr auto iTypeSize = static_cast<int>(sizeof(decltype(integer)));
			byte container[iTypeSize] = { 0 };
			syn::decodeInt64LE(integer, container);
            MultifieldBuilder mf(env, iTypeSize);
			for (int i = 0, j = 1; i < iTypeSize; ++i, ++j) {
                mf.setField(j, INTEGER, EnvAddLong(env, container[i]));
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
	template<typename T>
	using Block = T[];

    bool PerformArgCheck(void* env, int position, unsigned int type, CLIPSValuePtr storage, const std::string& funcStr) noexcept {
        return EnvArgTypeCheck(env, funcStr.c_str(), position, type, storage);
    }

    bool Arg2IsInteger(void* env, CLIPSValuePtr storage, const std::string& funcStr) noexcept {
        return PerformArgCheck(env, 2, INTEGER, storage, funcStr);
    }
    bool Arg2IsSymbol(void* env, CLIPSValuePtr storage, const std::string& funcStr) noexcept {
        return PerformArgCheck(env, 2, SYMBOL, storage, funcStr);
    }
    void handleProblem(void* env, CLIPSValuePtr ret, syn::Problem& p, const std::string funcErrorPrefix) noexcept {
        CVSetBoolean(ret, false);
        std::stringstream s;
        s << "an exception was thrown: " << p.what();
        auto str = s.str();
        errorMessage(env, "CALL", 2, funcErrorPrefix, str);
    }

    int getArgCount(void* env) noexcept {
        return EnvRtnArgCount(env);
    }
    template<typename T>
    constexpr bool isArithmeticOperation(T value) noexcept {
        switch(value) {
            case T::Combine:
            case T::Difference:
            case T::Product:
            case T::Divide:
            case T::Remainder:
                return true;
            default:
                return false;
        }
    }
    template<typename T>
    constexpr syn::ALU::StandardOperations translateArithmeticOperation(T op) noexcept {
        switch(op) {
            case T::Combine:
                return syn::ALU::StandardOperations::Add;
            case T::Difference:
                return syn::ALU::StandardOperations::Subtract;
            case T::Product:
                return syn::ALU::StandardOperations::Multiply;
            case T::Divide:
                return syn::ALU::StandardOperations::Divide;
            case T::Remainder:
                return syn::ALU::StandardOperations::Remainder;
            default:
                return defaultErrorState<syn::ALU::StandardOperations>;
        }
    }
	template<typename Word>
	class ManagedMemoryBlock : public ExternalAddressWrapper<Block<Word>> {
		public:
			using Address = CLIPSInteger;
			using WordBlock = Block<Word>;
			using Parent = ExternalAddressWrapper<WordBlock>;
			using Self = ManagedMemoryBlock;
			using Self_Ptr = Self*;
			static ManagedMemoryBlock* make(CLIPSInteger capacity) noexcept {
				return new ManagedMemoryBlock(capacity);
			}
			using ManagedMemoryBlock_Ptr = ManagedMemoryBlock*;
			static void newFunction(void* env, DataObjectPtr ret) {
				static bool init = true;
				static std::string funcStr;
				static std::string funcErrorPrefix;
				if (init) {
					init = false;
                    auto functions = syn::retrieveFunctionNames<WordBlock>("new");
                    funcStr = std::get<1>(functions);
                    funcErrorPrefix = std::get<2>(functions);
				}

				try {
                    if (getArgCount(env) == 2) {
						CLIPSValue capacity;
                        if (!Arg2IsInteger(env, &capacity, funcStr)) {
							CVSetBoolean(ret, false);
							errorMessage(env, "NEW", 1, funcErrorPrefix, " expected an integer for capacity!");
						} else {
                            auto size = extractLong(env, capacity);
							auto idIndex = Self::getAssociatedEnvironmentId(env);
                            CVSetExternalAddress(ret, Self::make(size), idIndex);
						}
					} else {
						errorMessage(env, "NEW", 1, funcErrorPrefix, " function new expected no arguments besides type!");
						CVSetBoolean(ret, false);
					}
				} catch(syn::Problem p) {
                    handleProblem(env, ret, p, funcErrorPrefix);
				}
			}
			static bool callFunction(void* env, DataObjectPtr value, DataObjectPtr ret) {
				static bool init = true;
				static std::string funcStr;
				static std::string funcErrorPrefix;
#include "syn_memory_block_defines.h"
				if (init) {
					init = false;
                    auto t = retrieveFunctionNames<WordBlock>("call");
                    funcStr = std::get<1>(t);
                    funcErrorPrefix = std::get<2>(t);
				}
                if (!isExternalAddress(value)) {
					return errorMessage(env, "CALL", 1, funcErrorPrefix, "Function call expected an external address as the first argument!");
                }
                auto callErrorMessage = [env, ret](const std::string& subOp, const std::string& rest) {
                    CVSetBoolean(ret, false);
                    std::stringstream stm;
                    stm << " " << subOp << ": " << rest << std::endl;
                    auto msg = stm.str();
                    return errorMessage(env, "CALL", 3, funcErrorPrefix, msg);
                };
                auto errOutOfRange = [callErrorMessage, env, ret](const std::string& subOp, CLIPSInteger capacity, Address address) {
                    std::stringstream ss;
                    ss << funcErrorPrefix << ": Provided address " << std::hex << address << " is either less than zero or greater than " << std::hex << capacity << std::endl;
                    return callErrorMessage(subOp, ss.str());
                };
                CLIPSValue operation;
                if (!Arg2IsSymbol(env, &operation, funcStr)) {
                    return errorMessage(env, "CALL", 2, funcErrorPrefix, "expected a function name to call!");
                }
                std::string str(extractLexeme(env, operation));
                // translate the op to an enumeration
                auto result = opTranslation.find(str);
                if (result == opTranslation.end()) {
                    CVSetBoolean(ret, false);
                    return callErrorMessage(str, "<- unknown operation requested!");
                }
                auto ptr = static_cast<Self_Ptr>(DOPToExternalAddress(value));
                auto rangeViolation = [errOutOfRange, ptr, &str](Address addr) { errOutOfRange(str, ptr->size(), addr); };
                CLIPSValue arg0, arg1;
                auto checkArg = [callErrorMessage, &str, env](unsigned int index, unsigned int type, const std::string& msg, CLIPSValue* dat) {
                    if (!PerformArgCheck(env, index, type, dat, funcStr)) {
                        return callErrorMessage(str, msg);
                    }
                    return true;
                };
                auto checkArg0 = [checkArg, &arg0, env, &str](unsigned int type, const std::string& msg) { return checkArg(3, type, msg, &arg0); };
                auto checkArg1 = [checkArg, &arg1, env, &str](unsigned int type, const std::string& msg) { return checkArg(4, type, msg, &arg1); };
                auto oneCheck = [checkArg0](unsigned int type, const std::string& msg) { return checkArg0(type, msg); };
                auto twoCheck = [checkArg0, checkArg1](unsigned int type0, const std::string& msg0, unsigned int type1, const std::string& msg1) {
                    return checkArg0(type0, msg0) && checkArg1(type1, msg1);
                };
                MemoryBlockOp op;
                int aCount;
                std::tie(op, aCount) = result->second;
                // if it is registered then check the length
                auto argCount = 2 /* always have two arguments */  + aCount;
                if (argCount != getArgCount(env)) {
                    std::stringstream ss;
                    ss << " expected " << std::dec << argCount << " arguments";
                    CVSetBoolean(ret, false);
                    auto tmp = ss.str();
                    return callErrorMessage(str, tmp);
                }
                CVSetBoolean(ret, true);
                // now check and see if we are looking at a legal
                // instruction count
                auto checkAddr = [ptr, rangeViolation](auto addr) {
                    auto result = ptr->legalAddress(addr);
                    if (!result) {
                        rangeViolation(addr);
                    }
                    return result;
                };
                auto commonSingleIntegerBody = [oneCheck, checkAddr, env, &arg0, ptr](auto fn) {
                    auto check = oneCheck(INTEGER, "First argument must be an address");
                    if (check) {
                        auto addr = extractLong(env, arg0);
                        if (!checkAddr(addr)) {
                            return false;
                        }
                        fn(ptr, addr);
                    }
                    return check;
                };
                if (op == MemoryBlockOp::Type) {
                    Self::getType(ret);
                } else if (op == MemoryBlockOp::Size) {
                    CVSetInteger(ret, ptr->size());
                } else if (op == MemoryBlockOp::Clear) {
                    ptr->clearMemory();
                } else if (op == MemoryBlockOp::Initialize) {
                    ptr->setMemoryToSingleValue(0);
                } else if (op == MemoryBlockOp::Shutdown) {
                    // do nothing right now
                } else if (op == MemoryBlockOp::Get) {
                    auto check = oneCheck(INTEGER, "Argument 0 must be an integer address!");
                    if (check) {
                        auto addr = extractLong(env, arg0);
                        if (!checkAddr(addr)) {
                            return false;
                        }
                        CVSetInteger(ret, ptr->getMemoryCellValue(addr));
                    }
                    return check;
                } else if (op == MemoryBlockOp::Populate) {
                    auto check = oneCheck(INTEGER, "First argument must be an INTEGER value to populate all of the memory cells with!");
                    if (check) {
                        ptr->setMemoryToSingleValue(extractLong(env, arg0));
                    }
                    return check;
                } else if (op == MemoryBlockOp::Increment) {
                    return commonSingleIntegerBody([](auto ptr, auto addr) { ptr->incrementMemoryCell(addr); });
                } else if (op == MemoryBlockOp::Decrement) {
                    return commonSingleIntegerBody([](auto ptr, auto addr) { ptr->decrementMemoryCell(addr); });
                } else if (op == MemoryBlockOp::Swap || op == MemoryBlockOp::Move) {
                    auto check = twoCheck(INTEGER, "First argument must be an address", INTEGER, "Second argument must be an address");
                    if (check) {
                        auto addr0 = extractLong(env, arg0);
                        auto addr1 = extractLong(env, arg1);
                        if (!checkAddr(addr0) || !checkAddr(addr1)) {
                            return false;
                        }
                        if (op == MemoryBlockOp::Swap) {
                            ptr->swapMemoryCells(addr0, addr1);
                        } else {
                            ptr->copyMemoryCell(addr0, addr1);
                        }
                    }
                    return check;
                } else if (op == MemoryBlockOp::Set) {
                    auto check = twoCheck(INTEGER, "First argument must be an address", INTEGER, "Second argument must be an address");
                    if (check) {
                        auto addr0 = extractLong(env, arg0);
                        if (!checkAddr(addr0)) {
                            return false;
                        }
                        auto addr1 = extractLong(env, arg1);
                        ptr->setMemoryCell(addr0, addr1);
                    }
                    return check;
                } else if (isArithmeticOperation(op)) {
                    auto check = twoCheck(INTEGER, "First argument must be an address", INTEGER, "Second argument must be an address!");
                    CVSetBoolean(ret, false);
                    if (check) {
                        auto addr0 = extractLong(env, arg0);
                        auto addr1 = extractLong(env, arg1);
                        if (!checkAddr(addr0) || !checkAddr(addr1)) {
                            return false;
                        }
                        try {
                            auto pCall = translateArithmeticOperation(op);
                            if (syn::isErrorState(pCall)) {
                                return callErrorMessage(str, "<- not an arithmetic operation!");
                            }
                            auto val0 = ptr->getMemoryCellValue(addr0);
                            auto val1 = ptr->getMemoryCellValue(addr1);
                            CVSetInteger(ret, syn::ALU::performOperation<Word>(pCall, val0, val1));
                        } catch (syn::Problem p) {
                            handleProblem(env, ret, p, funcErrorPrefix);
                            return false;
                        }
                    }
                    return check;
                } else {
                    return callErrorMessage(str, "<- legal but unimplemented operation!");
                }
                return true;
			}
			static void registerWithEnvironment(void* env, const char* title) {
				Parent::registerWithEnvironment(env, title, callFunction, newFunction);
			}

			static void registerWithEnvironment(void* env) {
				static bool init = true;
				static std::string func;
				if (init) {
					init = false;
					func = Parent::getType();
				}
				registerWithEnvironment(env, func.c_str());
			}
		public:
			ManagedMemoryBlock(Address capacity) : Parent(std::move(std::make_unique<WordBlock>(capacity))), _capacity(capacity) { }
			inline Address size() const noexcept                                    { return _capacity; }
			inline bool legalAddress(Address idx) const noexcept                    { return inRange<Address>(_capacity, idx); }
			inline Word getMemoryCellValue(Address addr) noexcept                   { return this->_value.get()[addr]; }
			inline void setMemoryCell(Address addr0, Word value) noexcept           { this->_value.get()[addr0] = value; }
			inline void swapMemoryCells(Address addr0, Address addr1) noexcept      { swap<Word>(this->_value.get()[addr0], this->_value.get()[addr1]); }
			inline void decrementMemoryCell(Address address) noexcept               { --this->_value.get()[address]; }
			inline void incrementMemoryCell(Address address) noexcept               { ++this->_value.get()[address]; }
            inline void clearMemory() noexcept                                      { setMemoryToSingleValue(0); }

			inline void copyMemoryCell(Address from, Address to) noexcept {
				auto ptr = this->_value.get();
				ptr[to] = ptr[from];
			}
			inline void setMemoryToSingleValue(Word value) noexcept {
				auto ptr = this->_value.get();
				for (Address i = 0; i < _capacity; ++i) {
					ptr[i] = value;
				}
			}
		private:
			Address _capacity;
	};

	DefWrapperSymbolicName(Block<CLIPSInteger>, "memory-block");
	using StandardManagedMemoryBlock = ManagedMemoryBlock<CLIPSInteger>;
#define DefMemoryBlock(name, type, alias) \
	DefWrapperSymbolicName(Block< type > , name ); \
	using alias = ManagedMemoryBlock< type >
	DefMemoryBlock("memory-block:uint16", uint16_t, ManagedMemoryBlock_uint16);
#undef DefMemoryBlock

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
		EnvAddUDF(env, "break-apart-number", "m", CLIPS_breakApartNumber, "CLIPS_breakApartNumber", 1, 1, "l", nullptr);
		StandardManagedMemoryBlock::registerWithEnvironment(theEnv);
		ManagedMemoryBlock_uint16::registerWithEnvironment(theEnv);
	}

    MultifieldBuilder::MultifieldBuilder(void* env, long capacity) : _size(capacity), _rawMultifield(EnvCreateMultifield(env, capacity)) { }

    void MultifieldBuilder::setField(int index, int type, void* value) {
        if (index <= 0) {
            throw syn::Problem("Can't set a value to a field with a negative index!");
        } else if (index > _size) {
            throw syn::Problem("Attempted to set a field which was out of range of the multifield!");
        }
        SetMFType(_rawMultifield, index, type);
        SetMFValue(_rawMultifield, index, value);
    }
	void MultifieldBuilder::assign(DataObjectPtr ptr) noexcept {
		ptr->type = MULTIFIELD;
		ptr->begin = 0;
		ptr->end = _size - 1;
		ptr->value = _rawMultifield;
	}

    bool isExternalAddress(DataObjectPtr value) noexcept {
        return GetpType(value) == EXTERNAL_ADDRESS;
    }
    CLIPSInteger extractLong(void* env, DataObjectPtr value) noexcept {
        return EnvDOPToLong(env, value);
    }
    CLIPSInteger extractLong(void* env, DataObject& value) noexcept {
        return EnvDOToLong(env, value);
    }

    const char* extractLexeme(void* env, DataObjectPtr value) noexcept {
        return EnvDOPToString(env, value);
    }
    const char* extractLexeme(void* env, DataObject& value) noexcept {
        return EnvDOToString(env, value);
    }
}
