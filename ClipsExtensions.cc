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
#include "ExecutionUnits.h"

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
			CVSetInteger(ret, encodeBits<CLIPSInteger, CLIPSInteger>(CVToInteger(&input), CVToInteger(&value), CVToInteger(&mask), CVToInteger(&shift)));
		}
	}
	void CLIPS_breakApartNumber(UDFContext* context, CLIPSValue* ret) {
		CLIPSValue number;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &number)) {
			CVSetBoolean(ret, false);
            return;
		}
        auto env = UDFContextEnvironment(context);
        auto integer = CVToInteger(&number);
        BinaryContainer<decltype(integer)> storage;
        storage.value = integer;
        constexpr int integerWidth = byteCount<decltype(storage)>;
        FixedSizeMultifieldBuilder<integerWidth> mf(env);
        using IType = decltype(integer);
        using OType = decltype(integer);
        for (int i = 0, j = 1; i < integerWidth; ++i, ++j) {
            mf.setField(j, MayaType::Integer, EnvAddLong(env, syn::decodeBits<IType, OType>(integer, 0x00000000000000FF << (8 * i), (8 * i))));
        }
        mf.assign(ret);
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

    bool Arg2IsInteger(void* env, CLIPSValuePtr storage, const std::string& funcStr) noexcept {
        return tryGetArgumentAsInteger(env, funcStr, 2, storage);
    }
    bool Arg2IsSymbol(void* env, CLIPSValuePtr storage, const std::string& funcStr) noexcept {
        return tryGetArgumentAsSymbol(env, funcStr, 2, storage);
    }
    void handleProblem(void* env, CLIPSValuePtr ret, const syn::Problem& p, const std::string funcErrorPrefix) noexcept {
        CVSetBoolean(ret, false);
        std::stringstream s;
        s << "an exception was thrown: " << p.what();
        auto str = s.str();
        errorMessage(env, "CALL", 2, funcErrorPrefix, str);
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
				try {
                    // TODO: fix this checkArgumentCount to not reference
                    // the call version of this function
                    if (Parent::checkArgumentCount(env, ret, getFunctionPrefixNew<WordBlock>(), 2)) {
						CLIPSValue capacity;
                        if (!Arg2IsInteger(env, &capacity, getFunctionPrefixNew<WordBlock>())) {
							CVSetBoolean(ret, false);
							errorMessage(env, "NEW", 1, getFunctionErrorPrefixNew<WordBlock>(), " expected an integer for capacity!");
						} else {
                            auto size = extractLong(env, capacity);
							auto idIndex = Self::getAssociatedEnvironmentId(env);
                            CVSetExternalAddress(ret, Self::make(size), idIndex);
						}
					}
				} catch(const syn::Problem& p) {
                    handleProblem(env, ret, p, getFunctionErrorPrefixNew<WordBlock>());
				}
			}

			static bool callFunction(void* env, DataObjectPtr value, DataObjectPtr ret) {
#include "defines_syn_memory_block.h"
                __RETURN_FALSE_ON_FALSE__(Parent::isExternalAddress(env, ret, value));
                CLIPSValue operation;
                __RETURN_FALSE_ON_FALSE__(Parent::tryExtractFunctionName(env, ret, &operation));
                std::string str(extractLexeme(env, operation));
                // translate the op to an enumeration
                auto result = opTranslation.find(str);
				__RETURN_FALSE_ON_FALSE__(Parent::isLegalOperation(env, ret, str, result, opTranslation.end()));
                MemoryBlockOp op;
                int aCount;
                std::tie(op, aCount) = result->second;
                __RETURN_FALSE_ON_FALSE__(Parent::checkArgumentCount(env, ret, str, aCount));
                CVSetBoolean(ret, true);
                auto ptr = static_cast<Self_Ptr>(DOPToExternalAddress(value));
                auto errOutOfRange = [env, ret](const std::string& subOp, CLIPSInteger capacity, Address address) noexcept {
                    std::stringstream ss;
                    ss << "Provided address " << std::hex << address << " is either less than zero or greater than " << std::hex << capacity << std::endl;
                    return Parent::callErrorMessageCode3(env, ret, subOp, ss.str());
                };
                auto rangeViolation = [errOutOfRange, ptr, &str](Address addr) { errOutOfRange(str, ptr->size(), addr); };
                // now check and see if we are looking at a legal
                // instruction count
                auto checkAddr = [ptr, rangeViolation](auto addr) {
                    auto result = ptr->legalAddress(addr);
                    if (!result) {
                        rangeViolation(addr);
                    }
                    return result;
                };
                auto commonSingleIntegerBody = [checkAddr, env, ret, ptr](auto fn) {
					CLIPSValue arg0;
                    auto check = Parent::tryExtractArgument1(env, ret, &arg0, MayaType::Integer, "First argument be be an address");
                    if (check) {
                        auto addr = extractLong(env, arg0);
                        if (!checkAddr(addr)) {
                            return false;
                        }
                        fn(ptr, addr);
                    }
                    return check;
                };
				auto populate = [env, ret, ptr]() {
					CLIPSValue arg0;
					auto check = Parent::tryExtractArgument1(env, ret, &arg0, MayaType::Integer, "First argument must be an integer value to populate all of the memory cells with!");
					if (check) {
						ptr->setMemoryToSingleValue(extractLong(env, arg0));
					}
					return check;
				};
				auto swapOrMove = [env, ret, checkAddr, ptr](auto op) {
					CLIPSValue arg0, arg1;
                    auto check = Parent::tryExtractArgument1(env, ret, &arg0, MayaType::Integer, "First argument must be an address") &&
                                 Parent::tryExtractArgument2(env, ret, &arg1, MayaType::Integer, "Second argument must be an address");
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
				};
				auto setAction = [env, ret, checkAddr, ptr]() {
					CLIPSValue arg0, arg1;
                    auto check = Parent::tryExtractArgument1(env, ret, &arg0, MayaType::Integer, "First argument must be an address") &&
                                 Parent::tryExtractArgument2(env, ret, &arg1, MayaType::Integer, "Second argument must be an address");
                    if (check) {
                        auto addr0 = extractLong(env, arg0);
                        if (!checkAddr(addr0)) {
                            return false;
                        }
                        auto addr1 = extractLong(env, arg1);
                        ptr->setMemoryCell(addr0, addr1);
                    }
                    return check;
				};
				switch(op) {
					case MemoryBlockOp::Type:
						Self::setType(ret);
						break;
					case MemoryBlockOp::Size:
						CVSetInteger(ret, ptr->size());
						break;
					case MemoryBlockOp::Clear:
						ptr->clearMemory();
						break;
					case MemoryBlockOp::Initialize:
						ptr->setMemoryToSingleValue(0);
						break;
					case MemoryBlockOp::Shutdown:
						break;
					case MemoryBlockOp::Get:
						return commonSingleIntegerBody([ret](auto ptr, auto addr) { CVSetInteger(ret, ptr->getMemoryCellValue(addr)); });
					case MemoryBlockOp::Populate:
						return populate();
					case MemoryBlockOp::Increment:
						return commonSingleIntegerBody([](auto ptr, auto addr) { ptr->incrementMemoryCell(addr); });
					case MemoryBlockOp::Decrement:
						return commonSingleIntegerBody([](auto ptr, auto addr) { ptr->decrementMemoryCell(addr); });
					case MemoryBlockOp::Swap:
					case MemoryBlockOp::Move:
						return swapOrMove(op);
					case MemoryBlockOp::Set:
						return setAction();
					default:
                    	return Parent::callErrorMessageCode3(env, ret, str, "<- legal but unimplemented operation!");
				}
                return true;
			}
			static void registerWithEnvironment(void* env, const char* title) {
				Parent::registerWithEnvironment(env, title, callFunction, newFunction);
			}

			static void registerWithEnvironment(void* env) {
				registerWithEnvironment(env, Parent::getType().c_str());
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
#ifndef ENABLE_EXTENDED_MEMORY_BLOCKS
#define ENABLE_EXTENDED_MEMORY_BLOCKS 0
#endif // end ENABLE_EXTENDED_MEMORY_BLOCKS

#if ENABLE_EXTENDED_MEMORY_BLOCKS
#define DefMemoryBlock(name, type, alias) \
	DefWrapperSymbolicName(Block< type > , name ); \
	using alias = ManagedMemoryBlock< type >
	DefMemoryBlock("memory-block:uint16", uint16, ManagedMemoryBlock_uint16);
	DefMemoryBlock("memory-block:uint32", uint32, ManagedMemoryBlock_uint32);
	DefMemoryBlock("memory-block:int32", int32, ManagedMemoryBlock_int32);
	DefMemoryBlock("memory-block:int16", int16, ManagedMemoryBlock_int16);
#undef DefMemoryBlock
#endif // end ENABLE_EXTENDED_MEMORY_BLOCKS

	void installExtensions(void* theEnv) {
		Environment* env = static_cast<Environment*>(theEnv);

		EnvAddUDF(env, "bitmask->int", "l", CLIPS_translateBitmask, "CLIPS_translateBitmask", 1, 1, "sy", nullptr);
		EnvAddUDF(env, "expand-bit", "l", CLIPS_expandBit, "CLIPS_expandBit", 1, 1,  nullptr, nullptr);
		EnvAddUDF(env, "decode-bits", "l", CLIPS_decodeBits, "CLIPS_decodeBits", 3, 3, "l;l;l", nullptr);
		EnvAddUDF(env, "encode-bits", "l", CLIPS_encodeBits, "CLIPS_encodeBits", 4, 4, "l;l;l;l", nullptr);
		EnvAddUDF(env, "break-apart-number", "m", CLIPS_breakApartNumber, "CLIPS_breakApartNumber", 1, 1, "l", nullptr);
		StandardManagedMemoryBlock::registerWithEnvironment(theEnv);
#if ENABLE_EXTENDED_MEMORY_BLOCKS
		ManagedMemoryBlock_uint16::registerWithEnvironment(theEnv);
		ManagedMemoryBlock_uint32::registerWithEnvironment(theEnv);
		ManagedMemoryBlock_int16::registerWithEnvironment(theEnv);
		ManagedMemoryBlock_int32::registerWithEnvironment(theEnv);
#endif // end ENABLE_EXTENDED_MEMORY_BLOCKS
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
