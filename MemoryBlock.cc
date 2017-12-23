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
#include "MemoryBlock.h"

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
					if (syn::getArgCount(env) == 2) {
						CLIPSValue capacity;
                        if (!Arg2IsInteger(env, &capacity, getFunctionPrefixNew<WordBlock>())) {
							CVSetBoolean(ret, false);
							errorMessage(env, "NEW", 1, getFunctionErrorPrefixNew<WordBlock>(), " expected an integer for capacity!");
						} else {
                            auto size = extractCLIPSInteger(env, capacity);
							auto idIndex = Self::getAssociatedEnvironmentId(env);
                            CVSetExternalAddress(ret, Self::make(size), idIndex);
						}
					} else {
						Parent::callErrorMessageCode3(env, ret, getFunctionPrefixNew<WordBlock>(), " either too many or too few arguments provided!");
					}
				} catch(const syn::Problem& p) {
                    handleProblem(env, ret, p, getFunctionErrorPrefixNew<WordBlock>());
				}
			}

			enum class MemoryBlockOp {
				Populate,
				Size,
				Type,
				Set,
				Move,
				Swap,
				Decrement,
				Increment,
				Get,
				Count,
			};
			static std::tuple<MemoryBlockOp, int> getParameters(const std::string& op) noexcept {
				static std::map<std::string, std::tuple<MemoryBlockOp, int>> opTranslation = {
					{ "populate", std::make_tuple(MemoryBlockOp:: Populate , 1) },
					{ "size", std::make_tuple(MemoryBlockOp:: Size , 0) },
					{ "type", std::make_tuple(MemoryBlockOp:: Type , 0) },
					{ "write", std::make_tuple(MemoryBlockOp:: Set , 2) },
					{ "move", std::make_tuple(MemoryBlockOp:: Move , 2) },
					{ "swap", std::make_tuple(MemoryBlockOp:: Swap , 2) },
					{ "decrement", std::make_tuple(MemoryBlockOp:: Decrement , 1) },
					{ "increment", std::make_tuple(MemoryBlockOp:: Increment , 1) },
					{ "read", std::make_tuple(MemoryBlockOp:: Get , 1) },
				};
				static std::tuple<MemoryBlockOp, int> bad;
				static bool init = false;
				if (!init) {
					init = true;
					bad = std::make_tuple(syn::defaultErrorState<MemoryBlockOp>, -1);
				}
				auto result = opTranslation.find(op);
				if (result == opTranslation.end()) {
					return bad;
				} else {
					return result->second;
				}
			}

			static bool callFunction(void* env, DataObjectPtr value, DataObjectPtr ret) {
                __RETURN_FALSE_ON_FALSE__(Parent::isExternalAddress(env, ret, value));
                CLIPSValue operation;
                __RETURN_FALSE_ON_FALSE__(Parent::tryExtractFunctionName(env, ret, &operation));
                std::string str(extractLexeme(env, operation));
                // translate the op to an enumeration
				auto result = getParameters(str);
				if (syn::isErrorState(std::get<0>(result))) {
                	return Parent::callErrorMessageCode3(env, ret, str, " <- unknown operation requested!");
				}
                MemoryBlockOp op;
                int aCount;
                std::tie(op, aCount) = result;
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
                        auto addr = extractCLIPSInteger(env, arg0);
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
						ptr->setMemoryToSingleValue(extractCLIPSInteger(env, arg0));
					}
					return check;
				};
				auto swapOrMove = [env, ret, checkAddr, ptr](auto op) {
					CLIPSValue arg0, arg1;
                    auto check = Parent::tryExtractArgument1(env, ret, &arg0, MayaType::Integer, "First argument must be an address") &&
                                 Parent::tryExtractArgument2(env, ret, &arg1, MayaType::Integer, "Second argument must be an address");
                    if (check) {
                        auto addr0 = extractCLIPSInteger(env, arg0);
                        auto addr1 = extractCLIPSInteger(env, arg1);
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
                        auto addr0 = extractCLIPSInteger(env, arg0);
                        if (!checkAddr(addr0)) {
                            return false;
                        }
                        auto addr1 = extractCLIPSInteger(env, arg1);
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
			inline bool legalAddress(Address idx) const noexcept                    { return addressInRange<Address>(_capacity, idx); }
			inline Word getMemoryCellValue(Address addr) noexcept                   { return this->_value.get()[addr]; }
			inline void setMemoryCell(Address addr0, Word value) noexcept           { this->_value.get()[addr0] = value; }
			inline void swapMemoryCells(Address addr0, Address addr1) noexcept      { swap<Word>(this->_value.get()[addr0], this->_value.get()[addr1]); }
			inline void decrementMemoryCell(Address address) noexcept               { --this->_value.get()[address]; }
			inline void incrementMemoryCell(Address address) noexcept               { ++this->_value.get()[address]; }

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
    DefMemoryBlock("memory-block:uint8", uint8, ManagedMemoryBlock_uint8);
	DefMemoryBlock("memory-block:uint16", uint16, ManagedMemoryBlock_uint16);
	DefMemoryBlock("memory-block:uint32", uint32, ManagedMemoryBlock_uint32);
	DefMemoryBlock("memory-block:int32", int32, ManagedMemoryBlock_int32);
	DefMemoryBlock("memory-block:int16", int16, ManagedMemoryBlock_int16);
    DefMemoryBlock("memory-block:int8", int8, ManagedMemoryBlock_int8);
#undef DefMemoryBlock
#endif // end ENABLE_EXTENDED_MEMORY_BLOCKS

	void installMemoryBlockTypes(void* theEnv) {
		StandardManagedMemoryBlock::registerWithEnvironment(theEnv);
#if ENABLE_EXTENDED_MEMORY_BLOCKS
        ManagedMemoryBlock_uint8::registerWithEnvironment(theEnv);
		ManagedMemoryBlock_uint16::registerWithEnvironment(theEnv);
		ManagedMemoryBlock_uint32::registerWithEnvironment(theEnv);
        ManagedMemoryBlock_int8::registerWithEnvironment(theEnv);
		ManagedMemoryBlock_int16::registerWithEnvironment(theEnv);
		ManagedMemoryBlock_int32::registerWithEnvironment(theEnv);
#endif // end ENABLE_EXTENDED_MEMORY_BLOCKS
	}


}
