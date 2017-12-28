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

    //bool Arg2IsInteger(Environment* env, UDFValue* storage, const std::string& funcStr) noexcept {
    //    return tryGetArgumentAsInteger(env, funcStr, 2, storage);
    //}
    //bool Arg2IsSymbol(Environment* env, UDFValue* storage, const std::string& funcStr) noexcept {
    //    return tryGetArgumentAsSymbol(env, funcStr, 2, storage);
    //}
    void handleProblem(Environment* env, UDFValue* ret, const syn::Problem& p, const std::string funcErrorPrefix) noexcept {
		setBoolean(env, ret, false);

        std::stringstream s;
        s << "an exception was thrown: " << p.what();
        auto str = s.str();
        errorMessage(env, "CALL", 2, funcErrorPrefix, str);
    }

	template<typename Word>
	class ManagedMemoryBlock : public ExternalAddressWrapper<Block<Word>> {
		public:
			static_assert(std::is_integral<Word>::value, "Expected an integral type to be for type Word");
			using Address = int64_t;
			using WordBlock = Block<Word>;
			using Parent = ExternalAddressWrapper<WordBlock>;
			using Self = ManagedMemoryBlock;
			using Self_Ptr = Self*;
			static ManagedMemoryBlock* make(int64_t capacity) noexcept {
				return new ManagedMemoryBlock(capacity);
			}
			using ManagedMemoryBlock_Ptr = ManagedMemoryBlock*;
			static void newFunction(UDFContext* context, UDFValue* ret) {
				auto* env = context->environment;
				try {
					UDFValue capacity;
					if (!UDFNextArgument(context, MayaType::INTEGER_BIT, &capacity)) {
						setBoolean(env, ret, false);
						errorMessage(env, "NEW", 1, getFunctionErrorPrefixNew<WordBlock>(), " expected an integer for capacity!");
					}
					auto cap = getInteger(capacity);
					auto idIndex = Self::getAssociatedEnvironmentId(env);
					setExternalAddress(env, ret, Self::make(cap), idIndex);
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
				MapWrite,
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
					{ "map-write", std::make_tuple(MemoryBlockOp::MapWrite, 2) },
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

			static bool callFunction(UDFContext* context, UDFValue* theValue, UDFValue* ret) {
				UDFValue operation;
				if (!UDFNextArgument(context, MayaType::SYMBOL_BIT, &operation)) {
					//TODO: put error messages in here
					return false;
				}
				std::string str(getLexeme(&operation));
                // translate the op to an enumeration
				auto* env = context->environment;
				auto result = getParameters(str);
				if (syn::isErrorState(std::get<0>(result))) {
					setBoolean(context, ret, false);
					return false;
                	//return Parent::callErrorMessageCode3(env, ret, str, " <- unknown operation requested!");
				}
                MemoryBlockOp op;
                int aCount;
				setBoolean(env, ret, true);
                auto ptr = static_cast<Self_Ptr>(getExternalAddress(theValue));
				std::tie(op, aCount) = result;
				switch(op) {
					case MemoryBlockOp::Type:
						Self::setType(context, ret);
						break;
					case MemoryBlockOp::Size:
						setInteger(context, ret, ptr->size());
						break;
					case MemoryBlockOp::Get:
						return ptr->load(env, context, ret);
					case MemoryBlockOp::Populate:
						return ptr->populate(env, context, ret);
					case MemoryBlockOp::Increment:
						return ptr->increment(env, context, ret);
					case MemoryBlockOp::Decrement:
						return ptr->decrement(env, context, ret);
					case MemoryBlockOp::Swap:
						return ptr->swap(env, context, ret);
					case MemoryBlockOp::Move:
						return ptr->move(env, context, ret);
					case MemoryBlockOp::Set:
						return ptr->store(env, context, ret);
					default:
						setBoolean(context, ret, false);
                    	//return Parent::callErrorMessageCode3(env, ret, str, "<- legal but unimplemented operation!");
						//TODO: add error message
						return false;
				}
                return true;
			}
			static void registerWithEnvironment(Environment* env, const char* title) {
				Parent::registerWithEnvironment(env, title, callFunction, newFunction);
			}

			static void registerWithEnvironment(Environment* env) {
				registerWithEnvironment(env, Parent::getType().c_str());
			}
		public:
			ManagedMemoryBlock(Address capacity) : Parent(std::move(std::make_unique<WordBlock>(capacity))), _capacity(capacity) { }
			inline Address size() const noexcept                                    { return _capacity; }
			inline bool legalAddress(Address idx) const noexcept                    { return addressInRange<Address>(_capacity, idx); }
			inline Word getMemoryCellValue(Address addr) noexcept                   { return this->_value.get()[addr]; }
			inline void setMemoryCell(Address addr0, Word value) noexcept           { this->_value.get()[addr0] = value; }
			inline void swapMemoryCells(Address addr0, Address addr1) noexcept      { syn::swap<Word>(this->_value.get()[addr0], this->_value.get()[addr1]); }
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
			bool extractInteger(UDFContext* context, UDFValue& storage) noexcept {
				if (!UDFNextArgument(context, MayaType::INTEGER_BIT, &storage)) {
					// TODO: put error message here
					return false;
				}
				return true;
			}
			bool defaultSingleOperationBody(Environment* env, UDFContext* context, UDFValue* ret, std::function<bool(Environment*, UDFContext*, UDFValue*, Address)> body) noexcept {
				UDFValue address;
				if (!extractInteger(context, address)) {
					setBoolean(env, ret, false);
					return false;
				}
				auto value = static_cast<Address>(getInteger(address));
				if (!legalAddress(value)) {
					// TODO: insert error message here about illegal address
					setBoolean(env, ret, false);
					return false;
				}
				return body(env, context, ret, value);
			}
			bool defaultTwoOperationBody(Environment* env, UDFContext* context, UDFValue* ret, std::function<bool(Environment*, UDFContext*, UDFValue*, Address, Address)> body) noexcept {
				UDFValue address, address2;
				if (!extractInteger(context, address)) {
					setBoolean(env, ret, false);
					return false;
				}
				if (!extractInteger(context, address2)) {
					setBoolean(env, ret, false);
					return false;
				}
				auto addr0 = static_cast<Address>(getInteger(address));
				auto addr1 = static_cast<Address>(getInteger(address2));
				if (!legalAddress(addr0) || !legalAddress(addr1)) {
					setBoolean(env, ret, false);
					return false;
				}
				return body(env, context, ret, addr0, addr1);
			}
		public:
			bool populate(Environment* env, UDFContext* context, UDFValue* ret) noexcept {
				UDFValue value;
				if (!extractInteger(context, value)) {
					setBoolean(env, ret, false);
					return false;
				}
				auto population = static_cast<Word>(getInteger(value));
				setMemoryToSingleValue(population);
				setBoolean(env, ret, true);
				return true;
			}
			bool load(Environment* env, UDFContext* context, UDFValue* ret) noexcept {
				return defaultSingleOperationBody(env, context, ret, [this](auto* env, auto* context, auto* ret, auto address) noexcept {
							setInteger(env, ret, this->getMemoryCellValue(address));
							return true;
						});
			}
			bool increment(Environment* env, UDFContext* context, UDFValue* ret) noexcept {
				return defaultSingleOperationBody(env, context, ret, [this](auto* env, auto* context, auto* ret, auto address) noexcept {
							this->incrementMemoryCell(address);
							setBoolean(env, ret, true);
							return true;
						});
			}

			bool decrement(Environment* env, UDFContext* context, UDFValue* ret) noexcept {
				return defaultSingleOperationBody(env, context, ret, [this](auto* env, auto* context, auto* ret, auto address) noexcept {
							this->decrementMemoryCell(address);
							setBoolean(env, ret, true);
							return true;
						});
			}

			bool swap(Environment* env, UDFContext* context, UDFValue* ret) noexcept {
				return defaultTwoOperationBody(env, context, ret, [this](auto* env, auto* context, auto* ret, auto addr0, auto addr1) noexcept {
							this->swapMemoryCells(addr0, addr1);
							setBoolean(env, ret, true);
							return true;
						});
			}

			bool move(Environment* env, UDFContext* context, UDFValue* ret) noexcept {
				return defaultTwoOperationBody(env, context, ret, [this](auto* env, auto* context, auto* ret, auto from, auto to) noexcept {
							this->copyMemoryCell(from, to);
							setBoolean(env, ret, true);
							return true;
						});
			}
			bool store(Environment* env, UDFContext* context, UDFValue* ret) noexcept {
				UDFValue address, value;
				if (!extractInteger(context, address)) {
					setBoolean(env, ret, false);
					return false;
				}

				if (!extractInteger(context, value)) {
					setBoolean(env, ret, false);
					return false;
				}

				auto addr = static_cast<Address>(getInteger(address));
				if (!legalAddress(addr)) {
					setBoolean(env, ret, false);
					return false;
				}

				auto data = static_cast<Word>(getInteger(value));
				this->setMemoryCell(addr, data);
				setBoolean(env, ret, true);
				return true;

				
			}
			bool mapWrite(Environment* env, UDFContext* context, UDFValue* ret) noexcept {
				UDFValue startingAddress;
				if (!extractInteger(context, startingAddress)) {
					setBoolean(env, ret, false);
					return false;
				}
				auto addr = static_cast<Address>(getInteger(startingAddress));
				while(UDFHasNextArgument(context)) {
					if (!legalAddress(addr)) {
						setBoolean(env, ret, false);
						return false;
					}
					UDFValue currentItem;
					if (!extractInteger(context, currentItem)) {
						setBoolean(env, ret, false);
						return false;
					}
					auto data = static_cast<Word>(getInteger(currentItem));
					this->setMemoryCell(addr, data);
					++addr;
				}
				
				setBoolean(env, ret, true);
				return true;
			}

		private:
			Address _capacity;
	};

	DefWrapperSymbolicName(Block<int64_t>, "memory-block");
	using StandardManagedMemoryBlock = ManagedMemoryBlock<int64_t>;
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

	void installMemoryBlockTypes(Environment* theEnv) {
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
