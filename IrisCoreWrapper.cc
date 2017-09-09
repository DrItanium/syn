/**
 * @file
 * Definition and implementation of the core wrapper for iris
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

#include "IrisCoreWrapper.h"
#include "IrisCore.h"
#include "ClipsExtensions.h"
#include "CoreWrapper.h"
#include "IrisCoreAssemblerStructures.h"


namespace iris {
    /// CLIPS interface to an instance of an iris core
    class CoreWrapper : public syn::CoreWrapper<Core> {
        public:
            using Parent = syn::CoreWrapper<Core>;
            /// list of operations exposed to CLIPS
			enum class Operations {
                WriteDataMemory,
                ReadDataMemory,
                WriteCodeMemory,
                ReadCodeMemory,
                WriteStackMemory,
                ReadStackMemory,
                WriteIOMemory,
                ReadIOMemory,
				GetRegister,
				SetRegister,
                GetPredicateRegister,
                SetPredicateRegister,
                __DEFAULT_ERROR_STATE__,
			};
            /// list of spaces that the core exposes conceptually
            enum class TargetSpace {
                None,
                Code,
                Data,
                Stack,
                IO,
                GPR,
                Predicates,
            };

        public:
            using Parent::Parent;
			virtual bool decodeInstruction(void* env, syn::DataObjectPtr ret, const std::string& op) override;
			virtual CLIPSInteger getWordSize() const noexcept override;
			virtual CLIPSInteger getAddressSize() const noexcept override;
    };
	CLIPSInteger CoreWrapper::getWordSize() const noexcept {
		return sizeof(word);
	}
	CLIPSInteger CoreWrapper::getAddressSize() const noexcept {
		return sizeof(word);
	}
	bool CoreWrapper::decodeInstruction(void* env, syn::DataObjectPtr ret, const std::string& op) {
		__RETURN_FALSE_ON_FALSE__(CoreWrapper::checkArgumentCount(env, ret, op, 1));
		CLIPSValue instruction;
		__RETURN_FALSE_ON_FALSE__(CoreWrapper::tryExtractArgument1(env, ret, &instruction, syn::MayaType::Integer, "Must provide an instruction as an integer!"));
		auto result = syn::extractLong<raw_instruction>(env, instruction);
		auto outcome = translateInstruction(result);
		CVSetString(ret, outcome.c_str());
		return true;
	}
	void installCoreWrapper(void* env) {
		CoreWrapper::registerWithEnvironment(env);
	}
    template<CoreWrapper::TargetSpace expectedSpace, word expectedValue>
    constexpr bool inGivenSpaceAndValueGreaterThanExpected(CoreWrapper::TargetSpace space, word value) noexcept {
        return (expectedSpace == space) && (value >= expectedValue);
    }
    /**
     * Is the given TargetSpace one which holds registers or isn't a space at
     * all?
     * @param space the space to check
     * @return true if the given space holds registers or doesn't hold anything
     */
    constexpr bool registerSpaceOrNone(CoreWrapper::TargetSpace space) noexcept {
        using TargetSpace = CoreWrapper::TargetSpace;
        switch(space) {
            case TargetSpace::None:
            case TargetSpace::GPR:
            case TargetSpace::Predicates:
                return true;
            default:
                return false;
        }
    }

	bool Core::handleOperation(void* env, CLIPSValue* ret) {
        using TargetSpace = CoreWrapper::TargetSpace;
		using WrappedOp = CoreWrapper::Operations;
		using OpToArgCount = std::tuple<WrappedOp, int, TargetSpace>;
		static std::map<std::string, OpToArgCount> ops = {
			{ "write-data-memory", std::make_tuple(WrappedOp::WriteDataMemory, 2, TargetSpace::Data) },
			{ "read-data-memory", std::make_tuple(WrappedOp::ReadDataMemory, 1, TargetSpace::Data  ) },
			{ "write-code-memory", std::make_tuple(WrappedOp::WriteCodeMemory, 2, TargetSpace::Code ) },
			{ "read-code-memory", std::make_tuple(WrappedOp::ReadCodeMemory, 1, TargetSpace::Code ) },
			{ "write-io-memory", std::make_tuple(WrappedOp::WriteIOMemory, 2 , TargetSpace::IO ) },
			{ "read-io-memory", std::make_tuple(WrappedOp::ReadCodeMemory, 1 , TargetSpace::IO ) },
			{ "write-stack-memory", std::make_tuple(WrappedOp::WriteStackMemory, 2, TargetSpace::Stack ) },
			{ "read-stack-memory", std::make_tuple(WrappedOp::ReadStackMemory, 1, TargetSpace::Stack) },
			{ "get-register", std::make_tuple(WrappedOp::GetRegister, 1, TargetSpace::GPR ) },
			{ "set-register", std::make_tuple(WrappedOp::SetRegister, 2, TargetSpace::GPR) },
			{ "get-predicate-register", std::make_tuple(WrappedOp::GetRegister, 1, TargetSpace::Predicates ) },
			{ "set-predicate-register", std::make_tuple(WrappedOp::SetRegister, 2, TargetSpace::Predicates) },
		};
		CLIPSValue operation;
        __RETURN_FALSE_ON_FALSE__(CoreWrapper::tryExtractFunctionName(env, ret, &operation));
		std::string opStr(syn::extractLexeme(env, operation));
		auto result = ops.find(opStr);
		if (result == ops.end()) {
			return CoreWrapper::callErrorMessageCode3(env, ret, opStr, " <- unknown operation requested!");
		}
		WrappedOp fop;
		int argCount;
        TargetSpace space;
		std::tie(fop, argCount, space) = result->second;
        __RETURN_FALSE_ON_FALSE__(CoreWrapper::checkArgumentCount(env, ret, opStr, argCount));
		auto getRegister = [this, env, ret](TargetSpace space) noexcept {
            if (space != TargetSpace::GPR && space != TargetSpace::Predicates) {
                return CoreWrapper::callErrorCode4(env, ret, "Illegal space provided for retrieving a register from!");
            }
			CLIPSValue index;
            __RETURN_FALSE_ON_FALSE__(CoreWrapper::tryExtractArgument1(env, ret, &index, syn::MayaType::Integer, "Must provide an integer index to retrieve a register value!"));
			auto i = syn::extractLong(env, index);
            if (i < 0) {
                return CoreWrapper::callErrorCode3(env, ret, "Was given a negative register index!");
            }
            if (inGivenSpaceAndValueGreaterThanExpected<TargetSpace::GPR, ArchitectureConstants::RegisterCount>(space, i)) {
                return CoreWrapper::callErrorCode3(env, ret, "Illegal register index!");
			}
            if (inGivenSpaceAndValueGreaterThanExpected<TargetSpace::Predicates, ArchitectureConstants::ConditionRegisterCount>(space, i)) {
                return CoreWrapper::callErrorCode3(env, ret, "Illegal condition register index!");
			}
            try {
                switch(space) {
                    case TargetSpace::GPR:
                        CVSetInteger(ret, gpr[static_cast<byte>(i)]);
                        return true;
                    case TargetSpace::Predicates:
                        CVSetBoolean(ret, getPredicateRegister(static_cast<byte>(i)));
                        return true;
                    default:
                        return CoreWrapper::callErrorCode4(env, ret, "illegal space specified for retrieving registers!");
                }
            } catch(const syn::Problem& p) {
                return CoreWrapper::callErrorCode4(env, ret, p.what());
            }
		};
		auto setRegister = [this, env, ret](TargetSpace space) noexcept {
            if (space != TargetSpace::GPR && space != TargetSpace::Predicates) {
                return CoreWrapper::callErrorCode4(env, ret, "Illegal space provided for setting a register!");
            }
			CLIPSValue index;
            __RETURN_FALSE_ON_FALSE__(CoreWrapper::tryExtractArgument1(env, ret, &index, syn::MayaType::Integer, "Must provide an integer index to assign a register value!"));
			auto ind = syn::extractLong(env, index);
            if (ind < 0) {
                return CoreWrapper::callErrorCode3(env, ret, "Was given a negative address!");
            }
            if (inGivenSpaceAndValueGreaterThanExpected<TargetSpace::GPR, ArchitectureConstants::RegisterCount>(space, ind)) {
                return CoreWrapper::callErrorCode3(env, ret, "Illegal register index!");
			}
            if (inGivenSpaceAndValueGreaterThanExpected<TargetSpace::Predicates, ArchitectureConstants::ConditionRegisterCount>(space, ind)) {
                return CoreWrapper::callErrorCode3(env, ret, "Illegal condition register index!");
			}
            CLIPSValue value;
            __RETURN_FALSE_ON_FALSE__(CoreWrapper::tryExtractArgument2(env, ret, &value, syn::MayaType::Integer, "Must provide an integer value to assign to the given register!"));
            try {
                auto theValue = syn::extractLong<word>(env, value);
                auto rind = static_cast<byte>(ind);
                switch(space) {
                    case TargetSpace::GPR:
                        gpr[rind] = theValue;
                        return syn::setClipsBoolean(ret);
                    case TargetSpace::Predicates:
                        setPredicateRegister(rind, theValue != 0);
                        return syn::setClipsBoolean(ret);
                    default:
                        return CoreWrapper::callErrorCode4(env, ret, "illegal space specified for assigning registers!");
                }
            } catch(const syn::Problem& p) {
                return CoreWrapper::callErrorCode4(env, ret, p.what());
            }
		};
		auto readMemory = [this, env, ret](TargetSpace space) noexcept {
            if (registerSpaceOrNone(space)) {
                return CoreWrapper::callErrorCode4(env, ret, "illegal space specified for performing a read from memory");
            }
			CLIPSValue index;
            __RETURN_FALSE_ON_FALSE__(CoreWrapper::tryExtractArgument1(env, ret, &index, syn::MayaType::Integer, "Must provide an integer index to retrieve a memory value!"));
            try {
                auto address = syn::extractLong<word>(env, index);
                switch(space) {
                    case TargetSpace::Data:
                        CVSetInteger(ret, data[address]);
                        break;
                    case TargetSpace::Stack:
                        CVSetInteger(ret, stack[address]);
                        break;
                    case TargetSpace::Code:
                        CVSetInteger(ret, instruction[address]);
                        break;
                    case TargetSpace::IO:
						CVSetInteger(ret, readFromBus(address));
                        break;
                    default:
                        return CoreWrapper::callErrorCode4(env, ret, "Unimplemented target space found!");
                }
            } catch(const syn::Problem& p) {
                return CoreWrapper::callErrorCode4(env, ret, p.what());
            }
			return true;
		};
		auto writeMemory = [this, env, ret](TargetSpace space) noexcept {
            if (registerSpaceOrNone(space)) {
                return CoreWrapper::callErrorCode4(env, ret, "illegal space specified for performing a write to memory");
            }
			CLIPSValue index;
            __RETURN_FALSE_ON_FALSE__(CoreWrapper::tryExtractArgument1(env, ret, &index, syn::MayaType::Integer, "Must provide an integer index to assign a register value!"));
            CLIPSValue value;
            __RETURN_FALSE_ON_FALSE__(CoreWrapper::tryExtractArgument2(env, ret, &value, syn::MayaType::Integer, "Must provide an integer value to assign to the given register!"));
            try {
			    auto ind = syn::extractLong<word>(env, index);
                auto valueToWrite = syn::extractLong(env, value);
                switch(space) {
                    case TargetSpace::Code:
                        instruction[ind] = static_cast<raw_instruction>(valueToWrite);
                        break;
                    case TargetSpace::Data:
                        data[ind] = static_cast<word>(valueToWrite);
                        break;
                    case TargetSpace::Stack:
                        stack[ind] = static_cast<word>(valueToWrite);
                        break;
                    case TargetSpace::IO:
                        writeToBus(ind, valueToWrite);
                        break;
                    default:
                        return CoreWrapper::callErrorCode4(env, ret, "Unimplemented target space found!");
                }
                return syn::setClipsBoolean(ret);
            } catch(const syn::Problem& p) {
                return CoreWrapper::callErrorCode4(env, ret, p.what());
            }
		};
		CVSetBoolean(ret, true);
		try {
			switch(fop) {
				case WrappedOp::GetRegister:
                case WrappedOp::GetPredicateRegister:
					return getRegister(space);
				case WrappedOp::SetRegister:
                case WrappedOp::SetPredicateRegister:
					return setRegister(space);
                case WrappedOp::ReadDataMemory:
                case WrappedOp::ReadIOMemory:
                case WrappedOp::ReadCodeMemory:
                case WrappedOp::ReadStackMemory:
                    return readMemory(space);
                case WrappedOp::WriteDataMemory:
                case WrappedOp::WriteIOMemory:
                case WrappedOp::WriteCodeMemory:
                case WrappedOp::WriteStackMemory:
                    return writeMemory(space);
				default:
					return CoreWrapper::callErrorMessageCode3(env, ret, opStr, " <- legal but unimplemented operation!");
			}
			return true;
		} catch(const syn::Problem& p) {
            return CoreWrapper::callErrorCode2(env, ret, p.what());
		}
	}
} // end namespace iris

namespace syn {
	DefWrapperSymbolicName(iris::Core,  "iris-core");
    DefExternalAddressWrapperType(iris::Core, iris::CoreWrapper);
} // end namespace syn

namespace syn {
	namespace WrappedNewCallBuilder {
		template<>
		iris::Core* invokeNewFunction<iris::Core>(void* env, CLIPSValuePtr ret, const std::string& funcErrorPrefix, const std::string& function) noexcept {
			return syn::newCore<iris::Core>(env, ret, funcErrorPrefix, function);
		}
	} // end namespace WrappedNewCallBuilder
} // end namespace syn

