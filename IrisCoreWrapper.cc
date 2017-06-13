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

#include "IrisCoreWrapper.h"
#include "IrisCore.h"
#include "ClipsExtensions.h"
#include "CoreWrapper.h"


namespace iris {
    class CoreWrapper : public syn::CoreWrapper<Core> {
        public:
            using Parent = syn::CoreWrapper<Core>;
			enum class Operations {
                __DEFAULT_CORE_OPERATIONS__,
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
    };
	void installCoreWrapper(void* env) {
		CoreWrapper::registerWithEnvironment(env);
	}
	bool Core::handleOperation(void* env, CLIPSValue* ret) {
        using TargetSpace = CoreWrapper::TargetSpace;
		using WrappedOp = CoreWrapper::Operations;
		using OpToArgCount = std::tuple<WrappedOp, int, TargetSpace>;
		static std::map<std::string, OpToArgCount> ops = {
			{ "initialize", std::make_tuple( WrappedOp::Initialize, 0, TargetSpace::None )},
			{ "shutdown", std::make_tuple(WrappedOp::Shutdown, 0, TargetSpace::None)},
			{ "run", std::make_tuple(WrappedOp::Run, 0, TargetSpace::None)},
			{ "cycle", std::make_tuple(WrappedOp::Cycle, 0, TargetSpace::None) },
			{ "write-data-memory", std::make_tuple(WrappedOp::WriteDataMemory, 2, TargetSpace::Code) },
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
        if (!syn::tryGetArgumentAsSymbolFromCall<Core>(env, &operation, 2)) {
            return syn::callErrorCode2<Core>(env, ret, "expected a function name to call!");
		}
		std::string opStr(syn::extractLexeme(env, operation));
		auto result = ops.find(opStr);
		if (result == ops.end()) {
			return syn::callErrorMessage<Core>(env, ret, 3, opStr, " <- unknown operation requested!");
		}
		WrappedOp fop;
		int argCount;
        TargetSpace space;
		std::tie(fop, argCount, space) = result->second;
		auto aCount = 2 + argCount;
        if (!syn::hasCorrectArgCount(env, aCount)) {
			return syn::callErrorMessage<Core>(env, ret, 3, opStr, " too many arguments provided!");
		}
		auto getRegister = [this, env, ret](TargetSpace space) noexcept {
            if (space != TargetSpace::GPR && space != TargetSpace::Predicates) {
                return syn::callErrorCode4<Core>(env, ret, "Illegal space provided for retrieving a register from!");
            }
			CLIPSValue index;
            if (!syn::tryGetArgumentAsIntegerFromCall<Core>(env, &index, 3)) {
                return syn::callErrorCode3<Core>(env, ret, "Must provide an integer index to retrieve a register value!");
			}
			auto i = syn::extractLong(env, index);
            if (i < 0) {
                return syn::callErrorCode3<Core>(env, ret, "Was given a negative register index!");
            }
            if (space == TargetSpace::GPR && i >= ArchitectureConstants::RegisterCount) {
                return syn::callErrorCode3<Core>(env, ret, "Illegal register index!");
			}
            if (space == TargetSpace::Predicates && i >= ArchitectureConstants::ConditionRegisterCount) {
                return syn::callErrorCode3<Core>(env, ret, "Illegal condition register index!");
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
                        return syn::callErrorCode4<Core>(env, ret, "illegal space specified for retrieving registers!");
                }
            } catch(const syn::Problem& p) {
                return syn::callErrorCode4<Core>(env, ret, p.what());
            }
		};
		auto setRegister = [this, env, ret](TargetSpace space) noexcept {
            if (space != TargetSpace::GPR && space != TargetSpace::Predicates) {
                return syn::callErrorCode4<Core>(env, ret, "Illegal space provided for setting a register!");
            }
			CLIPSValue index, value;
            if (!syn::tryGetArgumentAsIntegerFromCall<Core>(env, &index, 3)) {
                return syn::callErrorCode3<Core>(env, ret, "Must provide an integer index to assign a register value!");
			}
			auto ind = syn::extractLong(env, index);
            if (ind < 0) {
                return syn::callErrorCode3<Core>(env, ret, "Was given a negative address!");
            }
			if (space == TargetSpace::GPR && ind >= ArchitectureConstants::RegisterCount)  {
                return syn::callErrorCode3<Core>(env, ret, "Illegal register index!");
			}
			if (space == TargetSpace::Predicates && ind >= ArchitectureConstants::ConditionRegisterCount)  {
                return syn::callErrorCode3<Core>(env, ret, "Illegal condition register index!");
			}
            if (!syn::tryGetArgumentAsIntegerFromCall<Core>(env, &value, 4)) {
                return syn::callErrorCode3<Core>(env, ret, "Must provide an integer value to assign to the given register!");
			}
            try {
                auto theValue = syn::extractLong<word>(env, value);
                auto rind = static_cast<byte>(ind);
                switch(space) {
                    case TargetSpace::GPR:
                        gpr[rind] = theValue;
                        CVSetBoolean(ret, true);
                        return true;
                    case TargetSpace::Predicates:
                        setPredicateRegister(rind, theValue);
                        CVSetBoolean(ret, true);
                        return true;
                    default:
                        return syn::callErrorCode4<Core>(env, ret, "illegal space specified for assigning registers!");
                }
            } catch(const syn::Problem& p) {
                return syn::callErrorCode4<Core>(env, ret, p.what());
            }
		};
		auto readMemory = [this, env, ret](TargetSpace space) noexcept {
            switch(space) {
                case TargetSpace::None:
                case TargetSpace::GPR:
                case TargetSpace::Predicates:
                    return syn::callErrorCode4<Core>(env, ret, "illegal space specified for performing a read from memory");
                default:
                    break;
            }
			CLIPSValue index;
            if (!syn::tryGetArgumentAsIntegerFromCall<Core>(env, &index, 3)) {
                return syn::callErrorCode3<Core>(env, ret, "Must provide an integer index to retrieve a memory value!");
			}
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
                        CVSetInteger(ret, ioSpaceRead(address));
                        break;
                    default:
                        return syn::callErrorCode4<Core>(env, ret, "Unimplemented target space found!");
                }
            } catch(const syn::Problem& p) {
                return syn::callErrorCode4<Core>(env, ret, p.what());
            }
			return true;
		};
		auto writeMemory = [this, env, ret](TargetSpace space) noexcept {
            switch(space) {
                case TargetSpace::None:
                case TargetSpace::GPR:
                case TargetSpace::Predicates:
                    return syn::callErrorCode4<Core>(env, ret, "illegal space specified for performing a write to memory");
                default:
                    break;
            }
			CLIPSValue index;
            if (!syn::tryGetArgumentAsIntegerFromCall<Core>(env, &index, 3)) {
                return syn::callErrorCode3<Core>(env, ret, "Must provide an integer index to assign a register value!");
			}
            CLIPSValue value;
            if (!syn::tryGetArgumentAsIntegerFromCall<Core>(env, &value, 4)) {
                return syn::callErrorCode3<Core>(env, ret, "Must provide an integer value to assign to the given register!");
			}
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
                        ioSpaceWrite(ind, valueToWrite);
                        break;
                    default:
                        return syn::callErrorCode4<Core>(env, ret, "Unimplemented target space found!");
                }
            } catch(const syn::Problem& p) {
                return syn::callErrorCode4<Core>(env, ret, p.what());
            }
			CVSetBoolean(ret, true);
			return true;
		};
		CVSetBoolean(ret, true);
		try {
			switch(fop) {
				case WrappedOp::Initialize:
					initialize();
					break;
				case WrappedOp::Shutdown:
					shutdown();
					break;
				case WrappedOp::Run:
					run();
					break;
				case WrappedOp::Cycle:
					CVSetBoolean(ret, cycle());
					break;
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
					return syn::callErrorMessage<Core>(env, ret, 3, opStr, " <- legal but unimplemented operation!");
			}
			return true;
		} catch(const syn::Problem& p) {
            return syn::callErrorCode2<Core>(env, ret, p.what());
		}
	}
	Core* Core::make() noexcept {
		return new Core();
	}
} // end namespace iris

namespace syn {
	DefWrapperSymbolicName(iris::Core,  "iris-core");
    DefExternalAddressWrapperType(iris::Core, iris::CoreWrapper);
} // end namespace syn

