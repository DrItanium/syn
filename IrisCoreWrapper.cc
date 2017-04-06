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


namespace iris {
	class CoreWrapper : public syn::ExternalAddressWrapper<Core> {
		public:
			using Parent = syn::ExternalAddressWrapper<Core>;
			using Self = CoreWrapper;
			enum class Operations {
				Initialize,
				Shutdown,
				Run,
				Cycle,
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
				Count,
			};
		public:
			static bool callFunction(void* env, syn::DataObjectPtr value, syn::DataObjectPtr ret);
			static void registerWithEnvironment(void* env, const char* title) {
				Parent::registerWithEnvironment(env, title, callFunction);
			}
			static void registerWithEnvironment(void* env) {
				static bool init = true;
				static std::string func;
				if (init) {
					init = false;
					func = Self::getType();
				}
				registerWithEnvironment(env, func.c_str());
			}
		public:
            CoreWrapper(Core* core) : Parent(core), _testValue(127) { }
			CoreWrapper() : Parent(new Core()), _testValue(128) { }
			virtual ~CoreWrapper() { }
		private:
			void initialize() { get()->initialize(); }
			void shutdown() { get()->shutdown(); }
			void run() { get()->run(); }
			void cycle() { get()->cycle(); }
			void haveCorePerformInternalAction(void* env, CLIPSValue* ret, Operations op);
        private:
            int _testValue;
	};
} // end namespace iris

namespace iris {
	bool CoreWrapper::callFunction(void* env, syn::DataObjectPtr value, syn::DataObjectPtr ret) {
		// unpack the object and do the magic
		static bool init = true;
		static std::string funcErrorPrefix;
		if (init) {
			init = false;
			auto functions = syn::retrieveFunctionNames<Core>("call");
			funcErrorPrefix = std::get<2>(functions);
		}
		if (GetpType(value) != EXTERNAL_ADDRESS) {
			return syn::errorMessage(env, "CALL", 1, funcErrorPrefix, "Function call expected an external address as the first argument!");
		}
        auto ptr = static_cast<CoreWrapper*>(EnvDOPToExternalAddress(value));
		return ptr->get()->handleOperation(env, ret);
	}
	void installCoreWrapper(void* env) {
		CoreWrapper::registerWithEnvironment(env);
	}
	bool Core::handleOperation(void* env, CLIPSValue* ret) {
		static bool init = true;
		static std::string funcStr;
		static std::string funcErrorPrefix;
        enum class TargetSpace {
            None,
            Code,
            Data,
            Stack,
            IO,
            GPR,
            Predicates,
        };
		using OpToArgCount = std::tuple<CoreWrapper::Operations, int, TargetSpace>;
		using WrappedOp = CoreWrapper::Operations;
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
		auto callErrorMessage = [env, ret](const std::string& subOp, const std::string& rest) {
			CVSetBoolean(ret, false);
			std::stringstream stm;
			stm << " " << subOp << ": " << rest << std::endl;
			auto msg = stm.str();
			return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, msg);
		};
		if (init) {
			init = false;
			auto functions = syn::retrieveFunctionNames<Core>("call");
			funcStr = std::get<1>(functions);
			funcErrorPrefix = std::get<2>(functions);
		}
		CLIPSValue operation;
		if (!EnvArgTypeCheck(env, funcStr.c_str(), 2, SYMBOL, &operation)) {
			return syn::errorMessage(env, "CALL", 2, funcErrorPrefix, "expected a function name to call!");
		}
		std::string opStr(EnvDOToString(env, operation));
		auto result = ops.find(opStr);
		if (result == ops.end()) {
			CVSetBoolean(ret, false);
			return callErrorMessage(opStr, " <- unknown operation requested!");
		}
		WrappedOp fop;
		int argCount;
        TargetSpace space;
		std::tie(fop, argCount, space) = result->second;
		auto aCount = 2 + argCount;
		if (aCount != EnvRtnArgCount(env)) {
			CVSetBoolean(ret, false);
			return callErrorMessage(opStr, " too many arguments provided!");
		}
		auto getRegister = [this, env, ret, callErrorMessage](TargetSpace space) {
            if (space != TargetSpace::GPR && space != TargetSpace::Predicates) {
                CVSetBoolean(ret, false);
                return syn::errorMessage(env, "CALL", 4, funcErrorPrefix, "Illegal space provided for retrieving a register from!");
            }
			CLIPSValue index;
			if (!EnvArgTypeCheck(env, funcStr.c_str(), 3, INTEGER, &index)) {
				CVSetBoolean(ret, false);
				return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "Must provide an integer index to retrieve a register value!");
			}
			auto i = EnvDOToLong(env, index);
            if (i < 0) {
                CVSetBoolean(ret, false);
                return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "Was given a negative register index!");
            }
            if (space == TargetSpace::GPR && i >= ArchitectureConstants::RegisterCount) {
				CVSetBoolean(ret, false);
				return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "Illegal register index!");
			}
            if (space == TargetSpace::Predicates && i >= ArchitectureConstants::ConditionRegisterCount) {
				CVSetBoolean(ret, false);
				return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "Illegal condition register index!");
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
                        return syn::errorMessage(env, "CALL", 4, funcErrorPrefix, "illegal space specified for retrieving registers!");
                }
            } catch(syn::Problem p) {
                return syn::errorMessage(env, "CALL", 4, funcErrorPrefix, p.what());
            }
		};
		auto setRegister = [this, env, ret, callErrorMessage](TargetSpace space) {
            if (space != TargetSpace::GPR && space != TargetSpace::Predicates) {
                CVSetBoolean(ret, false);
                return syn::errorMessage(env, "CALL", 4, funcErrorPrefix, "Illegal space provided for setting a register!");
            }
			CLIPSValue index, value;
			if (!EnvArgTypeCheck(env, funcStr.c_str(), 3, INTEGER, &index)) {
				CVSetBoolean(ret, false);
				return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "Must provide an integer index to assign a register value!");
			}
			auto ind = EnvDOToLong(env, index);
            if (ind < 0) {
                CVSetBoolean(ret, false);
                return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "Was given a negative address!");
            }
			if (space == TargetSpace::GPR && ind >= ArchitectureConstants::RegisterCount)  {
				CVSetBoolean(ret, false);
				return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "Illegal register index!");
			}
			if (space == TargetSpace::Predicates && ind >= ArchitectureConstants::ConditionRegisterCount)  {
				CVSetBoolean(ret, false);
				return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "Illegal condition register index!");
			}
			if(!EnvArgTypeCheck(env, funcStr.c_str(), 4, INTEGER, &value)) {
				CVSetBoolean(ret, false);
				return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "Must provide an integer value to assign to the given register!");
			}
            try {
                auto theValue = EnvDOToLong(env, value);
                switch(space) {
                    case TargetSpace::GPR:
                        gpr[static_cast<byte>(ind)] = static_cast<word>(theValue);
                        CVSetBoolean(ret, true);
                        return true;
                    case TargetSpace::Predicates:
                        getPredicateRegister(static_cast<byte>(ind)) = static_cast<word>(theValue);
                        CVSetBoolean(ret, true);
                        return true;
                    default:
                        return syn::errorMessage(env, "CALL", 4, funcErrorPrefix, "illegal space specified for assigning registers!");
                }
            } catch(syn::Problem p) {
                CVSetBoolean(ret, false);
                return syn::errorMessage(env, "CALL", 4, funcErrorPrefix, p.what());
            }
		};
		auto readMemory = [this, env, ret, callErrorMessage](TargetSpace space) {
            switch(space) {
                case TargetSpace::None:
                case TargetSpace::GPR:
                case TargetSpace::Predicates:
                    CVSetBoolean(ret, false);
                    return syn::errorMessage(env, "CALL", 4, funcErrorPrefix, "illegal space specified for performing a read from memory");
                default:
                    break;
            }
			CLIPSValue index;
			if (!EnvArgTypeCheck(env, funcStr.c_str(), 3, INTEGER, &index)) {
				CVSetBoolean(ret, false);
				return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "Must provide an integer index to retrieve a memory value!");
			}
            try {
                auto address = static_cast<word>(EnvDOToLong(env, index));
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
                        CVSetBoolean(ret, false);
                        return syn::errorMessage(env, "CALL", 4, funcErrorPrefix, "Unimplemented target space found!");
                }
            } catch(syn::Problem p) {
                CVSetBoolean(ret, false);
                return syn::errorMessage(env, "CALL", 4, funcErrorPrefix, p.what());
            }
			return true;
		};
		auto writeMemory = [this, env, ret, callErrorMessage](TargetSpace space) {
            switch(space) {
                case TargetSpace::None:
                case TargetSpace::GPR:
                case TargetSpace::Predicates:
                    CVSetBoolean(ret, false);
                    return syn::errorMessage(env, "CALL", 4, funcErrorPrefix, "illegal space specified for performing a write to memory");
                default:
                    break;
            }
			CLIPSValue index, value;
			if (!EnvArgTypeCheck(env, funcStr.c_str(), 3, INTEGER, &index)) {
				CVSetBoolean(ret, false);
				return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "Must provide an integer index to assign a register value!");
			}
			auto ind = static_cast<word>(EnvDOToLong(env, index));
			if(!EnvArgTypeCheck(env, funcStr.c_str(), 4, INTEGER, &value)) {
				CVSetBoolean(ret, false);
				return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "Must provide an integer value to assign to the given register!");
			}
            try {
                auto valueToWrite = EnvDOToLong(env, value);
                std::cout << "value to write " << valueToWrite << std::endl;
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
                        CVSetBoolean(ret, false);
                        return syn::errorMessage(env, "CALL", 4, funcErrorPrefix, "Unimplemented target space found!");
                }
            } catch(syn::Problem p) {
                CVSetBoolean(ret, false);
                return syn::errorMessage(env, "CALL", 4, funcErrorPrefix, p.what());
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
					CVSetBoolean(ret, false);
					return callErrorMessage(opStr, " <- legal but unimplemented operation!");
			}
			return true;
		} catch(syn::Problem p) {
			CVSetBoolean(ret, false);
			return syn::errorMessage(env, "CALL", 2, funcErrorPrefix, p.what());
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

