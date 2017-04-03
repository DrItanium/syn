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

#include "Cisc0CoreWrapper.h"
#include "Cisc0Core.h"
#include "ClipsExtensions.h"


namespace cisc0 {
	class CoreWrapper : public syn::ExternalAddressWrapper<Core> {
		public:
			using Parent = syn::ExternalAddressWrapper<Core>;
			using Self = CoreWrapper;
			enum class Operations {
				Initialize,
				Shutdown,
				Run,
				Cycle,
				WriteMemory,
				ReadMemory,
				GetRegister,
				SetRegister,
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
			CoreWrapper() : Parent(std::move(std::make_unique<Core>())) { }
			virtual ~CoreWrapper() { }
		private:
			void initialize() { get()->initialize(); }
			void shutdown() { get()->shutdown(); }
			void run() { get()->run(); }
			void cycle() { get()->cycle(); }
			void haveCorePerformInternalAction(void* env, CLIPSValue* ret, Operations op);
	};
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
		auto ptr = syn::unwrapExternalAddress<Self>(value);
		return ptr->get()->handleOperation(env, ret);
	}
	void installCoreWrapper(void* env) {
		CoreWrapper::registerWithEnvironment(env);
	}
	bool Core::handleOperation(void* env, CLIPSValue* ret) {
		static bool init = true;
		static std::string funcStr;
		static std::string funcErrorPrefix;
		using OpToArgCount = std::tuple<CoreWrapper::Operations, int>;
		using WrappedOp = CoreWrapper::Operations;
		static std::map<std::string, OpToArgCount> ops = {
			{ "initialize", std::make_tuple( WrappedOp::Initialize, 0 )},
			{ "shutdown", std::make_tuple(WrappedOp::Shutdown, 0 )},
			{ "run", std::make_tuple(WrappedOp::Run, 0 )},
			{ "cycle", std::make_tuple(WrappedOp::Cycle, 0) },
			{ "write-memory", std::make_tuple(WrappedOp::WriteMemory, 2 ) },
			{ "read-memory", std::make_tuple(WrappedOp::ReadMemory, 1 ) },
			{ "get-register", std::make_tuple(WrappedOp::GetRegister, 1 ) },
			{ "set-register", std::make_tuple(WrappedOp::SetRegister, 2 ) },
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
		std::tie(fop, argCount) = result->second;
		auto aCount = 2 + argCount;
		if (aCount != EnvRtnArgCount(env)) {
			CVSetBoolean(ret, false);
			return callErrorMessage(opStr, " too many arguments provided!");
		}
		auto getRegister = [this, env, ret, callErrorMessage]() {
			CLIPSValue index;
			if (!EnvArgTypeCheck(env, funcStr.c_str(), 3, INTEGER, &index)) {
				CVSetBoolean(ret, false);
				return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "Must provide an integer index to retrieve a register value!");
			}
			auto i = CVToInteger(&index);
			if (i >= ArchitectureConstants::RegisterCount || i < 0) {
				CVSetBoolean(ret, false);
				return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "Illegal register index!");
			}
			CVSetInteger(ret, registerValue(static_cast<byte>(i)));
			return true;
		};
		auto setRegister = [this, env, ret, callErrorMessage]() {
			CLIPSValue index, value;
			if (!EnvArgTypeCheck(env, funcStr.c_str(), 3, INTEGER, &index)) {
				CVSetBoolean(ret, false);
				return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "Must provide an integer index to assign a register value!");
			}
			auto ind = CVToInteger(&index);
			if (ind >= ArchitectureConstants::RegisterCount || ind < 0) {
				CVSetBoolean(ret, false);
				return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "Illegal register index!");
			}
			if(!EnvArgTypeCheck(env, funcStr.c_str(), 4, INTEGER, &value)) {
				CVSetBoolean(ret, false);
				return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "Must provide an integer value to assign to the given register!");
			}
			registerValue(static_cast<byte>(ind)) = static_cast<RegisterValue>(CVToInteger(&value));
			CVSetBoolean(ret, true);
			return true;
		};
		auto readMemory = [this, env, ret, callErrorMessage]() {
			CLIPSValue index;
			if (!EnvArgTypeCheck(env, funcStr.c_str(), 3, INTEGER, &index)) {
				CVSetBoolean(ret, false);
				return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "Must provide an integer index to retrieve a memory value!");
			}
			CVSetInteger(ret, loadWord(static_cast<RegisterValue>(CVToInteger(&index))));
			CVSetBoolean(ret, true);
			return true;
		};
		auto writeMemory = [this, env, ret, callErrorMessage]() {
			CLIPSValue index, value;
			if (!EnvArgTypeCheck(env, funcStr.c_str(), 3, INTEGER, &index)) {
				CVSetBoolean(ret, false);
				return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "Must provide an integer index to assign a register value!");
			}
			auto ind = static_cast<Address>(CVToInteger(&index));
			if(!EnvArgTypeCheck(env, funcStr.c_str(), 4, INTEGER, &value)) {
				CVSetBoolean(ret, false);
				return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "Must provide an integer value to assign to the given register!");
			}
			storeWord(ind, static_cast<Word>(CVToInteger(&value)));
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
					return getRegister();
				case WrappedOp::SetRegister:
					return setRegister();
				case WrappedOp::ReadMemory:
					return readMemory();
				case WrappedOp::WriteMemory:
					return writeMemory();
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
} // end namespace cisc0

namespace syn {
	DefWrapperSymbolicName(cisc0::CoreWrapper,  "cisc0-core");
} // end namespace syn

