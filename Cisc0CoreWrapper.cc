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
#include "CoreWrapper.h"


namespace cisc0 {
    class CoreWrapper : public syn::CoreWrapper<Core> {
        public:
            using Parent = syn::CoreWrapper<Core>;
			enum class Operations {
                __DEFAULT_CORE_OPERATIONS__,
				WriteMemory,
				ReadMemory,
				GetRegister,
				SetRegister,
                __DEFAULT_ERROR_STATE__,
			};
        public:
            using Parent::Parent;
    };

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
		if (init) {
			init = false;
			auto functions = syn::retrieveFunctionNames<Core>("call");
			funcStr = std::get<1>(functions);
			funcErrorPrefix = std::get<2>(functions);
		}
        auto badArgument = [env, ret](auto code, auto msg) {
            CVSetBoolean(ret, false);
            return syn::errorMessage(env, "CALL", code, funcErrorPrefix, msg);
        };
		auto callErrorMessage = [badArgument](const std::string& subOp, const std::string& rest) {
			std::stringstream stm;
			stm << " " << subOp << ": " << rest << std::endl;
			auto msg = stm.str();
            return badArgument(3, msg);
		};
		CLIPSValue operation;
        if (!syn::tryGetArgumentAsSymbol(env, funcStr, 2, &operation)) {
            return badArgument(2, "expected a function name to call!");
		}
		std::string opStr(syn::extractLexeme(env, operation));
		auto result = ops.find(opStr);
		if (result == ops.end()) {
			return callErrorMessage(opStr, " <- unknown operation requested!");
		}
		WrappedOp fop;
		int argCount;
		std::tie(fop, argCount) = result->second;
		auto aCount = 2 + argCount;
        if (!syn::hasCorrectArgCount(env, aCount)) {
			return callErrorMessage(opStr, " too many arguments provided!");
		}
		auto getRegister = [this, env, ret, badArgument]() {
			CLIPSValue index;
			if (!syn::tryGetArgumentAsInteger(env, funcStr, 3, &index)) {
                return badArgument(3, "Must provide an integer index to retrieve a register value!");
			}
			auto i = syn::extractLong(env, index);
			if (i >= ArchitectureConstants::RegisterCount || i < 0) {
                return badArgument(3, "Illegal register index!");
			}
			CVSetInteger(ret, registerValue(static_cast<byte>(i)));
			return true;
		};
		auto setRegister = [this, env, ret, badArgument]() {
			CLIPSValue index, value;
			if (!syn::tryGetArgumentAsInteger(env, funcStr, 3, &index)) {
                return badArgument(3, "Must provide an integer index to assign a register value!");
			}
			auto ind = syn::extractLong(env, index);
			if (ind >= ArchitectureConstants::RegisterCount || ind < 0) {
                return badArgument(3, "Illegal register index!");
			}
            if (!syn::tryGetArgumentAsInteger(env, funcStr, 4, &value)) {
                return badArgument(3, "Must provide an integer value to assign to the given register!");
			}
			registerValue(static_cast<byte>(ind)) = syn::extractLong<RegisterValue>(env, value);
			CVSetBoolean(ret, true);
			return true;
		};
		auto readMemory = [this, env, ret, badArgument]() {
			CLIPSValue index;
			if (!syn::tryGetArgumentAsInteger(env, funcStr, 3, &index)) {
                return badArgument(3, "Must provide an integer index to retrieve a memory value!");
			}
			CVSetInteger(ret, loadWord(syn::extractLong<RegisterValue>(env, index)));
			return true;
		};
		auto writeMemory = [this, env, ret, badArgument]() {
			CLIPSValue index, value;
			if (!syn::tryGetArgumentAsInteger(env, funcStr, 3, &index)) {
                return badArgument(3, "Must provide an integer index to assign a register value!");
			}
			auto ind = syn::extractLong<Address>(env, index);
            if (!syn::tryGetArgumentAsInteger(env, funcStr, 4, &value)) {
                return badArgument(3, "Must provide an integer value to assign to the given register!");
			}
			storeWord(ind, syn::extractLong<Word>(env, value));
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
					return callErrorMessage(opStr, " <- legal but unimplemented operation!");
			}
			return true;
		} catch(const syn::Problem& p) {
            return badArgument(2, p.what());
		}
	}
	Core* Core::make() noexcept {
		return new Core();
	}
} // end namespace cisc0

namespace syn {
	DefWrapperSymbolicName(cisc0::Core,  "cisc0-core");
    DefExternalAddressWrapperType(cisc0::Core, cisc0::CoreWrapper);
} // end namespace syn

