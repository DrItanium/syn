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


// Cisc0CoreAssembler rewritten to use pegtl
#include "Cisc0ClipsExtensions.h"
#include "Cisc0CoreAssembler.h"
#include "Cisc0CoreAssemblerWrapper.h"

namespace cisc0 {

	void installAssemblerParsingState(void* env) {
		// AssemblerState needs to be an external address and we can have
		// multiple assembler states sitting around too!
		pegtl::analyze<cisc0::Main>();
		// make sure that the parser is still valid before we go any further!
		AssemblerStateWrapper::registerWithEnvironment(env);
		AssemblerStateWrapper::registerWithEnvironment(env, "cisc0-asm-parser");
		AssemblerStateWrapper::registerWithEnvironment(env, "cisc0-assembler");
	}
    void AssemblerState::output(void* env, CLIPSValue* ret) noexcept {
        // we need to build a multifield out of the finalWords
        syn::MultifieldBuilder f(env, finalWords.size() * 2);
        int i = 1;
        for (auto q : finalWords) {
            // add them two at a time!
            f.setField(i, INTEGER, EnvAddLong(env, q.getAddress()));
            f.setField(i + 1, INTEGER, EnvAddLong(env, q.getValue()));
            i += 2;
        }
        f.assign(ret);
    }
	AssemblerStateWrapper* AssemblerStateWrapper::make() noexcept {
		return new AssemblerStateWrapper();
	}
	void AssemblerStateWrapper::getMultifield(void* env, CLIPSValuePtr ret) {
		get()->output(env, ret);
	}
	bool AssemblerStateWrapper::resolve() {
		get()->resolveDeclarations();
		get()->resolveInstructions();
		return true;
	}
	bool AssemblerStateWrapper::parseLine(const std::string& line) {
		auto& ref = *(get());
		return pegtl::parse_string<cisc0::Main, cisc0::Action>(line, "clips-input", ref);
	}


	void AssemblerStateWrapper::registerWithEnvironment(void* env, const char* title) {
		AssemblerStateWrapper::Parent::registerWithEnvironment(env, title, callFunction);
	}
	void AssemblerStateWrapper::registerWithEnvironment(void* env) {
		static bool init = true;
		static std::string func;
		if (init) {
			init = false;
			func = Self::getType();
		}
		registerWithEnvironment(env, func.c_str());
	}
	bool AssemblerStateWrapper::callFunction(void* env, syn::DataObjectPtr value, syn::DataObjectPtr ret) {
		using Operations = AssemblerStateWrapper::Operations;
		static bool init = true;
		static std::string funcStr;
		static std::string funcErrorPrefix;
        using MapOpToArgCount = std::tuple<Operations, int>;
		static std::map<std::string, MapOpToArgCount> ops = {
			{ "parse", std::make_tuple(Operations::Parse, 1) },
			{ "resolve", std::make_tuple(Operations::Resolve, 0) },
			{ "get", std::make_tuple(Operations::Get, 0) },
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
			auto functions = syn::retrieveFunctionNames<AssemblerState>("call");
			funcStr = std::get<1>(functions);
			funcErrorPrefix = std::get<2>(functions);
		}
		if (GetpType(value) != EXTERNAL_ADDRESS) {
			return syn::errorMessage(env, "CALL", 1, funcErrorPrefix, "Function call expected an external address as the first argument!");
		}
		CLIPSValue operation;
		if (!EnvArgTypeCheck(env, funcStr.c_str(), 2, SYMBOL, &operation)) {
			return syn::errorMessage(env, "CALL", 2, funcErrorPrefix, "expected a function name to call!");
		}
		std::string str(EnvDOToString(env, operation));
		auto result = ops.find(str);
		if (result == ops.end()) {
			CVSetBoolean(ret, false);
			return callErrorMessage(str, " <- unknown operation requested!");
		}
        Operations theOp;
        int argCount;
        std::tie(theOp, argCount) = result->second;
		auto aCount = 2 + argCount;
		if (aCount != EnvRtnArgCount(env)) {
			CVSetBoolean(ret, false);
			return callErrorMessage(str, " too many arguments provided!");
		}
		auto ptr = static_cast<Self*>(DOPToExternalAddress(value));
		auto parseLine = [env, ret, ptr]() {
			CLIPSValue line;
			if (!EnvArgTypeCheck(env, funcStr.c_str(), 3, STRING, &line)) {
				CVSetBoolean(ret, false);
				return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "provided assembly line is not a string!");
			}
			std::string str(EnvDOToString(env, line));
			auto result = ptr->parseLine(str);
			CVSetBoolean(ret, result);
			if (!result) {
				syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "parse: error during parsing!");
			}
			return result;
		};
		auto resolve = [env, ret, ptr]() {
			try {
				CVSetBoolean(ret, true);
				return ptr->resolve();
			} catch (syn::Problem p) {
				CVSetBoolean(ret, false);
				std::stringstream ss;
				ss << "error during resolve: " << p.what();
				auto str = ss.str();
				return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, str);
			}
		};
		switch(theOp) {
			case Operations::Parse:
				return parseLine();
			case Operations::Resolve:
				return resolve();
			case Operations::Get:
				ptr->getMultifield(env, ret);
				return true;
			default:
				CVSetBoolean(ret, false);
				return callErrorMessage(str, "<- unimlemented operation!!!!");
		}
		return false;
	}
	AssemblerStateWrapper::AssemblerStateWrapper() : Parent(std::move(std::make_unique<AssemblerState>())) { }
}
