/**
 * @file
 * Implementation of AssemblerStateWrapper
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


// IrisCoreAssembler rewritten to use pegtl
#include <sstream>
#include <typeinfo>
#include <iostream>
#include <map>
#include "Base.h"
#include "AssemblerBase.h"
#include "Problem.h"
#include "IrisCore.h"
#include <pegtl.hh>
#include <pegtl/analyze.hh>
#include <pegtl/file_parser.hh>
#include <pegtl/contrib/raw_string.hh>
#include <pegtl/contrib/abnf.hh>
#include <pegtl/parse.hh>
#include <vector>
#include "IrisClipsExtensions.h"
#include "ClipsExtensions.h"
#include "IrisCoreAssembler.h"
#include "IrisCoreAssemblerStateWrapper.h"

namespace iris {
    namespace assembler {
        class AssemblerState;
    } // end namespace assembler
} // end namespace iris
namespace syn {
    DefWrapperSymbolicName(iris::assembler::AssemblerState, "iris:assembly-parsing-state");
} // end namespace syn

namespace iris {
    void AssemblerStateWrapper::getMultifield(void* env, CLIPSValuePtr ret) {
        //get()->output(env, ret);
        output(env, ret);
    }
    bool AssemblerStateWrapper::resolve() {
        auto & state = *(get());
		auto resolveLabel = [&state](assembler::AssemblerData& data) {
			auto result = state.findLabel(data.currentLexeme);
			if (result == state.labelsEnd()) {
				std::stringstream msg;
				msg << "ERROR: label " << data.currentLexeme << " is undefined!" << std::endl;
				auto str = msg.str();
				throw syn::Problem(str);
			} else {
				return result->second;
			}
		};
        state.applyToFinishedData([resolveLabel](auto value) {
                    if (value.shouldResolveLabel()) {
                        auto result = resolveLabel(value);
                        if (value.instruction) {
                            value.setImmediate(result);
                        } else {
                            value.dataValue = result;
                        }
                    }
                });
        return true;
    }
    bool AssemblerStateWrapper::parseLine(const std::string& line) {
        auto& ref = *(get());
        return pegtl::parse_string<assembler::Main, assembler::Action>(line, "clips-input", ref);
    }
    void AssemblerStateWrapper::output(void* env, CLIPSValue* ret) noexcept {
        // we need to build a multifield out of the finalWords
        auto & state = *(get());
        syn::MultifieldBuilder f(env, state.numberOfFinishedItems() * 3);
        auto body = [&f, env, ret](auto value, auto baseIndex) noexcept {
            auto i = (3 * baseIndex) + 1;
            f.setField(i + 0, INTEGER, EnvAddLong(env, value.instruction ? 0 : 1));
            f.setField(i + 1, INTEGER, EnvAddLong(env, value.address));
            f.setField(i + 2, INTEGER, EnvAddLong(env, value.encode()));
        };
        state.applyToFinishedData(body);
        f.assign(ret);
    }

    void installAssemblerParsingState(void* env) {
        pegtl::analyze<iris::assembler::Main>();
        AssemblerStateWrapper::registerWithEnvironment(env);
        AssemblerStateWrapper::registerWithEnvironment(env, "iris-asm-parser");
        AssemblerStateWrapper::registerWithEnvironment(env, "iris-assembler");
    }
    bool AssemblerStateWrapper::callFunction(void* env, syn::DataObjectPtr value, syn::DataObjectPtr ret) {
        using MapOpToCount = std::tuple<Operations, int>;
        static std::map<std::string, MapOpToCount> ops = {
            { "parse", std::make_tuple(Operations::Parse, 1) },
            { "resolve", std::make_tuple(Operations::Resolve, 0) },
            { "get", std::make_tuple(Operations::Get, 0) },
        };

        __RETURN_FALSE_ON_FALSE__(Parent::isExternalAddress(env, ret, value));
        CLIPSValue operation;
        __RETURN_FALSE_ON_FALSE__(Parent::tryExtractFunctionName(env, ret, &operation));
        std::string str(syn::extractLexeme(env, operation));
        auto result = ops.find(str);
        __RETURN_FALSE_ON_FALSE__(Parent::isLegalOperation(env, ret, str, result, ops.end()));
        Operations theOp;
        int tArgCount;
        std::tie(theOp, tArgCount) = result->second;
        __RETURN_FALSE_ON_FALSE__(Parent::checkArgumentCount(env, ret, str, tArgCount));
        auto ptr = static_cast<Self*>(DOPToExternalAddress(value));
        auto parseLine = [env, ret, ptr]() {
            CLIPSValue line;
            __RETURN_FALSE_ON_FALSE__(Parent::tryExtractArgument1(env, ret, &line, syn::MayaType::String, "Must provide a string to parse!"));
            std::string str(syn::extractLexeme(env, line));
            try {
            auto result = ptr->parseLine(str);
            CVSetBoolean(ret, result);
            if (!result) {
                Parent::callErrorMessageCode3(env, ret, "parse", "error during parsing!");
            }
            return result;
            } catch(pegtl::basic_parse_error<pegtl::position_info>& ex) {

                std::string msg(ex.what());
                Parent::callErrorMessageCode3(env, ret, "parse", msg);
                return false;
            }
        };
        switch(theOp) {
            case Operations::Parse:
                return parseLine();
            case Operations::Resolve:
                return ptr->resolve();
                return true;
            case Operations::Get:
                ptr-> getMultifield(env, ret);
                return true;
            default:
                return Parent::callErrorMessageCode3(env, ret, str, "<- unimplemented operation!!!!");
        }
        return false;
    }
    void AssemblerStateWrapper::registerWithEnvironment(void* env, const char* title) {
        Parent::registerWithEnvironment(env, title, callFunction);
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
}
