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


#ifndef IRIS_CORE_ASSEMBLER_WRAPPER_H__
#define IRIS_CORE_ASSEMBLER_WRAPPER_H__

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

namespace iris {
	struct AssemblerState;
    class AssemblerStateWrapper : public syn::ExternalAddressWrapper<AssemblerState> {

        public:
            using Self = AssemblerStateWrapper;
            using Parent = syn::ExternalAddressWrapper<AssemblerState>;
            static Self* make() noexcept { return new Self(); }
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
            static bool callFunction(void* env, syn::DataObjectPtr value, syn::DataObjectPtr ret) {
                static bool init = true;
                static std::string funcStr;
                static std::string funcErrorPrefix;
                using MapOpToCount = std::tuple<Operations, int>;
                static std::map<std::string, MapOpToCount> ops = {
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
                    auto result = ptr->parseLine(str);
                    CVSetBoolean(ret, result);
                    if (!result) {
                        Parent::callErrorMessageCode3(env, ret, "parse", "error during parsing!");
                    }
                    return result;
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
                        return callErrorMessage(str, "<- unimlemented operation!!!!");
                }
                return false;
            }
        public:
            enum Operations {
                Parse,
                Resolve,
                Get,
                Count,
            };
         public:
            AssemblerStateWrapper() : Parent(std::move(std::make_unique<AssemblerState>())) { }
            bool parseLine(const std::string& line);
            bool resolve();
            void getMultifield(void* env, CLIPSValuePtr ret);
         private:
            void output(void* env, CLIPSValue* ret) noexcept;
    };

} // end namespace iris
#endif // end IRIS_CORE_ASSEMBLER_H__
