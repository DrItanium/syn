/**
 * @file
 * Common assembler wrapper interface type, provides common functionality for
 * CLIPS <-> native interaction when parsing assembly
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


#ifndef __SYN_COMMON_ASSEMBLER_WRAPPER_H
#define __SYN_COMMON_ASSEMBLER_WRAPPER_H
#include "CommonExternalAddressWrapper.h"

namespace syn {

/**
 * Common super type for all assembler wrappers, provides the common call
 * function code for parsing results
 */
template<typename T>
class AssemblerWrapper : public CommonExternalAddressWrapper<T> {
	public:
		using Self = AssemblerWrapper<T>;
		using Parent = CommonExternalAddressWrapper<T>;
		/**
		 * Common assembler parsing operations
		 */
		enum Operations {
			Parse,
			Resolve,
			Get,
			Reset,
			Count,
		};
	public:
		using Parent::Parent;
		virtual ~AssemblerWrapper() { }
		virtual bool handleCallOperation(void* env, DataObjectPtr value, DataObjectPtr ret, const std::string& operation) override {
            static_assert(std::is_base_of<Self, typename ExternalAddressWrapperType<T>::TheType>::value, "To take advantage of this wrapper class, you must inherit from it and register that with the target type!");
			using Operations = Operations;
			using MapOpToArgCount = std::tuple<Operations, int>;
			static std::map<std::string, MapOpToArgCount> ops = {
				{ "parse", std::make_tuple(Operations::Parse, 1) },
				{ "resolve", std::make_tuple(Operations::Resolve, 0) },
				{ "get", std::make_tuple(Operations::Get, 0) },
				{ "reset", std::make_tuple(Operations::Reset, 0) },
			};
			auto result = ops.find(operation);
			__RETURN_FALSE_ON_FALSE__(Parent::isLegalOperation(env, ret, operation, result, ops.end()));
			Operations theOp;
			int argCount;
			std::tie(theOp, argCount) = result->second;
			__RETURN_FALSE_ON_FALSE__(Parent::checkArgumentCount(env, ret, operation, argCount));
			switch(theOp) {
				case Operations::Parse:
					return this->extractAndParseLine(env, ret);
				case Operations::Resolve:
					return this->resolveLabels(env, ret);
				case Operations::Get:
					this->getEncodedValues(env, ret);
					return true;
				case Operations::Reset:
					this->reset();
					return true;
				default:
					return this->parseCustomOperation(env, ret, operation);
			}
		}
		virtual bool parseLine(void* env, DataObjectPtr ret, const std::string& line) = 0;
		virtual bool resolve(void* env, DataObjectPtr ret) = 0;
		virtual void getEncodedValues(void* env, DataObjectPtr ret) = 0;
		virtual void reset() {
			this->get()->reset();
		}
		virtual bool parseCustomOperation(void* env, DataObjectPtr ret, const std::string& operation) {
            return Parent::callErrorMessageCode3(env, ret, operation, "<- unknown operation!!!!");
		}
		bool extractAndParseLine(void* env, DataObjectPtr ret) {
			CLIPSValue line;
			__RETURN_FALSE_ON_FALSE__(Parent::tryExtractArgument1(env, ret, &line, syn::MayaType::String, "Must provide a string to parse!"));
			std::string str(syn::extractLexeme(env, line));
			auto result = parseLine(env, ret, str);
			CVSetBoolean(ret, result);
			if (!result) {
				Parent::callErrorMessageCode3(env, ret, "parse", "error during parsing!");
			}
			return result;
		}
		bool resolveLabels(void* env, DataObjectPtr ret) {
			try {
				CVSetBoolean(ret, true);
				return resolve(env, ret);
			} catch (const syn::Problem& p) {
                return Parent::callErrorMessageCode3(env, ret, "resolve", p);
			}
		}
		virtual bool isAssembler() noexcept override {
			return true;
		}
};
} // end namespace syn

#endif // end __SYN_COMMON_ASSEMBLER_WRAPPER_H
