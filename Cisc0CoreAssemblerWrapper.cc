/**
 * @file
 * Implementation of cisc0's assembler <-> CLIPS interface
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


// Cisc0CoreAssembler rewritten to use pegtl
#include "Cisc0ClipsExtensions.h"
#include "Cisc0CoreAssembler.h"
#include "Cisc0CoreAssemblerWrapper.h"
#include <tao/pegtl.hpp>
#include <tao/pegtl/analyze.hpp>
#include <tao/pegtl/contrib/raw_string.hpp>
#include <tao/pegtl/contrib/abnf.hpp>
#include <tao/pegtl/parse.hpp>
#include <tao/pegtl/string_input.hpp>

namespace cisc0 {

	void installAssemblerParsingState(void* env) {
		// AssemblerState needs to be an external address and we can have
		// multiple assembler states sitting around too!
        tao::pegtl::analyze<cisc0::assembler::Main>();
		// make sure that the parser is still valid before we go any further!
		AssemblerStateWrapper::registerWithEnvironment(env);
		AssemblerStateWrapper::registerWithEnvironment(env, "cisc0-asm-parser");
		AssemblerStateWrapper::registerWithEnvironment(env, "cisc0-assembler");
	}
    namespace assembler {
		void AssemblerState::reset() noexcept {
			finalWords.clear();
			wordsToResolve.clear();
			LabelParent::reset();
			AddressParent::reset();
			FinishedDataParent::reset();
		}
        void AssemblerState::output(void* env, CLIPSValue* ret) noexcept {
            // we need to build a multifield out of the finalWords
            syn::MultifieldBuilder f(env, finalWords.size() * 2);
            int i = 1;
            for (auto q : finalWords) {
                // add them two at a time!
				f.setField(i, q.getAddress());
                f.setField(i + 1, q.getValue());
                i += 2;
            }
            f.assign(ret);
        }
    } // end namespace assembler
	void AssemblerStateWrapper::getEncodedValues(void* env, CLIPSValuePtr ret) {
		get()->output(env, ret);
	}

	bool AssemblerStateWrapper::resolve(void* env, syn::DataObjectPtr ret) {
		get()->resolveDeclarations();
		get()->resolveInstructions();
		return true;
	}
	bool AssemblerStateWrapper::parseLine(void* env, syn::DataObjectPtr ret, const std::string& line) {
		try {
			auto& ref = *(get());
			std::string tmpStorage;
			tao::pegtl::string_input<> in(line, tmpStorage);
			return tao::pegtl::parse<assembler::Main, assembler::Action>(in, ref);
		} catch(const tao::pegtl::parse_error& e) {
			return Parent::callErrorMessageCode3(env, ret, line, e);
		}
	}
}
