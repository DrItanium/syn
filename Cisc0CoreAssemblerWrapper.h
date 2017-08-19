/**
 * @file
 * The assembler wrapper description
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


#ifndef CISC0_CORE_ASSEMBLER_WRAPPER_H__
#define CISC0_CORE_ASSEMBLER_WRAPPER_H__

#include <string>
#include <sstream>
#include <typeinfo>
#include <iostream>
#include <map>
#include "Base.h"
#include "AssemblerBase.h"
#include "Problem.h"
#include <vector>
#include "ClipsExtensions.h"

namespace cisc0 {
    namespace assembler {
	    struct AssemblerState;
    } // end namespace assembler
    /**
     * Interface between cisc0's assembler and clips.
     */
    class AssemblerStateWrapper : public syn::CommonExternalAddressWrapper<assembler::AssemblerState> {
        public:
            using Self = AssemblerStateWrapper;
            using Parent = syn::CommonExternalAddressWrapper<assembler::AssemblerState>;
        public:
            /**
             * The different actions that this wrapper can perform when invoked
             * through clips.
             */
            enum Operations {
                Parse,
                Resolve,
                Get,
                Count,
            };
			static Self* make() noexcept;
        public:
			AssemblerStateWrapper();
            bool parseLine(const std::string& line);
            bool resolve();
            void getMultifield(void* env, CLIPSValuePtr ret);
            virtual bool handleCallOperation(void* env, syn::DataObjectPtr value, syn::DataObjectPtr ret, const std::string& operation) override;

    };
}
namespace syn {
	DefWrapperSymbolicName(cisc0::assembler::AssemblerState, "cisc0:assembly-parsing-state");
}
#endif
