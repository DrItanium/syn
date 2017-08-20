/**
 * @file
 * An interface between clips and the iris assembler
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
#include <vector>
#include "IrisClipsExtensions.h"
#include "ClipsExtensions.h"
#include "CommonAssemblerWrapper.h"

namespace iris {
    namespace assembler {
	    class AssemblerState;
    }
    class AssemblerStateWrapper : public syn::AssemblerWrapper<assembler::AssemblerState> {

        public:
            using Self = AssemblerStateWrapper;
            using Parent = syn::AssemblerWrapper<assembler::AssemblerState>;
         public:
			using Parent::Parent;
			virtual ~AssemblerStateWrapper()  { }
            virtual bool parseLine(void* env, syn::DataObjectPtr ret, const std::string& line) override;
            virtual bool resolve(void* env, syn::DataObjectPtr ret) override;
			virtual void getEncodedValues(void* env, syn::DataObjectPtr ret) override;
         private:
            void output(void* env, CLIPSValue* ret) noexcept;
    };

} // end namespace iris
#endif // end IRIS_CORE_ASSEMBLER_H__
