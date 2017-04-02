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
	};
	bool CoreWrapper::callFunction(void* env, syn::DataObjectPtr value, syn::DataObjectPtr ret) {
		// unpack the object and do the magic
		static bool init = true;
		static std::string funcStr;
		static std::string funcErrorPrefix;
		static std::map<std::string, CoreWrapper::Operations> ops = {
			{ "initialize", Operations::Initialize },
			{ "shutdown", Operations::Shutdown},
			{ "run", Operations::Run },
			{ "cycle", Operations::Cycle },
		};
		return true;
	}
	void installCoreWrapper(void* env) {
		CoreWrapper::registerWithEnvironment(env);
	}
	bool Core::handleOperation(void* env, CLIPSValue* ret) {
		return true;
	}
	Core* Core::make() noexcept {
		return new Core();
	}
} // end namespace cisc0

namespace syn {
	DefWrapperSymbolicName(cisc0::CoreWrapper,  "cisc0-core");
} // end namespace syn

