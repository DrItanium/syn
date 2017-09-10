/**
 * @file
 * Common CLIPS <-> C++ core wrapper
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


#ifndef CORE_WRAPPER_H__
#define CORE_WRAPPER_H__

#include "ClipsExtensions.h"
#include "CommonExternalAddressWrapper.h"
#include "DeviceWrapper.h"
#include "IOController.h"
#include <map>

namespace syn {
// Paste these default core operations in

#define __DEFAULT_ERROR_STATE__ Count

/**
 * Base class for wrapping a Core for use in CLIPS as an external address type.
 * @tparam T the type to wrap
 */
template<typename T>
class CoreWrapper : public syn::DeviceWrapper<T> {
    public:
        using Parent = syn::DeviceWrapper<T>;
        using Self = CoreWrapper<T>;
    public:
        static void registerWithEnvironment(void* env) {
            Parent::registerWithEnvironment(env, Parent::getType().c_str());
        }
		static void setString(CLIPSValuePtr val, const std::string& str) noexcept {
			CVSetString(val, str.c_str());
		}
		enum class DefaultCoreOperations {
			Run,
			Cycle,
			DecodeInstruction,
			WordSize,
			AddressSize,
			RegisterSize,
			__DEFAULT_ERROR_STATE__
		};
   public:
        /// Wrap an already existing core type
        CoreWrapper(T* core) : Parent(core) { }
        /// Construct a new Core type and pass it to the parent
        CoreWrapper() : Parent(new T()) { }
        virtual ~CoreWrapper() { }
        virtual bool handleExtendedCallOperation(void* env, DataObjectPtr value, DataObjectPtr ret, const std::string& operation) override {
            static_assert(std::is_base_of<Self, typename ExternalAddressWrapperType<T>::TheType>::value, "The external address wrapper for this core must inherit from this class to take advantage of it!");
			// all of the default core operations should be handled here
			static std::map<std::string, DefaultCoreOperations> lookup = {
				{ "run", DefaultCoreOperations::Run },
				{ "cycle", DefaultCoreOperations::Cycle },
				{ "decode-instruction", DefaultCoreOperations::DecodeInstruction },
				{ "address-size", DefaultCoreOperations::AddressSize },
				{ "word-size", DefaultCoreOperations::WordSize },
				{ "register-size", DefaultCoreOperations::RegisterSize },
			};
			auto result = lookup.find(operation);
			if (result != lookup.end()) {
				switch(result->second) {
					case DefaultCoreOperations::Run:
						this->get()->run();
						break;
					case DefaultCoreOperations::Cycle:
						CVSetBoolean(ret, this->get()->cycle());
						break;
					case DefaultCoreOperations::AddressSize:
						CVSetInteger(ret, this->getAddressSize());
						break;
					case DefaultCoreOperations::WordSize:
						CVSetInteger(ret, this->getWordSize());
						break;
					case DefaultCoreOperations::RegisterSize:
						CVSetInteger(ret, this->getRegisterSize());
					case DefaultCoreOperations::DecodeInstruction:
						return this->decodeInstruction(env, ret, operation);
					default:
            			return Parent::callErrorMessageCode3(env, ret, operation, "<- unknown but registered operation!!!!");
				}
				return true;
			} else {
            	return this->get()->handleOperation(env, ret);
			}
        }
		virtual bool isCore() noexcept override final {
			return true;
		}
		virtual bool decodeInstruction(void* env, DataObjectPtr ret, const std::string& op) = 0;
		virtual CLIPSInteger getWordSize() const noexcept = 0;
		virtual CLIPSInteger getAddressSize() const noexcept = 0;
		virtual CLIPSInteger getRegisterSize() const noexcept = 0;
};

template<typename Core>
Core* newCore(void* env, CLIPSValuePtr ret, const std::string& funcErrorPrefix, const std::string& function) noexcept {
    static_assert(std::is_constructible<Core, syn::CLIPSIOController&>::value, "The target core must be constructible with a reference to the clips io bus!");
	try {
		if (syn::getArgCount(env) == 1) {
			// make sure that our base is actually an IOBus
			return new Core(syn::CLIPSIOController::fromRaw(env));
		} else {
			syn::errorMessage(env, "NEW", 2, funcErrorPrefix, " no arguments should be provided for function new!");
		}
	} catch (const syn::Problem& p) {
		CVSetBoolean(ret, false);
		std::stringstream s;
		s << "an exception was thrown: " << p.what();
		auto str = s.str();
		syn::errorMessage(env, "NEW", 2, funcErrorPrefix, str);
	}
	return nullptr;
}

} // end namespace syn
#endif // end CORE_WRAPPER_H__
