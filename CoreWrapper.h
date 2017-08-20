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
#include <map>

namespace syn {
// Paste these default core operations in

#define __DEFAULT_ERROR_STATE__ Count

#define __DEFAULT_CORE_OPERATIONS_EXEC__(TYPE) \
    case TYPE :: Initialize: initialize(); break; \
    case TYPE :: Shutdown: shutdown(); break; \
    case TYPE :: Run: run(); break; \
    case TYPE :: Cycle: CVSetBoolean(ret, cycle()); break; \
	case TYPE :: DecodeInstruction: decodeInstruction(); break

/**
 * Base class for wrapping a Core for use in CLIPS as an external address type.
 * @tparam T the type to wrap
 */
template<typename T>
class CoreWrapper : public syn::CommonExternalAddressWrapper<T> {
    public:
        using Parent = syn::CommonExternalAddressWrapper<T>;
        using Self = CoreWrapper<T>;
    public:
        static void registerWithEnvironment(void* env) {
            Parent::registerWithEnvironment(env, Parent::getType().c_str());
        }
		static void setString(CLIPSValuePtr val, const std::string& str) noexcept {
			CVSetString(val, str.c_str());
		}
		enum class DefaultCoreOperations {
			Initialize, 
			Shutdown, 
			Run, 
			Cycle, 
			DecodeInstruction,
			__DEFAULT_ERROR_STATE__
		};
   public:
        /// Wrap an already existing core type
        CoreWrapper(T* core) : Parent(core) { }
        /// Construct a new Core type and pass it to the parent
        CoreWrapper() : Parent(new T()) { }
        virtual ~CoreWrapper() { }
        virtual bool handleCallOperation(void* env, DataObjectPtr value, DataObjectPtr ret, const std::string& operation) override {
			// all of the default core operations should be handled here
			static std::map<std::string, DefaultCoreOperations> lookup = {
				{ "initialize", DefaultCoreOperations::Initialize }, 
				{ "shutdown", DefaultCoreOperations::Shutdown },
				{ "run", DefaultCoreOperations::Run },
				{ "cycle", DefaultCoreOperations::Cycle },
				{ "decode-instruction", DefaultCoreOperations::DecodeInstruction },
			};
			auto result = lookup.find(operation);
			if (result != lookup.end()) {
				switch(result->second) {
					case DefaultCoreOperations::Initialize:
						get()->initialize();
						return true;
					case DefaultCoreOperations::Shutdown:
						get()->shutdown();
						return true;
					case DefaultCoreOperations::Run:
						get()->run();
						return true;
					default:
            			return Parent::callErrorMessageCode3(env, ret, str, "<- unknown but registered operation!!!!");
				}
			} else {
            	return this->get()->handleOperation(env, ret);
			}
        }
		virtual bool isCore() noexcept override {
			return true;
		}
};

} // end namespace syn
#endif // end CORE_WRAPPER_H__
