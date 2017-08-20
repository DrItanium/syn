/**
 * @file
 * Somewhat base class for all external address wrappers, useful going forward
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


#ifndef __SYN_COMMON_EXTERNAL_ADDRESS_WRAPPER_H
#define __SYN_COMMON_EXTERNAL_ADDRESS_WRAPPER_H
#include "ClipsExtensions.h"
namespace syn {

/**
 * A common external address wrapper which removes the need to extract and
 * check to make sure that using the call function in CLIPS is done correctly.
 * The type "function" is handled by this class directly (call ?x type). If the
 * calls handled directly by the class do not meet the name then an abstract
 * method is called which is implemented by child classes.
 * @tparam T the type to be placed inside the external address wrapper parent.
 */
template<typename T>
class CommonExternalAddressWrapper : public ExternalAddressWrapper<T> {

    public:
        using Parent = ExternalAddressWrapper<T>;
        using Self = CommonExternalAddressWrapper<T>;
        static bool callFunction(void* env, DataObjectPtr value, DataObjectPtr ret) {
            __RETURN_FALSE_ON_FALSE__(Parent::isExternalAddress(env, ret, value));
            CLIPSValue operation;
            __RETURN_FALSE_ON_FALSE__(Parent::tryExtractFunctionName(env, ret, &operation));
            std::string str(extractLexeme(env, operation));
            if (str == "type") {
                CVSetSymbol(ret, Parent::getType().c_str());
                return true;
            } else {
				static_assert(ExternalAddressWrapperType<T>::customImpl, "Must provide a custom external address wrapper type defintion, the default one will segfault the program on use!");
				// most likely we can safely do this so go for it if we
				// have a custom implementation
				auto* ptr = static_cast<typename ExternalAddressWrapperType<T>::TheType *>(EnvDOPToExternalAddress(value));
				return ptr->handleCallOperation(env, value, ret, str);
            }
        }
        static inline bool callErrorCode2(void* env, CLIPSValue* ret, const std::string& msg) noexcept {
            return Parent::badCallArgument(env, ret, 2, msg);
        }

        static inline bool callErrorCode3(void* env, CLIPSValue* ret, const std::string& msg) noexcept {
            return Parent::badCallArgument(env, ret, 3, msg);
        }

        static inline bool callErrorCode4(void* env, CLIPSValue* ret, const std::string& msg) noexcept {
            return Parent::badCallArgument(env, ret, 4, msg);
        }
        static void registerWithEnvironment(void* env) noexcept {
            registerWithEnvironment(env, Parent::getType().c_str());
        }
		static void registerWithEnvironment(void* env, const char* title) {
            Parent::registerWithEnvironment(env, title, callFunction);
		}
    public:
        using Parent::Parent;
		virtual ~CommonExternalAddressWrapper() { }
		/**
		 * Called if the static callFunction can't satisfy the given function
		 * to call.
		 * @param env the environment which called this external address wrapper
		 * @param value the wrapped external address wrapper, this has already
		 * been extracted since this method is being called. It is provided
		 * just in case something else has to be done with it.
		 * @param ret The data object to store the return result in
		 * @param operation the extracted string in the set of arguments when
		 * performing a call.
		 * @return a boolean value signifying if an error occurred or not, this
		 * is not the same as what is actually returned to CLIPS.
		 */
        virtual bool handleCallOperation(void* env, DataObjectPtr value, DataObjectPtr ret, const std::string& operation) = 0;
};
} // end namespace syn
#endif
