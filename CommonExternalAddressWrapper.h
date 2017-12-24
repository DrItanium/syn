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
#include "ExternalAddressWrapper.h"

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
		enum class BuiltinStandardFunctions {
			Type,
			Count,
		};
        static bool callFunction(UDFContext* context, UDFValue* value, UDFValue* ret) {
            using CastTo = typename ExternalAddressWrapperType<T>::TheType;
			static_assert(ExternalAddressWrapperType<T>::customImpl, "Must provide a custom external address wrapper type defintion, the default one will segfault the program on use!");
            static_assert(std::is_base_of<Self, CastTo>::value, "To use the common functionality hierarchy, you must inherit from CommonExternalAddressWrapper");
			static std::map<std::string, BuiltinStandardFunctions> lookup = {
				{ "type", BuiltinStandardFunctions::Type },
			};
			if (value->header->type != EXTERNAL_ADDRESS_TYPE) {
				setBoolean(context, ret, false);
				return false;
			}
            UDFValue operation;
			if (!extractFunctionName(context, operation)) {
				setBoolean(context, ret, false);
				return false;
			}
			std::string str(getLexeme(operation));
			// most likely we can safely do this so go for it if we
			// have a custom implementation
			auto* ptr = static_cast<CastTo*>(getExternalAddress(value));
			auto result = lookup.find(str);
			if (result != lookup.end()) {
				switch(result->second) {
					case BuiltinStandardFunctions::Type:
						CVSetSymbol(ret, Parent::getType().c_str());
						return true;
					default:
            			return Parent::callErrorMessageCode3(env, ret, str, "<- unknown but registered operation!!!!");
				}
            } else {
				return ptr->handleCallOperation(env, value, ret, str);
            }
        }
        static inline bool callErrorCode2(Environment* env, CLIPSValue* ret, const std::string& msg) noexcept {
            return Parent::badCallArgument(env, ret, 2, msg);
        }

        static inline bool callErrorCode3(Environment* env, CLIPSValue* ret, const std::string& msg) noexcept {
            return Parent::badCallArgument(env, ret, 3, msg);
        }

        static inline bool callErrorCode4(Environment* env, CLIPSValue* ret, const std::string& msg) noexcept {
            return Parent::badCallArgument(env, ret, 4, msg);
        }
        static void registerWithEnvironment(Environment* env) noexcept {
            registerWithEnvironment(env, Parent::getType().c_str());
        }
		static void registerWithEnvironment(Environment* env, const char* title) {
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
        virtual bool handleCallOperation(Environment* env, UDFValue* value, UDFValue* ret, const std::string& operation) = 0;
};
} // end namespace syn
#endif
