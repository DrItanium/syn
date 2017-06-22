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


// Common CLIPS <-> C++ core wrapper
#ifndef CORE_WRAPPER_H__
#define CORE_WRAPPER_H__

#include "ClipsExtensions.h"
#include <map>

namespace syn {


// Paste these default core operations in
#define __DEFAULT_CORE_OPERATIONS__ \
        Initialize, \
        Shutdown, \
        Run, \
        Cycle

#define __DEFAULT_ERROR_STATE__ Count

#define __DEFAULT_CORE_OPERATIONS_EXEC__(TYPE) \
    case TYPE :: Initialize: initialize(); break; \
    case TYPE :: Shutdown: shutdown(); break; \
    case TYPE :: Run: run(); break; \
    case TYPE :: Cycle: CVSetBoolean(ret, cycle()); break

template<typename T>
class CoreWrapper : public syn::ExternalAddressWrapper<T> {
    public:
        using Parent = syn::ExternalAddressWrapper<T>;
        using Self = CoreWrapper<T>;
    public:
        static inline bool callErrorCode2(void* env, CLIPSValue* ret, const std::string& msg) noexcept {
            return Parent::badCallArgument(env, ret, 2, msg);
        }

        static inline bool callErrorCode3(void* env, CLIPSValue* ret, const std::string& msg) noexcept {
            return Parent::badCallArgument(env, ret, 3, msg);
        }

        static inline bool callErrorCode4(void* env, CLIPSValue* ret, const std::string& msg) noexcept {
            return Parent::badCallArgument(env, ret, 4, msg);
        }

        static bool callFunction(void* env, syn::DataObjectPtr value, syn::DataObjectPtr ret) {
            __RETURN_FALSE_ON_FALSE__(Parent::isExternalAddress(env, ret, value));
            auto ptr = static_cast<Self*>(EnvDOPToExternalAddress(value));
            return ptr->get()->handleOperation(env, ret);
        }
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
        CoreWrapper(T* core) : Parent(core) { }
        CoreWrapper() : Parent(new T()) { }
        virtual ~CoreWrapper() { }
};





} // end namespace syn
#endif // end CORE_WRAPPER_H__
