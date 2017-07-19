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


/**
 * The concept of an IO devices which is mapped into memory
 */
#ifndef SYN_WRAPPED_IO_DEVICE_H_
#define SYN_WRAPPED_IO_DEVICE_H_
#include <tuple>
#include <functional>
#include <iostream>
#include <memory>
#include <random>
#include <future>

#include "Base.h"
#include "Problem.h"
#include "Device.h"
#include "Core.h"
#include "IODevice.h"
#include "ClipsExtensions.h"
namespace syn {

    /**
     * Extract out all of the different routines which will _NOT_ change from
     * instantiation to instantiation of WrappedIODevice.
     */
    namespace WrappedIODeviceConstants {
        enum class Operations {
            Type,
            Read,
            Write,
            Initialize,
            Shutdown,
            ListCommands,
            Count,
            Error = Count,
        };
        const char* operationsName(Operations op) noexcept;

        Operations nameToOperation(const std::string& title) noexcept;
        constexpr int getArgCount(Operations op) noexcept;
        bool getCommandList(void* env, CLIPSValuePtr ret) noexcept;
    } // end namespace WrappedIODeviceConstants

    template<>
    constexpr auto defaultErrorState<WrappedIODeviceConstants::Operations> = WrappedIODeviceConstants::Operations::Error;

    void handleProblem(void* env, const syn::Problem& p, CLIPSValue* ret, const std::string& funcErrorPrefix, const char* type, int code) noexcept;
    template<typename Data, typename Address, template<typename, typename> class T>
    struct WrappedIODeviceBuilder {
        WrappedIODeviceBuilder() = delete;
        ~WrappedIODeviceBuilder() = delete;
        WrappedIODeviceBuilder(const WrappedIODeviceBuilder&) = delete;
        WrappedIODeviceBuilder(WrappedIODeviceBuilder&&) = delete;
        // by default, any wrapped IO device can accept zero arguments
        static T<Data, Address>* invokeNewFunction(void* env, CLIPSValuePtr ret, const std::string& funcErrorPrefix, const std::string& function) noexcept;
    };
    template<typename Data, typename Address, template<typename, typename> class T>
    class WrappedIODevice : public ExternalAddressWrapper<T<Data, Address>> {
        public:
            using InternalType = T<Data, Address>;
            using Self = WrappedIODevice<Data, Address, T>;
            using Parent = ExternalAddressWrapper<InternalType>;
            using Self_Ptr = Self*;
            using Operations = WrappedIODeviceConstants::Operations;
       public:
            static void newFunction(void* env, CLIPSValue* ret) {
                static auto init = true;
                static std::string funcStr;
                static std::string funcErrorPrefix;
                if (init) {
                    init = false;
                    auto t = retrieveFunctionNames<InternalType>("new");
                    funcStr = std::get<1>(t);
                    funcErrorPrefix = std::get<2>(t);
                }
                // build the internal object first!
                auto ptr = WrappedIODeviceBuilder<Data, Address, T>::invokeNewFunction(env, ret, funcErrorPrefix, funcStr);
                if (ptr) {
                    auto idIndex = Self::getAssociatedEnvironmentId(env);
                    ret->bitType = EXTERNAL_ADDRESS_TYPE;
                    SetpType(ret, EXTERNAL_ADDRESS);
                    SetpValue(ret, EnvAddExternalAddress(env, Self::make(ptr), idIndex));
                } else {
                    CVSetBoolean(ret, false);
                }
            }
            static bool callFunction(void* env, CLIPSValue* value, CLIPSValue* ret) {
                __RETURN_FALSE_ON_FALSE__(Parent::isExternalAddress(env, ret, value));
                CLIPSValue op;
                __RETURN_FALSE_ON_FALSE__(Parent::tryExtractFunctionName(env, ret, &op));
                std::string str(syn::extractLexeme(env, op));
                auto result = WrappedIODeviceConstants::nameToOperation(str);
                __RETURN_FALSE_ON_FALSE__(Parent::isLegalOperation(env, ret, str, result, syn::defaultErrorState<decltype(WrappedIODeviceConstants::nameToOperation(str))>));
                auto theOp = result;
                auto countResult = WrappedIODeviceConstants::getArgCount(theOp);
                if (countResult == -1) {
                    return Parent::callErrorMessageCode3(env, ret, str, "<- unknown argument count, not registered!!!");
                }
                __RETURN_FALSE_ON_FALSE__(Parent::checkArgumentCount(env, ret, str, countResult));
                auto ptr = static_cast<Self_Ptr>(DOPToExternalAddress(value));
                auto readOperation = [ptr, ret, env]() {
                    CLIPSValue tmp;
                    __RETURN_FALSE_ON_FALSE__(Parent::tryExtractArgument1(env, ret, &tmp, MayaType::Integer, "provided address is not an integer!"));
                    try {
                        auto address = syn::extractLong<Address>(env, tmp);
                        CVSetInteger(ret, ptr->read(address));
                        return true;
                    } catch(const syn::Problem& p) {
                        return Parent::callErrorMessageCode3(env, ret, "read", p);
                    }
                };
                auto writeOperation = [ptr, ret, env]() {
                    CLIPSValue t0, t1;
                    __RETURN_FALSE_ON_FALSE__(Parent::tryExtractArgument1(env, ret, &t0, MayaType::Integer, "provided address is not an integer!"));
                    __RETURN_FALSE_ON_FALSE__(Parent::tryExtractArgument2(env, ret, &t1, MayaType::Integer, "provided value is not an integer!"));
                    try {
                        auto address = syn::extractLong<Address>(env, t0);
                        auto value = syn::extractLong<Data>(env, t1);
                        ptr->write(address, value);
                        return setClipsBoolean(ret);
                    } catch(const syn::Problem& p) {
                        return Parent::callErrorMessageCode3(env, ret, "write", p);
                    }
                };
                switch(theOp) {
                    case Operations::Type:
                        Self::setType(ret);
                        return true;
                    case Operations::Shutdown:
                        ptr->shutdown();
                        return setClipsBoolean(ret);
                    case Operations::Initialize:
                        ptr->initialize();
                        return setClipsBoolean(ret);
                    case Operations::Read:
                        return readOperation();
                    case Operations::Write:
                        return writeOperation();
                    case Operations::ListCommands:
                        return WrappedIODeviceConstants::getCommandList(env, ret);
                    default:
                        return Parent::callErrorMessageCode3(env, ret, str, "<- unimplemented operation!!!!");
                }
            }
			static void registerWithEnvironment(void* env, const char* title) {
				Parent::registerWithEnvironment(env, title, callFunction, newFunction);
			}
			static void registerWithEnvironment(void* env) {
                bool init = true;
                static std::string typeString;
                if (init) {
                    init = false;
                    typeString = Self::getType();
                }
				registerWithEnvironment(env, typeString.c_str());
			}
			static Self* make() noexcept {
				return new Self();
			}
            template<typename ... Args>
            static Self* make(Args ... args) noexcept {
                return new Self(args...);
            }
            static Self* make(InternalType* ptr) noexcept {
                return new Self(ptr);
            }
        public:
            WrappedIODevice() : Parent(std::move(std::make_unique<InternalType>())) { }
            template<typename ... Args>
            WrappedIODevice(Args ... args) : Parent(std::move(std::make_unique<InternalType>(args...))) { }
            WrappedIODevice(InternalType* ptr) : Parent(std::move(std::unique_ptr<InternalType>(ptr))) { }
            Data read(Address addr) { return this->_value->read(addr); }
            void write(Address addr, Data value) { this->_value->write(addr, value); }
            void initialize() { this->_value->initialize(); }
            void shutdown() { this->_value->shutdown(); }
    };

	template<typename Word, typename Address = CLIPSInteger>
	using WrappedGenericRandomDevice = WrappedIODevice<Word, Address, RandomDevice>;


    template<typename Data, typename Address, template<typename, typename> class T>
    T<Data, Address>* WrappedIODeviceBuilder<Data, Address, T>::invokeNewFunction(void* env, CLIPSValue* ret, const std::string& funcErrorPrefix, const std::string& function) noexcept {
        using InternalType = T<Data, Address>;
        try {
            if (!syn::hasCorrectArgCount(env, 1)) {
                return new InternalType();
            } else {
                errorMessage(env, "NEW", 1, funcErrorPrefix, " no arguments should be provided for function new!");
            }
        } catch(const syn::Problem& p) {
            handleProblem(env, p, ret, funcErrorPrefix, "NEW", 2);
        }
        return nullptr;
    }

    template<typename Data, typename Address>
        struct WrappedIODeviceBuilder<Data, Address, RandomDevice> {
            WrappedIODeviceBuilder() = delete;
            ~WrappedIODeviceBuilder() = delete;
            WrappedIODeviceBuilder(const WrappedIODeviceBuilder&) = delete;
            WrappedIODeviceBuilder(WrappedIODeviceBuilder&&) = delete;
            using InternalType = RandomDevice<Data, Address>;
            static InternalType* invokeNewFunction(void* env, CLIPSValue* ret, const std::string& prefix, const std::string& function) noexcept {
                try {
                    auto count = syn::getArgCount(env);
                    if (count == 1) {
                        return new InternalType();
                    } else if (count == 2) {
                        CLIPSValue val;
                        if (!syn::tryGetArgumentAsInteger(env, function, 2, &val)) {
                            CVSetBoolean(ret, false);
                            errorMessage(env, "NEW", 2, prefix, "first argument must be an integer to seed with!");
                            return nullptr;
                        }
                        return new InternalType(syn::extractLong<typename InternalType::SeedType>(env, val));
                    } else {
                        errorMessage(env, "NEW", 1, prefix, " too many arguments are provided for function new!");
                        return nullptr;
                    }
                } catch(const syn::Problem& p) {
                    handleProblem(env, p, ret, prefix, "NEW", 2);
                    return nullptr;
                }
            }
        };

	void CLIPS_installDefaultIODevices(void* theEnv);

} // end namespace syn
#endif // end SYN_WRAPPED_IO_DEVICE_H_

