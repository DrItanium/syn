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
#include "Base.h"
#include "Problem.h"
#include "Device.h"
#include "Core.h"
#include <random>
#include <future>
#include "ClipsExtensions.h"
#include "IODevice.h"
namespace syn {

    /**
     * Extract out all of the different routines which will _NOT_ change from
     * instantiation to instantiation of WrappedIODevice.
     */
    struct WrappedIODeviceConstants {
        WrappedIODeviceConstants() = delete;
        WrappedIODeviceConstants(const WrappedIODeviceConstants&) = delete;
        WrappedIODeviceConstants(WrappedIODeviceConstants&&) = delete;
        ~WrappedIODeviceConstants() = delete;
        enum Operations {
            Type,
            Read,
            Write,
            Initialize,
            Shutdown,
            ListCommands,
            Count,
            Error = Count,
        };
        static std::string operationsName(Operations op) noexcept;
        static Operations nameToOperation(const std::string& title) noexcept;
        static int getArgCount(Operations op) noexcept;
        static bool getCommandList(void* env, CLIPSValuePtr ret) noexcept;
    };
    template<typename Data, typename Address, template<typename, typename> class T>
    struct WrappedIODeviceBuilder {
        WrappedIODeviceBuilder() = delete;
        ~WrappedIODeviceBuilder() = delete;
        WrappedIODeviceBuilder(const WrappedIODeviceBuilder&) = delete;
        WrappedIODeviceBuilder(WrappedIODeviceBuilder&&) = delete;
        // by default, any wrapped IO device can accept zero arguments
        static T<Data, Address>* invokeNewFunction(void* env, CLIPSValuePtr ret, const std::string& funcErrorPrefix) noexcept;
    };
    template<typename Data, typename Address, template<typename, typename> class T>
    class WrappedIODevice : public ExternalAddressWrapper<T<Data, Address>> {
        public:
            using InternalType = T<Data, Address>;
            using Self = WrappedIODevice<Data, Address, T>;
            using Parent = ExternalAddressWrapper<InternalType>;
            using Self_Ptr = Self*;
            using Constants = WrappedIODeviceConstants;
            using Operations = Constants::Operations;
       public:
            static void newFunction(void* env, CLIPSValue* ret) {
                static auto init = true;
                static std::string funcStr;
                static std::string funcErrorPrefix;
                if (init) {
                    init = false;
                    std::stringstream ss, ss2;
                    ss << "new (" << Self::getType() << ")";
                    funcStr = ss.str();
                    ss2 << "Function " << funcStr;
                    funcErrorPrefix = ss2.str();
                }
                // build the internal object first!
                auto ptr = WrappedIODeviceBuilder<Data, Address, T>::invokeNewFunction(env, ret, funcErrorPrefix);
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
                static auto init = true;
                static std::string funcStr;
                static std::string funcErrorPrefix;
				if (init) {
					init = false;
					std::stringstream ss, ss2;
					ss << "call (" << Self::getType() << ")";
					funcStr = ss.str();
					ss2 << "Function " << funcStr;
					funcErrorPrefix = ss2.str();
				}
				auto callErrorMessage = [env, ret](const std::string& subOp, const std::string& rest) {
					CVSetBoolean(ret, false);
					std::stringstream stm;
					stm << " " << subOp << ": " << rest << std::endl;
					auto msg = stm.str();
					return errorMessage(env, "CALL", 3, funcErrorPrefix, msg);
				};

				if (GetpType(value) == EXTERNAL_ADDRESS) {
					CLIPSValue op;
					if (!EnvArgTypeCheck(env, funcStr.c_str(), 2, SYMBOL, &op)) {
						return errorMessage(env, "CALL", 2, funcErrorPrefix, "expected a function name to call!");
					} else {
						std::string str(EnvDOToString(env, op));
                        auto result = Constants::nameToOperation(str);
                        if (result == Operations::Error) {
							CVSetBoolean(ret, false);
							return callErrorMessage(str, "<- unknown operation requested!");
						} else {
							auto theOp = result;
                            auto countResult = Constants::getArgCount(theOp);
                            if (countResult == -1) {
								CVSetBoolean(ret, false);
								return callErrorMessage(str, "<- unknown argument count, not registered!!!");
							}
							auto count = 2 + countResult;
							if (count != EnvRtnArgCount(env)) {
								CVSetBoolean(ret, false);
								return callErrorMessage(str, " too many arguments provided!");
							}
                            auto ptr = static_cast<Self_Ptr>(DOPToExternalAddress(value));
                            auto readOperation = [ptr, ret, env]() {
				                CLIPSValue tmp;
                                if (!EnvArgTypeCheck(env, funcStr.c_str(), 3, INTEGER, &tmp)) {
                                    CVSetBoolean(ret, false);
                                    return errorMessage(env, "CALL", 3, funcErrorPrefix, "provided address is not an integer!");
                                } else {
									try {
										auto address = static_cast<Address>(EnvDOToLong(env, tmp));
										CVSetInteger(ret, ptr->read(address));
										return true;
									} catch(syn::Problem p) {
										CVSetBoolean(ret, false);
										return errorMessage(env, "CALL", 3, funcErrorPrefix, p.what());
									}
                                }
                            };
                            auto writeOperation = [ptr, ret, env]() {
                                CLIPSValue t0, t1;
                                if (!EnvArgTypeCheck(env, funcStr.c_str(), 3, INTEGER, &t0)) {
                                    CVSetBoolean(ret, false);
                                    return errorMessage(env, "CALL", 3, funcErrorPrefix, "provided address is not an integer!");
                                } else if (!EnvArgTypeCheck(env, funcStr.c_str(), 4, INTEGER, &t1)) {
                                    CVSetBoolean(ret, false);
                                    return errorMessage(env, "CALL", 3, funcErrorPrefix, "provided value is not an integer!");
                                } else {
									try {
										CVSetBoolean(ret, true);
										auto address = static_cast<Address>(EnvDOToLong(env, t0));
										auto value = static_cast<Data>(EnvDOToLong(env, t1));
                                        ptr->write(address, value);
										return true;
									} catch(syn::Problem p) {
										CVSetBoolean(ret, false);
										return errorMessage(env, "CALL", 3, funcErrorPrefix, p.what());
									}
                                }
                            };
							switch(theOp) {
                                case Operations::Type:
                                    CVSetString(ret, Self::getType().c_str());
                                    return true;
                                case Operations::Shutdown:
                                    CVSetBoolean(ret, true);
                                    ptr->shutdown();
                                    return true;
                                case Operations::Initialize:
                                    CVSetBoolean(ret, true);
                                    ptr->initialize();
                                    return true;
								case Operations::Read:
                                    return readOperation();
                                case Operations::Write:
                                    return writeOperation();
                                case Operations::ListCommands:
                                    return Constants::getCommandList(env, ret);
								default:
									CVSetBoolean(ret, false);
									return callErrorMessage(str, "<- unimplemented operation!!!!");
							}
						}
					}
				} else {
					return errorMessage(env, "CALL", 1, funcErrorPrefix, "Function call expected an external address as the first argument!");
				}

				return false;
            }
			static void registerWithEnvironment(void* env, const char* title) {
				Parent::registerWithEnvironment(env, title, newFunction, callFunction);
			}
			static void registerWithEnvironment(void* env, const std::string& str) {
				registerWithEnvironment(env, str.c_str());
			}
			static void registerWithEnvironment(void* env) {
                bool init = true;
                static std::string typeString;
                if (init) {
                    init = false;
                    typeString = Self::getType();
                }
				registerWithEnvironment(env, typeString);
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
    T<Data, Address>* WrappedIODeviceBuilder<Data, Address, T>::invokeNewFunction(void* env, CLIPSValue* ret, const std::string& funcErrorPrefix) noexcept {
        using InternalType = T<Data, Address>;
        try {
            if (EnvRtnArgCount(env) == 1) {
                return new InternalType();
            } else {
                errorMessage(env, "NEW", 1, funcErrorPrefix, " no arguments should be provided for function new!");
            }
        } catch(syn::Problem p) {
            CVSetBoolean(ret, false);
            std::stringstream s;
            s << "an exception was thrown: " << p.what();
            auto str = s.str();
            errorMessage(env, "NEW", 2, funcErrorPrefix, str);
        }
        return nullptr;
    }

	void CLIPS_installDefaultIODevices(void* theEnv);

} // end namespace syn
#endif // end SYN_WRAPPED_IO_DEVICE_H_

