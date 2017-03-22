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
#ifndef IRIS_IO_DEVICE_H_
#define IRIS_IO_DEVICE_H_
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
namespace syn {
	template<typename Data, typename Address = Data>
		class IODevice : public Device {
			public:
				using AddressType = Address;
				using DataType = Data;
			public:
				IODevice() { }
				virtual ~IODevice() { }
				virtual Data read(Address targetAddress) = 0;
				virtual void write(Address targetAddress, Data value) = 0;
				virtual void initialize() override;
				virtual void shutdown() override;
		};

	template<typename D, typename A>
		void IODevice<D, A>::initialize() {
			// do nothing
		}

	template<typename D, typename A>
		void IODevice<D, A>::shutdown() {
			// do nothing
		}
	template<typename Data, typename Address = Data>
	class AddressableIODevice : public IODevice<Data, Address> {
		public:
			using Parent = IODevice<Data, Address>;
		public:
			AddressableIODevice(Address base, Address length = 1) : Parent(), _base(base), _length(length) { }
			virtual ~AddressableIODevice() { }
			virtual Address baseAddress() const noexcept { return _base; }
			virtual Address endAddress() const noexcept { return _base == 0 ? _length : (_base + _length); }
			virtual Address size() const noexcept { return _length; }
			virtual bool respondsTo(Address targetAddress, Address length = 1) const noexcept {
				for (auto i = targetAddress; i < targetAddress + length; ++i) {
					if (syn::inRangeExcludingMaximum<Address>(targetAddress, _base, endAddress())) {
						return true;
					}
				}
				return false;
			}
			virtual Address computeInternalAddress(Address addr) const noexcept {
				return addr - _base;
			}
		protected:
			Address _base;
			Address _length;
	};
	/**
	 * Wrap another IODevice in this class and perform address checks before
	 * passing the data off to the device itself. It will also perform the
	 * address flattening as well
	 */
	template<typename T>
	class CaptiveAddressableIODevice : public AddressableIODevice<typename T::DataType, typename T::AddressType> {
		public:
			using Parent = AddressableIODevice<typename T::DataType, typename T::AddressType>;
			using CapturedType = T;
			using Address = typename Parent::AddressType;
			using Data = typename Parent::DataType;
		public:
			CaptiveAddressableIODevice(Address base, Address length = 1) : Parent(base, length) { }
			virtual ~CaptiveAddressableIODevice() { }
			virtual Data read(Address targetAddress) override;
			virtual void write(Address targetAddress, Data value) override;
			virtual void initialize() override {
				_this.initialize();
			}
			virtual void shutdown() override {
				_this.shutdown();
			}
		private:
			T _this;
	};

	template<typename T>
	typename CaptiveAddressableIODevice<T>::Data CaptiveAddressableIODevice<T>::read(CaptiveAddressableIODevice<T>::Address targetAddress) {
		if (this->respondsTo(targetAddress)) {
			return _this.read(this->computeInternalAddress(targetAddress));
		} else {
			throw syn::Problem("IODevice error! Provided device does not respond to the given address!");
		}
	}
	template<typename T>
	void CaptiveAddressableIODevice<T>::write(CaptiveAddressableIODevice<T>::Address targetAddress, CaptiveAddressableIODevice<T>::Data value) {
		if (this->respondsTo(targetAddress)) {
			_this.write(this->computeInternalAddress(targetAddress), value);
		} else {
			throw syn::Problem("IODevice error! Provided device does not respond to the given address!");
		}
	}


	template<typename D, typename A = D>
		class RandomDevice : public IODevice<D, A> {
			public:
				enum Addresses : A {
					SeedRandom,
					NextRandom,
					SkipRandom,
					Count,
				};
				using Operations = Addresses;
				RandomDevice() : IODevice<D, A>() {
					_next = std::async(std::launch::async, [this]() { return _engine(); });
				}
				virtual ~RandomDevice() { }
				virtual D read(A addr) override {
					if (addr == static_cast<A>(Addresses::NextRandom)) {
						auto result = static_cast<D>(_next.get());
						_next = std::async(std::launch::async, [this]() { return _engine(); });
						return result;
					} else {
						throw syn::Problem("Illegal random device address to read from");
					}
				}

				virtual void write(A addr, D value) override {
					if (addr == static_cast<A>(Addresses::SeedRandom)) {
						_engine.seed(value);
						generateNextValue();
					} else if (addr == static_cast<A>(Addresses::SkipRandom)) {
						_engine.discard(value);
						generateNextValue();
					} else {
						throw syn::Problem("Illegal random device address to write to");
					}
				}
			private:
				void generateNextValue() noexcept {
                    (void)_next.get();
					_next = std::async(std::launch::async, [this]() { return _engine(); });
				}
			private:
				std::future<std::mt19937_64::result_type> _next;
				std::mt19937_64 _engine;
		};

    template<typename Data, typename Address, template<typename, typename> class T>
    class WrappedIODevice : public ExternalAddressWrapper<T<Data, Address>> {
        public:
            using InternalType = T<Data, Address>;
            using Self = WrappedIODevice<Data, Address, T>;
            using Parent = ExternalAddressWrapper<InternalType>;
            using Self_Ptr = Self*;
            enum Operations {
                Type,
                Read,
                Write,
                Initialize,
                Shutdown,
                ListCommands,
                Count,
            };
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
                try {
                    if (EnvRtnArgCount(env) == 1) {
                        auto idIndex = Self::getAssociatedEnvironmentId(env);
                        ret->bitType = EXTERNAL_ADDRESS_TYPE;
                        SetpType(ret, EXTERNAL_ADDRESS);
                        SetpValue(ret, EnvAddExternalAddress(env, Self::make(), idIndex));
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
            }
            static bool callFunction(void* env, CLIPSValue* value, CLIPSValue* ret) {
                static auto init = true;
                static std::string funcStr;
                static std::string funcErrorPrefix;
                static std::map<std::string, Operations> opTranslation = {
                    { "read", Operations::Read },
                    { "write", Operations::Write },
                    { "type",  Operations::Type },
                    { "initialize", Operations::Initialize },
                    { "shutdown", Operations::Shutdown },
                    { "list-commands", Operations::ListCommands },
                };
                static std::map<Operations, int> argCounts = {
                    { Operations::Type, 0 },
                    { Operations::Read, 1 },
                    { Operations::Write, 2 },
					{ Operations::Initialize, 0 },
					{ Operations::Shutdown, 0 },
					{ Operations::ListCommands, 0 },
                };
                static std::map<Operations, std::string> reverseNameLookup = {
                    { Operations::Read, "read" },
                    { Operations::Write, "write" },
                    { Operations::Type, "type" },
                    { Operations::Initialize, "initialize" },
                    { Operations::Shutdown, "shutdown" },
                    { Operations::ListCommands, "list-commands" },
                };
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
					auto ptr = static_cast<Self_Ptr>(DOPToExternalAddress(value));
					CLIPSValue op;
					if (!EnvArgTypeCheck(env, funcStr.c_str(), 2, SYMBOL, &op)) {
						return errorMessage(env, "CALL", 2, funcErrorPrefix, "expected a function name to call!");
					} else {
						std::string str(EnvDOToString(env, op));
						auto result = opTranslation.find(str);
						if (result == opTranslation.end()) {
							CVSetBoolean(ret, false);
							return callErrorMessage(str, "<- unknown operation requested!");
						} else {
							auto theOp = result->second;
							auto countResult = argCounts.find(theOp);
							if (countResult == argCounts.end()) {
								CVSetBoolean(ret, false);
								return callErrorMessage(str, "<- unknown argument count, not registered!!!");
							}
							auto count = 2 + countResult->second;
							if (count != EnvRtnArgCount(env)) {
								CVSetBoolean(ret, false);
								return callErrorMessage(str, " too many arguments provided!");
							}
                            auto readOperation = [ptr, ret, env]() {
				                CLIPSValue tmp;
                                if (!EnvArgTypeCheck(env, funcStr.c_str(), 3, INTEGER, &tmp)) {
                                    CVSetBoolean(ret, false);
                                    return errorMessage(env, "CALL", 3, funcErrorPrefix, "provided address is not an integer!");
                                } else {
									try {
										auto address = static_cast<Address>(EnvDOToLong(env, tmp));
										CVSetInteger(ret, ptr->_value->read(address));
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
										ptr->_value->write(address, value);
										return true;
									} catch(syn::Problem p) {
										CVSetBoolean(ret, false);
										return errorMessage(env, "CALL", 3, funcErrorPrefix, p.what());
									}
                                }
                            };
                            auto listCommands = [ptr, ret, env]() {
                                FixedSizeMultifieldBuilder<static_cast<long>(Operations::Count)> mb(env);
                                auto setField = [&mb, env](int index, Operations op) {
                                    mb.setField(index, SYMBOL, EnvAddSymbol(env, reverseNameLookup[op].c_str()));
                                };
                                setField(1, Operations::Read);
                                setField(2, Operations::Write);
                                setField(3, Operations::Type);
                                setField(4, Operations::Initialize);
                                setField(5, Operations::Shutdown);
                                setField(6, Operations::ListCommands);
                                mb.assign(ret);
                                return true;
                            };
							switch(theOp) {
                                case Operations::Type:
                                    CVSetString(ret, Self::getType().c_str());
                                    return true;
                                case Operations::Shutdown:
                                    CVSetBoolean(ret, true);
                                    ptr->_value->shutdown();
                                    return true;
                                case Operations::Initialize:
                                    CVSetBoolean(ret, true);
                                    ptr->_value->initialize();
                                    return true;
								case Operations::Read:
                                    return readOperation();
                                case Operations::Write:
                                    return writeOperation();
                                case Operations::ListCommands:
                                    return listCommands();
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
        public:
            WrappedIODevice() : Parent(std::move(std::make_unique<InternalType>())) { }
    };

	template<typename Word, typename Address = CLIPSInteger>
	using WrappedGenericRandomDevice = WrappedIODevice<Word, Address, RandomDevice>;


	void CLIPS_installDefaultIODevices(void* theEnv);

} // end namespace syn
#endif // end IRIS_IO_DEVICE_H_

