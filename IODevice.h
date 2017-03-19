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
				using AddressRange = std::tuple<Address, Address>;
			public:
				IODevice(Address base, Address count) : _base(base), _count(count) { }
				IODevice(Address word) : IODevice(word, 1) { }
				virtual ~IODevice() { }
				virtual AddressRange getResponseRange() const noexcept;
				virtual bool respondsTo(Address targetAddress, Address length = 1) const;
				virtual bool respondsTo(IODevice<Data,Address>& other) const;
				virtual bool respondsTo(const std::shared_ptr<IODevice<Data,Address>>& other) const;
				virtual Address size() const noexcept { return _count; }
				virtual Address baseAddress() const noexcept { return _base; }
				virtual Address endAddress() const noexcept {
					if (_base == 0) {
						return _count;
					} else {
						return _base + _count;
					}
				}
                virtual Address computeInternalAddress(Address addr) const noexcept {
                    return addr - _base;
                }
				virtual Data read(Address targetAddress) = 0;
				virtual void write(Address targetAddress, Data value) = 0;
				virtual void initialize() override;
				virtual void shutdown() override;
			protected:
				Address _base;
				Address _count;
		};

	template<typename D, typename A>
		void IODevice<D, A>::initialize() {
			// do nothing
		}

	template<typename D, typename A>
		void IODevice<D, A>::shutdown() {
			// do nothing
		}
	template<typename Data, typename Address>
		typename IODevice<Data, Address>::AddressRange IODevice<Data, Address>::getResponseRange() const noexcept {
			return std::make_tuple(_base, endAddress());
		}

	template<typename Data, typename Address>
		bool IODevice<Data, Address>::respondsTo(Address targetAddress, Address length) const {
			for (auto i = targetAddress; i < targetAddress + length; ++i) {
				if (syn::inRangeExcludingMaximum<Address>(targetAddress, _base, endAddress())) {
					return true;
				}
			}
			return false;
		}

	template<typename D, typename A>
		bool IODevice<D, A>::respondsTo(const std::shared_ptr<IODevice<D, A>>& other) const {
			return respondsTo(other->_base, other->_count);
		}

	template<typename D, typename A>
		bool IODevice<D, A>::respondsTo(IODevice<D, A>& other) const {
			return respondsTo(other._base, other._count);
		}


	template<typename Data, typename Addr = Data>
		void initNothing() { }

	template<typename Data, typename Addr = Data>
		void shutdownNothing() { }

	template<typename Data, typename Addr = Data>
		void writeNothing(Addr address, Data value) { }
	template<typename Data, typename Addr = Data>
		Data readNothing(Addr address) { return static_cast<Data>(0); }

	template<typename Data, typename Address = Data>
		class LambdaIODevice : public IODevice<Data, Address> {
			public:
				using ReadFunction = std::function<Data(Address)>;
				using WriteFunction = std::function<void(Address, Data)>;
				using InitializeFunction = std::function<void()>;
				using ShutdownFunction = std::function<void()>;
			public:
				LambdaIODevice(Address begin, Address length,
						ReadFunction onRead,
						WriteFunction onWrite,
						InitializeFunction init = initNothing<Data, Address>,
						ShutdownFunction shutdown = shutdownNothing<Data, Address>);
				virtual ~LambdaIODevice();
				virtual Data read(Address targetAddress) override;
				virtual void write(Address targetAddress, Data value) override;
				virtual void initialize() override;
				virtual void shutdown() override;
			private:
				ReadFunction _onRead;
				WriteFunction _onWrite;
				InitializeFunction _init;
				ShutdownFunction _shutdown;
		};

	template<typename Data, typename Address>
		LambdaIODevice<Data, Address>::LambdaIODevice(Address begin, Address length,
				ReadFunction onRead,
				WriteFunction onWrite,
				InitializeFunction init,
				ShutdownFunction shutdown) :
			IODevice<Data, Address>(begin, length),
			_onRead(onRead),
			_onWrite(onWrite),
			_init(init),
			_shutdown(shutdown) { }

	template<typename D, typename A>
		LambdaIODevice<D, A>::~LambdaIODevice() { }

	template<typename D, typename A>
		D LambdaIODevice<D, A>::read(A addr) {
			return _onRead(addr);
		}

	template<typename D, typename A>
		void LambdaIODevice<D, A>::write(A addr, D value) {
			_onWrite(addr, value);
		}

	template<typename D, typename A>
		void LambdaIODevice<D, A>::initialize() {
			_init();
		}

	template<typename D, typename A>
		void LambdaIODevice<D, A>::shutdown() {
			_shutdown();
		}

	template<typename D, typename A = D>
		using LambdaIODeviceReadFunction = typename LambdaIODevice<D, A>::ReadFunction;

	template<typename D, typename A = D>
		using LambdaIODeviceWriteFunction = typename LambdaIODevice<D, A>::WriteFunction;

	template<typename D, typename A = D>
		using LambdaIODeviceInitializeFunction = typename LambdaIODevice<D, A>::InitializeFunction;

	template<typename D, typename A = D>
		using LambdaIODeviceShutdownFunction = typename LambdaIODevice<D, A>::ShutdownFunction;

	template<typename D, typename A = D>
		using SharedLambdaIODevice = typename std::shared_ptr<LambdaIODevice<D, A>>;

	template<typename D, typename A = D>
		SharedLambdaIODevice<D, A> makeLambdaDevice(A begin, A length, LambdaIODeviceReadFunction<D, A> onRead,
				LambdaIODeviceWriteFunction<D, A> onWrite,
				LambdaIODeviceInitializeFunction<D, A> onInit = initNothing<D, A>,
				LambdaIODeviceShutdownFunction<D, A> onShutdown = shutdownNothing<D, A>) {
			return std::make_shared<LambdaIODevice<D, A>>(begin, length, onRead, onWrite, onInit, onShutdown);
		}

	template<typename D, typename A = D>
		class StandardInputOutputDevice : public IODevice<D, A> {
			public:
				enum Addresses : A {
					Get,
					Put,
					Count,
				};
				StandardInputOutputDevice(A base) : IODevice<D, A>(base, static_cast<A>(Addresses::Count)) { }
				virtual ~StandardInputOutputDevice() { }
				virtual D read(A addr) override {
					auto actualAddr = addr - this->baseAddress();
					if (actualAddr == static_cast<A>(Addresses::Get)) {
                        return getc<D>();
					} else {
						throw syn::Problem("Illegal address to read from!");
					}
				}
				virtual void write(A addr, D value) override {
					auto actualAddr = addr - this->baseAddress();
					if (actualAddr == static_cast<A>(Addresses::Put)) {
                        putc<D>(value);
					} else {
						throw syn::Problem("Illegal address to write to!");
					}
				}
		};

	template<typename D, typename A = D>
		class RandomDevice : public IODevice<D, A> {
			public:
				enum Addresses : A {
					SeedRandom,
					NextRandom,
					SkipRandom,
					Count,
				};
				RandomDevice(A base) : IODevice<D, A>(base, static_cast<A>(Addresses::Count)) {
					_next = std::async(std::launch::async, [this]() { return _engine(); });
				}
				virtual ~RandomDevice() { }
				virtual D read(A addr) override {
					auto actualAddr = addr - this->baseAddress();
					if (actualAddr == static_cast<A>(Addresses::NextRandom)) {
						auto result = static_cast<D>(_next.get());
						_next = std::async(std::launch::async, [this]() { return _engine(); });
						return result;
					} else {
						throw syn::Problem("Illegal random device address to read from");
					}
				}

				virtual void write(A addr, D value) override {
					auto actualAddr = addr - this->baseAddress();
					if (actualAddr == static_cast<A>(Addresses::SeedRandom)) {
						_engine.seed(value);
						generateNextValue();
					} else if (actualAddr == static_cast<A>(Addresses::SkipRandom)) {
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
                };
                static std::map<Operations, int> argCounts = {
                    { Operations::Type, 0 },
                    { Operations::Read, 1 },
                    { Operations::Write, 2 },
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
                                    auto address = static_cast<Address>(EnvDOToLong(env, tmp));
                                    auto response = ptr->respondsTo(address);
                                    if (response) {
                                        CVSetInteger(ret, ptr->read(address));
                                    } else {
                                        CVSetBoolean(ret, false);
                                    }
                                    return response;
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
                                    CVSetBoolean(ret, true);
                                    auto address = static_cast<Address>(EnvDOToLong(env, t0));
                                    auto value = static_cast<Data>(EnvDOToLong(env, t1));
                                    ptr->write(address, value);
                                    return true;
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
            WrappedIODevice() : Parent(std::move(std::make_unique<InternalType>(0))) { }
    };
	template<typename Word, typename Address = CLIPSInteger>
	class WrappedGenericRandomDevice : public ExternalAddressWrapper<RandomDevice<Word, Address>> {
		public:
			using Device = RandomDevice<Word, Address>;
			using Parent = ExternalAddressWrapper<Device>;
			using Self = WrappedGenericRandomDevice<Word, Address>;
			using Self_Ptr = Self*;
			enum Operations {
				Seed,
				Next,
				Skip,
			};
			static void newFunction(void* env, DATA_OBJECT* ret) {
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
			static bool callFunction(void* env, DATA_OBJECT* value, DATA_OBJECT* ret) {
				static auto init = true;
				static std::string funcStr;
				static std::string funcErrorPrefix;
				static std::map<std::string, Operations> opTranslation = {
					{ "seed", Operations::Seed },
					{ "next", Operations::Next },
					{ "skip", Operations::Skip },
				};
				static std::map<Operations, int> argCounts = {
					{ Operations::Seed, 1 },
					{ Operations::Next, 0 },
					{ Operations::Skip, 0 },
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
							switch(theOp) {
								case Operations::Seed:
									return ptr->seedRandom(env, ret, funcStr, funcErrorPrefix);
								case Operations::Next:
									CVSetInteger(ret, ptr->nextRandom());
									return true;
								case Operations::Skip:
									ptr->skipRandom();
									return true;
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
			WrappedGenericRandomDevice() : ExternalAddressWrapper<Device>(std::move(std::make_unique<Device>(0))) { }
			inline bool seedRandom(void* env, CLIPSValue* ret, const std::string& funcStr, const std::string& funcErrorPrefix) {
				CLIPSValue tmp;
				if (!EnvArgTypeCheck(env, funcStr.c_str(), 3, INTEGER, &tmp)) {
					CVSetBoolean(ret, false);
					return errorMessage(env, "CALL", 3, funcErrorPrefix, "seed argument is not an integer!");
				} else {
					seedRandom(static_cast<Word>(EnvDOToLong(env, tmp)));
					return true;
				}
			}
			inline void seedRandom(Word value) {
				this->_value->write(Device::Addresses::SeedRandom, value);
			}
			inline void skipRandom() {
				this->_value->write(Device::Addresses::SkipRandom, 0u);
			}
			inline Word nextRandom() {
				return this->_value->read(Device::Addresses::NextRandom);
			}
	};

	using RandomNumberGenerator64bitDevice = RandomDevice<uint64_t, CLIPSInteger>;
	using RandomNumberGeneratorSigned64bitDevice = RandomDevice<int64_t, CLIPSInteger>;
	using RandomNumberGenerator32bitDevice = RandomDevice<uint32_t, CLIPSInteger>;
	using RandomNumberGeneratorSigned32bitDevice = RandomDevice<int32_t, CLIPSInteger>;
	using RandomNumberGenerator16bitDevice = RandomDevice<uint16_t, CLIPSInteger>;
	using RandomNumberGeneratorSigned16bitDevice = RandomDevice<int16_t, CLIPSInteger>;
	using RandomNumberGenerator8bitDevice = RandomDevice<uint8_t, CLIPSInteger>;
	using RandomNumberGeneratorSigned8bitDevice = RandomDevice<int8_t, CLIPSInteger>;

	DefWrapperSymbolicName(RandomNumberGenerator64bitDevice, "random-number-generator:uint64");
	DefWrapperSymbolicName(RandomNumberGeneratorSigned64bitDevice, "random-number-generator:int64");
	DefWrapperSymbolicName(RandomNumberGenerator32bitDevice, "random-number-generator:uint32");
	DefWrapperSymbolicName(RandomNumberGeneratorSigned32bitDevice, "random-number-generator:int32");
	DefWrapperSymbolicName(RandomNumberGenerator16bitDevice, "random-number-generator:uint16");
	DefWrapperSymbolicName(RandomNumberGeneratorSigned16bitDevice, "random-number-generator:int16");
	DefWrapperSymbolicName(RandomNumberGenerator8bitDevice, "random-number-generator:uint8");
	DefWrapperSymbolicName(RandomNumberGeneratorSigned8bitDevice, "random-number-generator:int8");

	using WrappedRandomNumberGenerator64bitDevice = WrappedGenericRandomDevice<uint64_t>;
	using WrappedRandomNumberGeneratorSigned64bitDevice = WrappedGenericRandomDevice<int64_t>;
	using WrappedRandomNumberGenerator32bitDevice = WrappedGenericRandomDevice<uint32_t>;
	using WrappedRandomNumberGeneratorSigned32bitDevice = WrappedGenericRandomDevice<int32_t>;
	using WrappedRandomNumberGenerator16bitDevice = WrappedGenericRandomDevice<uint16_t>;
	using WrappedRandomNumberGeneratorSigned16bitDevice = WrappedGenericRandomDevice<int16_t>;
	using WrappedRandomNumberGenerator8bitDevice = WrappedGenericRandomDevice<uint8_t>;
	using WrappedRandomNumberGeneratorSigned8bitDevice = WrappedGenericRandomDevice<int8_t>;

	void CLIPS_installDefaultIODevices(void* theEnv);

} // end namespace syn
#endif // end IRIS_IO_DEVICE_H_

