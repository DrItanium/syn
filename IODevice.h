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
						auto value = static_cast<byte>(0);
						std::cin >> std::noskipws >> value;
						return static_cast<D>(value);
					} else {
						throw syn::Problem("Illegal address to read from!");
					}
				}
				virtual void write(A addr, D value) override {
					auto actualAddr = addr - this->baseAddress();
					if (actualAddr == static_cast<A>(Addresses::Put)) {
						std::cout.put(static_cast<char>(value));
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
					_next = std::async(std::launch::async, [this]() { return _engine(); });
				}
			private:
				std::future<std::mt19937_64::result_type> _next;
				std::mt19937_64 _engine;
		};

	template<typename Word, typename Address = Word>
	class WrappedGenericRandomDevice : public ExternalAddressWrapper<RandomDevice<Word, Address>> {
		public:
			using Device = RandomDevice<Word, Address>;
			using Parent = ExternalAddressWrapper<Device>;
			using Self = WrappedGenericRandomDevice<Word, Address>;
			static void newFunction(void* env, DATA_OBJECT* ret) {
				
			}
			static bool callFunction(void* env, DATA_OBJECT* value, DATA_OBJECT* ret) {
				return false;
			}
			static void registerWithEnvironment(void* env, const char* title) {
				Parent::registerWithEnvironment(env, title, newFunction, callFunction);
			}
			static void registerWithEnvironment(void* env) {
				registerWithEnvironment(env, Parent::getType());
			}
		public:
			enum Operations {
				Seed,
				Next,
				Skip,
			};
			WrappedGenericRandomDevice() : ExternalAddressWrapper<Device>(std::move(std::make_unique<Device>(0))) { }
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

	using RandomNumberGenerator64bitDevice = RandomDevice<uint64_t>;
	using RandomNumberGeneratorSigned64bitDevice = RandomDevice<int64_t>;

	DefWrapperSymbolicName(RandomNumberGenerator64bitDevice, "random-number-generator:uint64");
	DefWrapperSymbolicName(RandomNumberGeneratorSigned64bitDevice, "random-number-generator:int64");

	void CLIPS_installDefaultIODevices(void* theEnv);

} // end namespace syn
#endif // end IRIS_IO_DEVICE_H_

