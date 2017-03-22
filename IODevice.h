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
} // end namespace syn
#endif // end IRIS_IO_DEVICE_H_

