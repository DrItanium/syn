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
#ifndef SYN_IO_DEVICE_H_
#define SYN_IO_DEVICE_H_
#include <tuple>
#include <functional>
#include <iostream>
#include <memory>
#include <random>
#include <future>

#include "Problem.h"
#include "BaseArithmetic.h"
#include "Device.h"

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
				virtual void initialize() override { }
				virtual void shutdown() override { }
		};

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
} // end namespace syn
#endif // end SYN_IO_DEVICE_H_

