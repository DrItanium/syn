/**
 * The concept of an IO devices which is mapped into memory
 */
#ifndef IRIS_IO_DEVICE_H_
#define IRIS_IO_DEVICE_H_
#include <tuple>
#include <functional>
#include <iostream>
#include <memory>
#include "iris_base.h"
#include "Problem.h"
#include "Device.h"
namespace iris {
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
		virtual Address endAddress() const noexcept { return _base + _count; }
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
		if (targetAddress >= _base && targetAddress < endAddress()) {
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
		LambdaIODevice(Address begin, Address end, 
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
LambdaIODevice<Data, Address>::LambdaIODevice(Address begin, Address end, ReadFunction onRead, WriteFunction onWrite, InitializeFunction init, ShutdownFunction shutdown) : 
	IODevice<Data, Address>(begin, end), 
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
				throw iris::Problem("Illegal address to read from!");
			}
		}
		virtual void write(A addr, D value) override {
			auto actualAddr = addr - this->baseAddress();
			if (actualAddr == static_cast<A>(Addresses::Put)) {
				std::cout.put(static_cast<char>(value));
			} else {
				throw iris::Problem("Illegal address to write to!");
			}
		}
};

} // end namespace iris
#endif // end IRIS_IO_DEVICE_H_

