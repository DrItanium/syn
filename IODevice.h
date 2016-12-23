/**
 * The concept of an IO devices which is mapped into memory
 */
#ifndef IRIS_IO_DEVICE_H_
#define IRIS_IO_DEVICE_H_
#include <tuple>
#include <functional>
#include "Device.h"
namespace iris {
template<typename Data, typename Address = Data>
class IODevice : public Device {
	public:
		using AddressType = Address;
		using DataType = Data;
		using AddressRange = std::tuple<Address, Address>;
	public:
		IODevice(Address begin, Address end) : _begin(begin), _end(end) { }
		virtual ~IODevice() { }
		virtual AddressRange getResponseRange() const noexcept;
		virtual bool respondsTo(Address targetAddress) const noexcept ;
		virtual Address size() const noexcept { return _end - _begin; }
		virtual Address beginAddress() const noexcept { return _begin; }
		virtual Address endAddress() const noexcept { return _end; }

		virtual Data read(Address targetAddress) = 0;
		virtual void write(Address targetAddress, Data value) = 0;
		virtual void initialize() override;
		virtual void shutdown() override;
	protected:
		Address _begin;
		Address _end;
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
	return std::make_tuple(_begin, _end);
}

template<typename Data, typename Address>
bool IODevice<Data, Address>::respondsTo(Address targetAddress) const noexcept {
	return targetAddress >= _begin && targetAddress <= _end;
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
		LambdaIODevice(Address begin, Address end, ReadFunction onRead, WriteFunction onWrite, InitializeFunction init = initNothing<Data, Address>, ShutdownFunction shutdown = shutdownNothing<Data, Address>);
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

} // end namespace iris
#endif // end IRIS_IO_DEVICE_H_
