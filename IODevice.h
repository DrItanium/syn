/**
 * The concept of an IO devices which is mapped into memory
 */
#ifndef IO_DEVICE_H_
#define IO_DEVICE_H_
#include <tuple>
#include <functional>
namespace iris {
template<typename Data, typename Address = Data>
class IODevice {
	public:
		using AddressRange = std::tuple<Address, Address>;
	public:
		IODevice(Address begin, Address end) : _begin(begin), _end(end) { }
		virtual ~IODevice() { }
		virtual AddressRange getResponseRange() const;

		virtual Data read(Address targetAddress) = 0;
		virtual void write(Address targetAddress, Data value) = 0;
		virtual bool respondsTo(Address targetAddress) const;
	protected:
		Address _begin;
		Address _end;
};
template<typename Data, typename Address>
typename IODevice<Data, Address>::AddressRange IODevice<Data, Address>::getResponseRange() const {
	return std::make_tuple(_begin, _end);
}

template<typename Data, typename Address>
bool IODevice<Data, Address>::respondsTo(Address targetAddress) const {
	return targetAddress >= _begin && targetAddress <= _end;
}

template<typename Data, typename Address = Data>
class LambdaIODevice : public IODevice<Data, Address> {
	public:
		using ReadFunction = std::function<Data(Address)>;
		using WriteFunction = std::function<void(Address, Data)>;
	public:
		LambdaIODevice(Address begin, Address end, ReadFunction onRead, WriteFunction onWrite);
		virtual ~LambdaIODevice();
		virtual Data read(Address targetAddress) override;
		virtual void write(Address targetAddress, Data value) override;
	private:
		ReadFunction _onRead;
		WriteFunction _onWrite;
};

template<typename Data, typename Address>
LambdaIODevice<Data, Address>::LambdaIODevice(Address begin, Address end, ReadFunction onRead, WriteFunction onWrite) : 
	IODevice<Data, Address>(begin, end), 
	_onRead(onRead), 
	_onWrite(onWrite) { }

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
} // end namespace iris
#endif // end IO_DEVICE_H_
