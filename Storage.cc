#include "phoenix.h"

namespace phoenix {
	Storage::Storage(Address size) : 
		_size(size), 
		_backingStore(new word[size]),
		_controller(_backingStore, size) 
	{ 
		
	}
	Storage::~Storage() { }
	void 
	Storage::initialize() {
		_controller.initialize();
	}

	void
	Storage::installprogram(std::istream& stream) {
		// install the controller firmware first
		_controller.installprogram(stream);
		// now we need to populate the storage "image"
		char buf[sizeof(word)] = { 0 };
		for (auto i = static_cast<Address>(0); i < _size; ++i) {
			stream.read(buf, sizeof(word));
			_backingStore.get()[i] = iris::encodeUint16LE(static_cast<byte>(buf[0]), static_cast<byte>(buf[1]));
		}
	}

	void 
	Storage::shutdown() {

	}

	void
	Storage::dump(std::ostream& stream) {
		_controller.dump(stream);
		char buf[sizeof(word)] = { 0 };
		for (auto i = static_cast<Address>(0); i < _size; ++i) {
			iris::decodeUint16LE(_backingStore.get()[i], (byte*)buf);
			stream.write(buf, sizeof(word));
		}
	}

	void
	Storage::run() {
		_controller.run();
	}
}
