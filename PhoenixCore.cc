#include "phoenix.h"
#include <functional>
#include <sstream>
#include <vector>

namespace phoenix {


	template<byte mask, byte shift = 0>
	constexpr byte maskCorrectly(byte input) noexcept {
		return syn::decodeBits<byte, byte, mask, shift>(input);
	}
	constexpr byte mask64(byte input) noexcept {
		return maskCorrectly<0b00111111>(input);
	}
	constexpr byte mask32(byte input) noexcept {
		return maskCorrectly<0b00011111>(input);
	}
	constexpr byte mask16(byte input) noexcept {
		return maskCorrectly<0b00001111>(input);
	}
	constexpr byte mask8(byte input) noexcept {
		return maskCorrectly<0b00000111>(input);
	}
	constexpr byte mask4(byte input) noexcept {
		return maskCorrectly<0b00000011>(input);
	}
	constexpr byte mask2(byte input) noexcept {
		return maskCorrectly<0b00000001>(input);
	}
	Bit getBit(Register value, byte index) noexcept {
		auto correctMask = mask64(index);
		return syn::decodeBits<Register, Bit>(value, 0x1 << correctMask, correctMask);
	}
	Register setBit(Register value, byte index, Bit b) noexcept {
		auto correctMask = mask64(index);
		return syn::encodeBits<Register, Bit>(value, b, 0x1 << correctMask, correctMask);
	}

	Nybble getNybble(Register value, byte index) noexcept {
		auto correctMask = mask16(index) * 4;
		return syn::decodeBits<Register, Nybble>(value, 0xF << correctMask, correctMask);
	}
	Register setNybble(Register value, byte index, Nybble b) noexcept {
		auto correctMask = mask16(index) * 4;
		return syn::encodeBits<Register, Nybble>(value, b, 0xF << correctMask, correctMask);
	}
	Byte getByte(Register value, byte index) noexcept {
		auto correctMask = mask8(index) * 8;
		return syn::decodeBits<Register, Byte>(value, 0xFF << correctMask, correctMask);
	}
	Register setByte(Register value, byte index, Byte b) noexcept {
		auto correctMask = mask8(index) * 8;
		return syn::encodeBits<Register, Byte>(value, b, 0xFF << correctMask, correctMask);
	}
	Qword getQword(Register value, byte index) noexcept {
		auto correctMask = mask4(index) * 16;
		return syn::decodeBits<Register, Qword>(value, 0xFFFF << correctMask, correctMask);
	}
	Register setQword(Register value, byte index, Qword b) noexcept {
		auto correctMask = mask8(index) * 16;
		return syn::encodeBits<Register, Qword>(value, b, 0xFFFF << correctMask, correctMask);
	}
	Hword getHword(Register value, byte index) noexcept {
		auto correctMask = mask2(index) * 32;
		return syn::decodeBits<Register, Hword>(value, 0xFFFFFFFF << correctMask, correctMask);
	}
	Register setHword(Register value, byte index, Hword b) noexcept {
		auto correctMask = mask2(index) * 32;
		return syn::encodeBits<Register, Hword>(value, b, 0xFFFFFFFF << correctMask, correctMask);
	}

	//full 64bit addr space
	Core::Core() noexcept { }

	Core::~Core() { }

	void Core::installprogram(std::istream& stream) {
	}

	void Core::dump(std::ostream& stream) {

	}
	bool Core::cycle() {
		advanceIp = true;
		dispatch();
		if (advanceIp) {
			++_ip;
		}
		return execute;
	}
	void Core::dispatch() {
	}

	Core* newCore() noexcept {
		return new phoenix::Core();
	}

	Register& Core::getRegister(byte index) noexcept {
		return _gpr[index];
	}
}
