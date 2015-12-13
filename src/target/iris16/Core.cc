#include "target/iris16/iris.h"
#include <functional>

namespace iris16 {
	Core::Core() { }

	void Core::initialize() {

	}
	template<typename T, int count>
	void populateContents(T* contents, std::istream& stream, char* buf, std::function<T(std::istream&, char*)> func) {
		for(int i = 0; i < count; ++i) {
			contents[i] = func(stream, buf);
		}
	}
	void Core::installprogram(std::istream& stream) {
		char* buf = new char[4];
		auto encodeWord = [](std::istream& s, char* buf) {
			s.read(buf, 2);
			return iris::encodeBits<word, byte, word, 0xFF00, 8>(static_cast<word>(buf[0]), static_cast<byte>(buf[1]));
		};
		auto encodeDword = [](std::istream& s, char* buf) {
			s.read(buf, 4);
			return iris::encodeBits<dword, byte, dword, 0xFF000000, 24>(
					iris::encodeBits<dword, byte, dword, 0x00FF000, 16>(
						iris::encodeBits<dword, byte, dword, 0x0000FF00, 8>(
							static_cast<dword>(buf[0]), 
							static_cast<byte>(buf[1])),
						static_cast<byte>(buf[2])),
					static_cast<byte>(buf[3]));
		};
		populateContents<word, ArchitectureConstants::RegisterCount>(gpr, stream, buf, encodeWord);
		populateContents<word, ArchitectureConstants::AddressMax>(data, stream, buf, encodeWord);
		populateContents<dword, ArchitectureConstants::AddressMax>(instruction, stream, buf, encodeDword);
		populateContents<word, ArchitectureConstants::AddressMax>(stack, stream, buf, encodeWord);
		delete[] buf;
	}

	void Core::shutdown() {

	}

	void Core::dump(std::ostream& stream) {
		// save the registers
		char* buf = new char[4];
		auto decomposeWord = [](word v, char* buf) {
			buf[0] = (char)v;
			buf[1] = (char)(v >> 8);
			return buf;
		};
		auto decomposeDword = [](dword v, char* buf) {
			buf[0] = (char)v;
			buf[1] = (char)(v >> 8);
			buf[2] = (char)(v >> 16);
			buf[3] = (char)(v >> 24);
			return buf;
		};
		for(int i = 0; i < ArchitectureConstants::RegisterCount; ++i) {
			stream.write(decomposeWord(gpr[i], buf), 2);
		}
		for (int i = 0; i < ArchitectureConstants::AddressMax; ++i) {
			stream.write(decomposeWord(data[i], buf), 2);
		}
		for (int i = 0; i < ArchitectureConstants::AddressMax; ++i) {
			stream.write(decomposeDword(instruction[i], buf), 4);
		}
		for (int i = 0; i < ArchitectureConstants::AddressMax; ++i) {
			stream.write(decomposeWord(stack[i], buf), 2);
		}
		delete[] buf;
	}
	void Core::run() {
		while(execute) {
			current.decode(instruction[gpr[ArchitectureConstants::InstructionPointerIndex]]);
			dispatch();
			if (advanceIp) {
				gpr[ArchitectureConstants::InstructionPointerIndex]++;
			}
		}
	}
	void Core::dispatch() {
		switch(static_cast<InstructionGroup>(current.getGroup())) {
#define X(_, operation, tag) case tag: operation(); break; 
#include "target/iris16/groups.def"
#undef X
			default:
				std::cerr << "Illegal instruction group " << current.getGroup() << std::endl;
				execute = false;
				break;
		}
	}

	void Core::compare() {

	}
}
