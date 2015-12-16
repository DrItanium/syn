#include "target/iris16/iris.h"
#include <functional>

namespace iris16 {
	Core::Core() { }
	void Core::setInstructionMemory(word address, dword value) {
		instruction[address] = value;
	}
	void Core::setDataMemory(word address, word value) {
		instruction[address] = value;
	}
	void Core::initialize() {

	}
	void Core::shutdown() {

	}
	template<typename T, int count>
	void populateContents(T* contents, std::istream& stream, char* buf, std::function<T(std::istream&, char*)> func) {
		for(int i = 0; i < count; ++i) {
			contents[i] = func(stream, buf);
		}
	}
	void Core::installprogram(std::istream& stream) {
		char* buf = new char[sizeof(dword)];
		auto encodeWord = [](std::istream& s, char* buf) {
			s.read(buf, sizeof(word));
			return iris16::encodeWord(buf[0], buf[1]);
		};
		auto encodeDword = [](std::istream& s, char* buf) {
			s.read(buf, sizeof(dword));
			return iris16::encodeDword(buf[0], buf[1], buf[2], buf[3]);
		};
		populateContents<word, ArchitectureConstants::RegisterCount>(gpr, stream, buf, encodeWord);
		populateContents<word, ArchitectureConstants::AddressMax>(data, stream, buf, encodeWord);
		populateContents<dword, ArchitectureConstants::AddressMax>(instruction, stream, buf, encodeDword);
		populateContents<word, ArchitectureConstants::AddressMax>(stack, stream, buf, encodeWord);
		delete[] buf;
	}

	template<typename T, int count>
	void dumpContents(T* contents, std::ostream& stream, char* buf, std::function<char*(T,char*)> func) {
		for(int i = 0; i < count; ++i) {
			stream.write(func(contents[i], buf), sizeof(T));
		}
	}
	void Core::dump(std::ostream& stream) {
		// save the registers
		char* buf = new char[sizeof(dword)];
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
		dumpContents<word, ArchitectureConstants::RegisterCount>(gpr, stream, buf, decomposeWord);
		dumpContents<word, ArchitectureConstants::RegisterCount>(data, stream, buf, decomposeWord);
		dumpContents<dword, ArchitectureConstants::RegisterCount>(instruction, stream, buf, decomposeDword);
		dumpContents<word, ArchitectureConstants::RegisterCount>(stack, stream, buf, decomposeWord);
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
}
