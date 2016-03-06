#include "iris.h"
#include <functional>

namespace iris16 {
	Core::Core() { }
	void Core::setInstructionMemory(word address, dword value) {
		instruction[address] = value;
	}
	void Core::setDataMemory(word address, word value) {
		data[address] = value;
	}
	void Core::initialize() {

	}
	void Core::shutdown() {

	}
	template<typename T, int count>
	void populateContents(T* contents, std::istream& stream, std::function<T(char*)> func) {
		char* buf = new char[sizeof(T)];
		for(int i = 0; i < count; ++i) {
			stream.read(buf, sizeof(T));
			contents[i] = func(buf);
		}
		delete[] buf;
	}
	void Core::installprogram(std::istream& stream) {
		auto encodeWord = [](char* buf) {
			return iris16::encodeWord(buf[0], buf[1]);
		};
		auto encodeDword = [](char* buf) {
			return iris16::encodeDword(buf[0], buf[1], buf[2], buf[3]);
		};
		populateContents<word, ArchitectureConstants::RegisterCount>(gpr, stream, encodeWord);
		populateContents<word, ArchitectureConstants::AddressMax>(data, stream, encodeWord);
		populateContents<dword, ArchitectureConstants::AddressMax>(instruction, stream, encodeDword);
		populateContents<word, ArchitectureConstants::AddressMax>(stack, stream, encodeWord);
	}

	template<typename T, int count>
	void dumpContents(T* contents, std::ostream& stream, std::function<char*(T,char*)> func) {
		char* buf = new char[sizeof(T)];
		for(int i = 0; i < count; ++i) {
			func(contents[i], buf);
			stream.write(buf, sizeof(T));
		}
		delete[] buf;
	}
	void Core::dump(std::ostream& stream) {
		// save the registers
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
		dumpContents<word, ArchitectureConstants::RegisterCount>(gpr, stream, decomposeWord);
		dumpContents<word, ArchitectureConstants::AddressMax>(data, stream, decomposeWord);
		dumpContents<dword, ArchitectureConstants::AddressMax>(instruction, stream, decomposeDword);
		dumpContents<word, ArchitectureConstants::AddressMax>(stack, stream, decomposeWord);
	}
	void Core::run() {
		while(execute) {
			if (!advanceIp) {
				advanceIp = true;
			}
			current.decode(instruction[gpr[ArchitectureConstants::InstructionPointerIndex]]);
			dispatch();
			if (advanceIp) {
				gpr[ArchitectureConstants::InstructionPointerIndex]++;
			} 
		}
	}
	void Core::dispatch() {
		switch(static_cast<InstructionGroup>(current.getGroup())) {
#define X(name, operation) case InstructionGroup:: name: operation(); break; 
#include "groups.def"
#undef X
			default:
				std::cerr << "Illegal instruction group " << current.getGroup() << std::endl;
				execute = false;
				break;
		}
	}
}
