#include "Termbox.h"
#include <iostream>
#include "Base.h"
#include <sstream>
#include <list>

// maximum of 10 lines!
std::list<std::string> lines;

void printLine(const std::string& str) {
	if (lines.size() == 10) {
		lines.pop_back();
	}
	lines.push_front(str);
}

void printLine(const char* str) {
	std::string tmp(str);
	printLine(tmp);
}
void printLine(const std::stringstream& str) {
	auto tmp = str.str();
	printLine(tmp);
}
void printLines(termbox::Screen& scr) {
	// one statement per line!
	auto y = 1;
	for (const auto& line : lines) {
		scr.printLine(line, 1, y, termbox::Color::Green, termbox::Color::Black);
		++y;
	}
}
int main(int argc, char** argv) {
	auto ret = termbox::init();
	if (termbox::errorOccurred(ret)) {
		std::cerr << "termbox initialization failed with error code 0x" << std::hex << ret << std::endl;
		return 1;
	}


	termbox::RawEvent evt;
	std::stringstream ss;
	termbox::setClearAttributes(termbox::Color::Black, termbox::Color::Black);
	termbox::Screen screen;
	bool done = false;
	while (!syn::isErrorState(termbox::pollEvent(evt))) {
		ss.clear();
		termbox::Event e(evt);
		switch(e.getType()) {
			case termbox::EventType::Key:
				ss << "Pressed ";
				switch(e.getKey()) {
					case termbox::keyEscape:
						ss << "Esc";
						done = true;
						break;
					case termbox::keyF1:
						ss << "F1";
						break;
					case termbox::keyF2:
						ss << "F2";
						break;
					default:
						ss << "Something???";
						break;
				}
				printLine(ss);
				break;
			case termbox::EventType::Mouse:
				printLine("Mouse Event\n");
				break;
			case termbox::EventType::Resize:
				printLine("Resize Event\n");
				screen.resize();
				break;
			default:
				printLine("Error event!?");
				done = true;
				break;
		}
		printLines(screen);
		screen.present();
		if (done) {
			break;
		}
		screen.clear();
	}
	termbox::shutdown();
	return 0;
}
