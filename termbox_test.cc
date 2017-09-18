#include "Termbox.h"
#include "Base.h"
#include <sstream>
#include <list>

// maximum of 10 lines!
std::list<std::string> lines;
size_t maxLines() noexcept {
	return static_cast<size_t>(termbox::getHeight() - 10);
}
void printLine(const std::string& str) {
	while(lines.size() >= maxLines()) {
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
void printLines(termbox::Screen& scr, std::stringstream& dimensions) {
	// one statement per line!
	scr.printLine(dimensions, 0, 0, termbox::Color::Green, termbox::Color::Black);
	auto y = 1;
	for (const auto& line : lines) {
		scr.printLine(line, 0, y, termbox::Color::Green, termbox::Color::Black);
		++y;
	}
}
void updateDimensions(termbox::Screen& screen, std::stringstream& ss) {
	ss << "Width: " << screen.getWidth() << " Height: " << screen.getHeight() << std::endl;
}
int main(int argc, char** argv) {
	auto ret = termbox::init();
	if (termbox::errorOccurred(ret)) {
		//std::cerr << "termbox initialization failed with error code 0x" << std::hex << ret << std::endl;
		return 1;
	}

	termbox::setInputMode(termbox::InputMode::EscMouse);
	termbox::RawEvent evt;
	std::stringstream ss, dimensions;
	termbox::setClearAttributes(termbox::Color::Black, termbox::Color::Black);
	termbox::Screen screen;
	bool done = false;
	// still getting strange artifacts at this point but it's much better
	updateDimensions(screen, dimensions);
	printLines(screen,dimensions);
	screen.present();
	screen.clear();
	while (!syn::isErrorState(termbox::pollEvent(evt))) {
		screen.present();
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
					case termbox::keyF3:
						ss << "F3";
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
				screen.clear();
				screen.present();
				dimensions.str("");
				updateDimensions(screen, dimensions);
				break;
			default:
				printLine("Error event!?");
				done = true;
				break;
		}
		if (done) {
			break;
		}
		printLines(screen, dimensions);
		screen.present();
		screen.clear();
		ss.str("");
	}
	termbox::shutdown();
	return 0;
}
