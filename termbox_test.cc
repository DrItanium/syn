#include "Termbox.h"
#include <iostream>
#include "Base.h"


int main(int argc, char** argv) {
	auto ret = termbox::init();
	if (termbox::errorOccurred(ret)) {
		std::cerr << "termbox initialization failed with error code 0x" << std::hex << ret << std::endl;
		return 1;
	}


	termbox::RawEvent evt;
	bool done = false;
	while (!syn::isErrorState(termbox::pollEvent(evt))) {
		if (done) {
			break;
		}
		termbox::Event e(evt);
		switch(e.getType()) {
			case termbox::EventType::Key:
				std::cout << "Pressed ";
				switch(e.getKey()) {
					case termbox::keyEscape:
						std::cout << "Esc";
						done = true;
						break;
					case termbox::keyF1:
						std::cout << "F1";
						break;
					case termbox::keyF2:
						std::cout << "F2";
						break;
					default:
						std::cout << "Something???";
						break;
				}
				std::cout << std::endl;
				break;
			case termbox::EventType::Mouse:
				std::cout << "Mouse Event" << std::endl;
				break;
			case termbox::EventType::Resize:
				std::cout << "Resize event!" << std::endl;
				break;
			default:
				std::cout << "Error event!?" << std::endl;
				done = true;
				break;
		}
	}
	termbox::shutdown();
	return 0;
}
