#include "Phoenix.h"

namespace Phoenix {
	Machine::Machine() : _cpu(new Machine::CPU) { }

	Machine::~Machine() { }

	void Machine::initialize() {
		_cpu->initialize();
		// setup the custom handlers
		_cpu->installSystemHandler(CustomHandlers::DrawPixel, Machine::drawPixel);

		_window.create(sf::VideoMode(Machine::WindowWidth, Machine::WindowHeight), "Phoenix Machine");
		_window.setFramerateLimit(75);
	}

	void Machine::drawPixel(CPU* cpu, iris17::DecodedInstruction&& current) {

	}

	void Machine::shutdown() {
		_cpu->shutdown();
	}

	void Machine::installprogram(std::istream& stream) {
		_cpu->installprogram(stream);
	}

	void Machine::dump(std::ostream& stream) {
		_cpu->dump(stream);
	}

	void Machine::run() {
		while (_window.isOpen()) {
			_cpu->cycle();
			if (!_cpu->shouldExecute()) {
				_window.close();
				break;
			}
			sf::Event evt;
			while (_window.pollEvent(evt)) {
				if (evt.type == sf::Event::Closed) {
					_window.close();
				}
			}
		}
	}

	void Machine::link(std::istream& stream) {
		_cpu->link(stream);
	}


}
