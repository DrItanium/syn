// the phoenix system is a machine based off of the iris17 cpu
// it provides a 512 mb disk and a 4 mb framebuffer 
// The display provided is a 512 x 384 pixel display
#ifndef __PHOENIX_H__
#define __PHOENIX_H__

#include "Core.h"
#include "iris17.h"
#include <SFML/Window.hpp>

namespace Phoenix {
class Machine : public iris::Core {
	public:
		using CPU = iris17::Core;
		static constexpr int WindowWidth = 512;
		static constexpr int WindowHeight = 384;
	public:
		Machine();
		virtual ~Machine();
		virtual void initialize() override;
		virtual void installprogram(std::istream& stream) override;
		virtual void shutdown() override;
		virtual void dump(std::ostream& stream) override;
		virtual void run() override;
		virtual void link(std::istream& stream) override;
	private:
		enum CustomHandlers {
			DrawPixel = CPU::DefaultHandlers::Count,


			Count,
		};
		static_assert(static_cast<int>(CustomHandlers::Count) <= static_cast<int>(iris17::ArchitectureConstants::MaxSystemCalls), "Too many system calls defined!");
	private:
		static void drawPixel(CPU* cpu, iris17::DecodedInstruction&& inst);
	private:
		std::unique_ptr<CPU> _cpu;
		sf::Window _window;
};
} // end namespace Phoenix

#endif // end __PHOENIX_H__
