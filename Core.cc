#include "Core.h"

namespace iris {

void Core::run() {
	while(execute) {
		execute = cycle();
	}
}
} // end namespace iris

