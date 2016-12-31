#include "Core.h"

namespace stdiris {

void Core::run() {
	while(execute) {
		execute = cycle();
	}
}
} // end namespace stdiris

