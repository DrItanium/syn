#include "Core.h"

namespace syn {

void Core::run() {
	while(execute) {
		execute = cycle();
	}
}
} // end namespace syn

