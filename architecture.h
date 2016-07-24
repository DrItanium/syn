// architecture related definitions

#ifndef IRIS_ARCHITECTURE
#define IRIS_ARCHITECTURE

namespace iris {
	enum class Architecture {
#define X(name, str, cl) name, 
	#include "architecture_registrations.def"
#undef X
	};
}

#endif // end IRIS_ARCHITECTURE
