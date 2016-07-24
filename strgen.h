// strgen related operations
#ifndef IRIS_STRGEN_H
#define IRIS_STRGEN_H
#include "architecture.h"
#include <iostream>
namespace iris {
	void constructWord(Architecture target, std::ostream& out, char c);
	template<Architecture arch>
	void getWordDescription(std::ostream& out) {
		// do nothing
		throw "unimplemented word description";
	}
	
}

#endif // end IRIS_STRGEN_H
