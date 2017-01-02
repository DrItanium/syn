#ifndef SYN_REGISTER_ENTRY_H
#define SYN_REGISTER_ENTRY_H
#include <string>
namespace syn {

	template<typename R, typename T>
	class RegisterEntry {
		public:
			RegisterEntry(R& reg, const std::string& name, typename R::Operation op) {
				reg.addToRegistry(name, op);
			}
	};
}

#endif // end SYN_REGISTER_ENTRY_H
