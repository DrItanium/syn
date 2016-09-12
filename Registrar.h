#ifndef Registrar_H
#define Registrar_H
#include <map>
#include <string>
#include <memory>
#include <sstream>
#include "Problem.h"
template<typename T>
class Registrar {
	public:
		using TypeDecl = T;
		using MapDecl = std::map<std::string, TypeDecl>;
		Registrar() : _backingStore(new MapDecl) { }
		~Registrar() = default;
		void add(const std::string& name, T value) {
			_backingStore->emplace(name, value);
		}
		T get(const std::string& name) {
			auto loc = _backingStore->find(name);
			if (loc != _backingStore->end()) {
				return loc->second;
			} else {
				std::stringstream stream;
				stream << "Unregistered name " << name<< "!!!";
				throw iris::Problem(stream.str());
			}
		}
	private:
		std::unique_ptr<MapDecl> _backingStore;

}; // end class Registrar

template<typename R>
struct RegisterAction {
	RegisterAction(R* registrar, const std::string& name, typename R::TypeDecl value) {
		registrar->add(name, value);
	}
	~RegisterAction() = default;
	RegisterAction(const RegisterAction&) = delete;
	RegisterAction(RegisterAction&&) = delete;
	RegisterAction(RegisterAction&) = delete;
};




#endif // end Registrar_H
