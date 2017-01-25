#ifndef __IRIS_CLIPS_H
#define __IRIS_CLIPS_H
#include "Base.h"
#include "Problem.h"
#include <map>
#include <memory>
#include <sstream>
extern "C" {
	#include "clips.h"
}

namespace syn {
void installExtensions(void* theEnv);

template<typename T>
struct ExternalAddressRegistrar {
	public:
		ExternalAddressRegistrar() = delete;
		~ExternalAddressRegistrar() = delete;
		ExternalAddressRegistrar(const ExternalAddressRegistrar&) = delete;
		ExternalAddressRegistrar(ExternalAddressRegistrar&&) = delete;
		static unsigned int getExternalAddressId(void* env) {
			auto found = _cache.find(env);
			if (found != _cache.end()) {
				return found->second;
			} else {
				throw syn::Problem("unregistered external address type!");
			}
		}
		static void registerExternalAddressId(void* env, unsigned int value) {
			_cache.emplace(env, value);
		}
		static bool isOfType(void* env, DATA_OBJECT* ptr) {
			return static_cast<struct externalAddressHashNode*>(ptr->value)->type == getExternalAddressId(env);
		}
	private:
		static std::map<void*, unsigned int> _cache;
};


template<typename T>
std::map<void*, unsigned int> ExternalAddressRegistrar<T>::_cache;

template<typename T>
inline constexpr bool inRange(T capacity, T address) noexcept {
	return address >= 0 && address < capacity;
}

template<typename T> struct TypeToName { };
#define DefWrapperSymbolicName(t, name) \
	template<> \
	struct TypeToName < t > { \
		static std::string getSymbolicName() noexcept { return name; } \
	}


void CLIPS_basePrintAddress(void* env, const char* logicalName, void* theValue, const char* func, const char* majorType);

template<typename T>
void CLIPS_basePrintAddress(void* env, const char* logicalName, void* theValue) {
	static bool init = true;
	static std::string func;
	if (init) {
		init = false;
		func = TypeToName<T>::getSymbolicName();
	}
	CLIPS_basePrintAddress(env, logicalName, theValue, func.c_str(), "Wrapper");
}

// Have to do it this way because std::function's will go out of scope and
// everything breaks
typedef void PrintFunction(void*, const char*, void*); 
typedef bool DeleteFunction(void*, void*);
typedef bool CallFunction(void*, DATA_OBJECT*, DATA_OBJECT*);
typedef void NewFunction(void*, DATA_OBJECT*);


template<typename T>
class ExternalAddressWrapper {
	public:
		using InternalType = T;
		using BaseClass = ExternalAddressWrapper<T>;
		static std::string getType() { return TypeToName<InternalType>::getSymbolicName(); }
		static unsigned int getAssociatedEnvironmentId(void* env) { return ExternalAddressRegistrar<InternalType>::getExternalAddressId(env); }
		static void registerWithEnvironment(void* env, externalAddressType* description) { 
			ExternalAddressRegistrar<InternalType>::registerExternalAddressId(env, InstallExternalAddressType(env, description));
		}
		static void printAddress(void* env, const char* logicalName, void* theValue) {
			CLIPS_basePrintAddress<InternalType>(env, logicalName, theValue);
		}
		static bool deleteWrapper(void* env, void* obj) {
			if (obj != nullptr) {
				auto result = static_cast<ExternalAddressWrapper<T>*>(obj);
				delete result;
			}
			return true;
		}
		static void registerWithEnvironment(void* env, const char* title, NewFunction _new, CallFunction _call, DeleteFunction _delete = deleteWrapper, PrintFunction _print = printAddress) {

			externalAddressType tmp = { 
				title,
				_print,
				_print,
				_delete,
				_new,
				_call,
			};
			registerWithEnvironment(env, &tmp);
		}
		static bool isOfType(void* env, DATA_OBJECT* ptr) {
			return ExternalAddressRegistrar<InternalType>::isOfType(env, ptr);
		}
	public:
		ExternalAddressWrapper(std::unique_ptr<T>&& value) : _value(std::move(value)) { }
		inline T* get() const noexcept { return _value.get(); }
	protected:
		std::unique_ptr<T> _value;
};


}
#endif
