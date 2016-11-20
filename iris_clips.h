#ifndef __IRIS_CLIPS_H
#define __IRIS_CLIPS_H
#include "iris_base.h"
#include "Problem.h"
#include <map>
#include <memory>
extern "C" {
	#include "clips.h"
}

namespace iris {
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
				throw iris::Problem("unregistered external address type!");
			}
		}
		static void registerExternalAddressId(void* env, unsigned int value) {
			_cache.emplace(env, value);
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
		static std::string getSymbolicName() noexcept { return #name; } \
	}
#define DefMemoryBlockAssociation(t, name) DefWrapperSymbolicName( t [] , name )
	DefMemoryBlockAssociation(int8_t, word8s);
	DefMemoryBlockAssociation(uint8_t, word8u);
	DefMemoryBlockAssociation(int16_t, word16s);
	DefMemoryBlockAssociation(uint16_t, word16u);
	DefMemoryBlockAssociation(int32_t, word32s);
	DefMemoryBlockAssociation(uint32_t, word32u);
	DefMemoryBlockAssociation(int64_t, word64s);
	DefMemoryBlockAssociation(uint64_t, word64u);
#undef DefMemoryBlockAssociation

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

typedef void PrintFunction(void*, const char*, void*); 
typedef bool DeleteFunction(void*, void*);
typedef bool CallFunction(void*, DATA_OBJECT*, DATA_OBJECT*);
typedef void NewFunction(void*, DATA_OBJECT*);


template<typename T>
class ExternalAddressWrapper {
	public:
		using InternalType = T;
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
	public:
		ExternalAddressWrapper(std::unique_ptr<T>&& value) : _value(std::move(value)) { }
		inline T* get() const noexcept { return _value.get(); }
	protected:
		std::unique_ptr<T> _value;
};

template<typename Word>
class ManagedMemoryBlock : public ExternalAddressWrapper<Word[]> {
	public:
		ManagedMemoryBlock(CLIPSInteger capacity) :
			ExternalAddressWrapper<Word[]>(std::move(std::make_unique<Word[]>(capacity))), _capacity(capacity) { }
		inline CLIPSInteger size() const noexcept                                    { return _capacity; }
		inline bool legalAddress(CLIPSInteger idx) const noexcept                    { return inRange<CLIPSInteger>(_capacity, idx); }
		static inline ManagedMemoryBlock<Word>* make(CLIPSInteger capacity) noexcept { return new ManagedMemoryBlock<Word>(capacity); }
		inline Word getMemoryCellValue(CLIPSInteger addr) noexcept                   { return this->_value.get()[addr]; }
		inline void setMemoryCell(CLIPSInteger addr0, Word value) noexcept           { this->_value.get()[addr0] = value; }
		inline void swapMemoryCells(CLIPSInteger addr0, CLIPSInteger addr1) noexcept { swap<Word>(this->_value.get()[addr0], this->_value.get()[addr1]); }
		inline void decrementMemoryCell(CLIPSInteger address) noexcept               { --this->_value.get()[address]; }
		inline void incrementMemoryCell(CLIPSInteger address) noexcept               { ++this->_value.get()[address]; }

		inline void copyMemoryCell(CLIPSInteger from, CLIPSInteger to) noexcept {
			auto ptr = this->_value.get();
			ptr[to] = ptr[from];
		}
		inline void setMemoryToSingleValue(Word value) noexcept {
			auto ptr = this->_value.get();
			for (CLIPSInteger i = 0; i < _capacity; ++i) {
				ptr[i] = value;
			}
		}
	private:
		CLIPSInteger _capacity;
};

template<typename Word>
using ManagedMemoryBlock_Ptr = ManagedMemoryBlock<Word>*;

bool errorMessage(void* env, const std::string& idClass, int idIndex, const std::string& msgPrefix, const std::string& msg) noexcept;

 }
 #endif
