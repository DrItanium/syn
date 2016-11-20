#ifndef __IRIS_CLIPS_H
#define __IRIS_CLIPS_H
#include "iris_base.h"
#include "Problem.h"
#include <map>
#include <memory>
#include <sstream>
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
	public:
		ExternalAddressWrapper(std::unique_ptr<T>&& value) : _value(std::move(value)) { }
		inline T* get() const noexcept { return _value.get(); }
	protected:
		std::unique_ptr<T> _value;
};

template<typename Word>
class ManagedMemoryBlock : public ExternalAddressWrapper<Word[]> {
	public:
		using Self = ManagedMemoryBlock<Word>;
		static ManagedMemoryBlock<Word>* make(CLIPSInteger capacity) noexcept;
		static void newFunction(void* env, DATA_OBJECT* ret); 
		static bool callFunction(void* env, DATA_OBJECT* value, DATA_OBJECT* ret);
		static void registerWithEnvironment(void* env, const char* title) { Self::BaseClass::registerWithEnvironment(env, title, newFunction, callFunction); }
	public:
		ManagedMemoryBlock(CLIPSInteger capacity) :
			ExternalAddressWrapper<Word[]>(std::move(std::make_unique<Word[]>(capacity))), _capacity(capacity) { }
		inline CLIPSInteger size() const noexcept                                    { return _capacity; }
		inline bool legalAddress(CLIPSInteger idx) const noexcept                    { return inRange<CLIPSInteger>(_capacity, idx); }
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
		static std::string _type;
};

template<typename Word>
std::string ManagedMemoryBlock<Word>::_type = ManagedMemoryBlock<Word>::getType();

template<typename Word>
using ManagedMemoryBlock_Ptr = ManagedMemoryBlock<Word>*;

bool errorMessage(void* env, const std::string& idClass, int idIndex, const std::string& msgPrefix, const std::string& msg) noexcept;


template<typename Word>
ManagedMemoryBlock<Word>* ManagedMemoryBlock<Word>::make(CLIPSInteger capacity) noexcept {
		return new ManagedMemoryBlock<Word>(capacity); 
}
template<typename Word>
void ManagedMemoryBlock<Word>::newFunction(void* env, DATA_OBJECT* ret) {
	static bool init = false;
	static std::string funcStr;
	static std::string funcErrorPrefix;
	if (init) {
		init = false;
		std::stringstream ss, ss2;
		ss << "new (" << _type << " memory block)";
		funcStr = ss.str();
		ss2 << "Function " << funcStr;
		funcErrorPrefix = ss2.str();
	}

	try {
		if (EnvRtnArgCount(env) == 2) {
			CLIPSValue capacity;
			if (EnvArgTypeCheck(env, funcStr.c_str(), 2, INTEGER, &capacity) == FALSE) {
				CVSetBoolean(ret, false);
				errorMessage(env, "NEW", 1, funcErrorPrefix, " expected an integer for capacity!");
			} else {
				auto size = EnvDOToLong(env, capacity);
				auto idIndex = ManagedMemoryBlock<Word>::getAssociatedEnvironmentId(env);
				ret->bitType = EXTERNAL_ADDRESS_TYPE;
				SetpType(ret, EXTERNAL_ADDRESS);
				SetpValue(ret, EnvAddExternalAddress(env, ManagedMemoryBlock<Word>::make(size), idIndex));
			}
		} else {
			errorMessage(env, "NEW", 1, funcErrorPrefix, " function new expected no arguments besides type!");
			CVSetBoolean(ret, false);
		}
	} catch(iris::Problem p) {
		CVSetBoolean(ret, false);
		std::stringstream s;
		s << "an exception was thrown: " << p.what();
		auto str = s.str();
		errorMessage(env, "NEW", 2, funcErrorPrefix, str);
	}
}

template<typename Word>
bool ManagedMemoryBlock<Word>::callFunction(void* env, DATA_OBJECT* value, DATA_OBJECT* ret) {
	static bool init = true;
	static std::string funcStr;
	static std::string funcErrorPrefix;
	if (init) {
		init = false;
		std::stringstream ss, ss2;
		ss << "call (" << _type << " memory block)";
		funcStr = ss.str();
		ss2 << "Function " << funcStr;
		funcErrorPrefix = ss2.str();
	}
	if (GetpType(value) == EXTERNAL_ADDRESS) {
		auto ptr = static_cast<ManagedMemoryBlock_Ptr<Word>>(DOPToExternalAddress(value));
#define argCheck(storage, position, type) EnvArgTypeCheck(env, funcStr.c_str(), position, type, storage)
		auto callErrorMessage = [env, ret](const std::string& subOp, const std::string& rest) {
			CVSetBoolean(ret, false);
			std::stringstream stm;
			stm << " " << subOp << ": " << rest << std::endl;
			auto msg = stm.str();
			return errorMessage(env, "CALL", 3, funcErrorPrefix, msg);
		};
		auto errOutOfRange = [callErrorMessage, env, ret](const std::string& subOp, CLIPSInteger capacity, CLIPSInteger address) {
			std::stringstream ss;
			ss << funcErrorPrefix << ": Provided address " << std::hex << address << " is either less than zero or greater than " << std::hex << capacity << std::endl;
			return callErrorMessage(subOp, ss.str());
		};
		CLIPSValue operation;
		if (EnvArgTypeCheck(env, funcStr.c_str(), 2, SYMBOL, &operation) == FALSE) {
			return errorMessage(env, "CALL", 2, funcErrorPrefix, "expected a function name to call!");
		} else {
			std::string str(EnvDOToString(env, operation));
			CVSetBoolean(ret, true);
			auto argc = EnvRtnArgCount(env);
			if (argc == 2) {
				if (str == "clear") {
					ptr->setMemoryToSingleValue(static_cast<Word>(0));
				} else if (str == "type") {
					// get the type of the current thing!
					CVSetSymbol(ret, _type.c_str());
				} else if (str == "size") {
					CVSetInteger(ret, ptr->size());
				} else {
					return callErrorMessage(str, "<- unknown operation requested!");
				}
			} else if (argc == 3) {
				CLIPSValue arg0;
				auto oneCheck = [callErrorMessage, &arg0, env, &str](unsigned int type, const std::string& msg) {
					if (!argCheck(&arg0, 3, type)) {
						return callErrorMessage(str, msg);
					} else {
						return true;
					}
				};
				if (str == "get") {
					if (oneCheck(INTEGER, "Argument 0 must be an address!")) {
						auto addr = EnvDOToLong(env, arg0);
						if (!ptr->legalAddress(addr)) {
							errOutOfRange("get", ptr->size(), addr);
						} else {
							CVSetInteger(ret, static_cast<CLIPSInteger>(ptr->getMemoryCellValue(addr)));
						}
					} else {
						return false;
					}
				} else if (str == "populate") {
					if (oneCheck(INTEGER, "First argument must be a value to populate all of the memory cells with!")) {
						ptr->setMemoryToSingleValue(static_cast<Word>(EnvDOToLong(env, arg0)));
					} else {
						return false;
					}
				} else if (str == "increment") {
					if (oneCheck(INTEGER, "First argument must an address")) {
						auto addr = EnvDOToLong(env, arg0);
						if (!ptr->legalAddress(addr)) {
							errOutOfRange("increment", ptr->size(), addr);
						} else {
							ptr->decrementMemoryCell(addr);
						}
					} else {
						return false;
					}
				} else if (str == "decrement") {
					if (oneCheck(INTEGER, "First argument must be an address")) {
						auto addr = EnvDOToLong(env, arg0);
						if (!ptr->legalAddress(addr)) {
							errOutOfRange("decrement", ptr->size(), addr);
						} else {
							ptr->incrementMemoryCell(addr);
						}
					} else {
						return false;
					}
				} else {
					return callErrorMessage(str, "<- unknown operation requested!");
				}
			} else if (argc == 4) {
				CLIPSValue arg0, arg1;
				auto twoCheck = [callErrorMessage, &arg0, &arg1, env, &str](unsigned int type0, const std::string& msg0, unsigned int type1, const std::string& msg1) {
					if (!argCheck(&arg0, 3, type0)) {
						return callErrorMessage(str, msg0);
					} else if (!argCheck(&arg1, 4, type1)) {
						return callErrorMessage(str, msg1);
					} else {
						return true;
					}
				};
				if (str == "set") {
					if (twoCheck(INTEGER, "First argument must be address!", INTEGER, "Second argument must be an integer!")) {
						auto addr = EnvDOToLong(env, arg0);
						auto value = static_cast<Word>(EnvDOToLong(env, arg1));
						if (!ptr->legalAddress(addr)) {
							errOutOfRange("set", ptr->size(), addr);
						} else {
							ptr->setMemoryCell(addr, value);
						}
					} else {
						return false;
					}
				} else if (str == "swap") {
					if (twoCheck(INTEGER, "First argument must be an address", INTEGER, "Second argument must be an address")) {
						auto addr0 = EnvDOToLong(env, arg0);
						auto addr1 = EnvDOToLong(env, arg1);
						if (!ptr->legalAddress(addr0)) {
							errOutOfRange("swap", ptr->size(), addr0);
						} else if (!ptr->legalAddress(addr1)) {
							errOutOfRange("swap", ptr->size(), addr1);
						} else {
							ptr->swapMemoryCells(addr0, addr1);
						}
					} else {
						return false;
					}
				} else if (str == "move") {
					// move the contents of one address to another in the same
					// memory space
					if (twoCheck(INTEGER, "First argument must be a source address", INTEGER, "Second argument must be a destination address")) {
						auto srcAddr = EnvDOToLong(env, arg0);
						auto destAddr = EnvDOToLong(env, arg1);
						if (!ptr->legalAddress(srcAddr)) {
							errOutOfRange("move", ptr->size(), srcAddr);
						} else if (!ptr->legalAddress(destAddr)) {
							errOutOfRange("move", ptr->size(), destAddr);
						} else {
							ptr->copyMemoryCell(srcAddr, destAddr);
						}
					} else {
						return false;
					}
				} else {
					return callErrorMessage(str, "<- unknown operation requested!");
				}
			} else {
				return callErrorMessage(str, "<- unknown operation requested!");
			}
		}
		return true;
	} else {
		return errorMessage(env, "CALL", 1, funcErrorPrefix, "Function call expected an external address as the first argument!");
	}
#undef argCheck
}

}
#endif
