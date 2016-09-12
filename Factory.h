/// A c++11 implementation of the factory pattern
#ifndef FACTORY_H
#define FACTORY_H
#include <functional>
#include "Registrar.h"

template<typename T>
using Factory = Registrar<std::function<T*()>>;

template<typename T>
using FactoryRegister = RegisterAction<Factory<T>>;


#endif // end FACTORY_H


