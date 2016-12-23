/**
 * The concept of an IO devices which is mapped into memory
 */
#ifndef IRIS_IO_CONTROLLER_H_
#define IRIS_IO_CONTROLLER_H_
#include <tuple>
#include <functional>
#include <memory>
#include <vector>
#include "Problem.h"
#include "Device.h"
#include "IODevice.h"

namespace iris {

template<typename D, typename A = D>
class IOController : public IODevice<D, A> {
	public:
		using IONode = IODevice<D, A>;
		using SharedIONodePtr = std::shared_ptr<IONode>;
	public:
		IOController(A base, A length) : IODevice<D, A>(base, length) { }
		virtual ~IOController() {
			for (auto & dev : _devices) {
				dev->shutdown();
			}

		}
		virtual D read(A addr) override {
			if (this->respondsTo(addr)) {
				auto caddr = addr - this->baseAddress(); // make sure that we adjust it to be a "flat" model
				auto dev = findResponsiveChild(caddr);
				if (dev) {
					return dev->read(caddr);
				} else {
					throw iris::Problem("Provided address is not mapped to anything");
				}
			} else {
				throw iris::Problem("Provided base address is out of range!");
			}
		}
		virtual void write(A addr, D value) override {
			if (this->respondsTo(addr)) {
				auto caddr = addr - this->baseAddress(); // make sure that we adjust it to be a "flat" model
				auto dev = findResponsiveChild(caddr);
				if (dev) {
					dev->write(caddr, value);
				} else {
					throw iris::Problem("Provided address is not mapped to anything");
				}
			} else {
				throw iris::Problem("Provided base address is out of range!");
			}
		}
		void install(SharedIONodePtr ptr) {
			if (ptr->baseAddress() < this->baseAddress()) {
				throw iris::Problem("Base address of provided device starts out of range of the IO space!");
			} else if (ptr->baseAddress() > this->endAddress()) {
				throw iris::Problem("Base address of provided device starts out after the range of the IO space!");
			} else if (ptr->endAddress() > this->endAddress()) {
				throw iris::Problem("End address of provided device ends beyond the IO space!");
			} else if (ptr->endAddress() < this->baseAddress()) {
				throw iris::Problem("End address of provided device ends before the beginning of IO space!");
			} else {
				if (childrenRespondTo(ptr)) {
					throw iris::Problem("Provided device installation will interfere with already installed device!");
				}
				_devices.emplace_back(ptr);
				ptr->initialize();
			}
		}
	private:
		bool childrenRespondTo(const SharedIONodePtr& ptr) {
			return childrenRespondTo(ptr->baseAddress(), ptr->size());
		}
		bool childrenRespondTo(A addr, A length = 1) {
			return static_cast<bool>(findResponsiveChild(addr, length));
		}
		SharedIONodePtr findResponsiveChild(A addr, A length = 1) {
			for (auto & dev : _devices) {
				if (dev->respondsTo(addr, length)) {
					return dev;
				}
			}
			return std::shared_ptr<IODevice<D, A>>();
		}
	private:
		std::vector<SharedIONodePtr> _devices;
};

template<typename D, typename A = D>
using MemoryController = IOController<D, A>;

} // end namespace iris
#endif // end IRIS_IO_CONTROLLER_H_
