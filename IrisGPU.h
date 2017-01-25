// an iris core masked as a planar operation
#ifndef IRIS_GPU_H
#define IRIS_GPU_H
#include "Device.h"
#include "IrisCore.h"
namespace iris {
    template<typename Bus>
    class GPU : public syn::Device {
        public:
            GPU(std::shared_ptr<Core> reference) : needsShutdown(false), callback(std::move(reference)) { }
            virtual ~GPU() {
                shutdown();
            }
            virtual void shutdown() override {
                if (needsShutdown) {
                    needsShutdown = false;
                    r.shutdown();
                    g.shutdown();
                    b.shutdown();
                    i.shutdown();
                }
            }
            virtual void initialize() override {
                if (!needsShutdown) {
                    needsShutdown = true;
                    r.initialize();
                    g.initialize();
                    b.initialize();
                    i.initialize();
                }
            }
        private:
            bool needsShutdown;
            std::shared_ptr<Core> callback;
            Core r;
            Core g;
            Core b;
            Core i;
    };

} // end namespace iris
#endif
