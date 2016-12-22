/**
 * The concept of a devices which is initialized and shutdown
 */
#ifndef IRIS_DEVICE_H_
#define IRIS_DEVICE_H_
namespace iris {
class Device {
	public:
		virtual void initialize() = 0;
		virtual void shutdown() = 0;
		inline bool debugEnabled() const { return _debug; }
		void toggleDebug() 				 { _debug = !_debug; }
	private:
		bool _debug = false;

};
} // end namespace iris
#endif // end IRIS_DEVICE_H_
