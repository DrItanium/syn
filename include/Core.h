#ifndef _IRIS_CORE_H
#define _IRIS_CORE_H
#include <iostream>
#include <typeinfo>
namespace iris {
	class Core;
	template<typename T = Core> 
	struct Describe {
		static void describe(std::ostream& out) {
			out << typeid(T).name();
		}
		friend std::ostream& operator<<(std::ostream& out, const Describe<T>& c) {
			describe(out);
			return out;
		}
	};
	 // Generic iris core interface
	 class Core {
		public:
			virtual void initialize() = 0;
			virtual void installprogram(std::istream& stream) = 0;
			virtual void shutdown() = 0;
			virtual void dump(std::ostream& stream) = 0;
			virtual void run() = 0;
			// descriptor stream
			friend std::ostream& operator<<(std::ostream& out, const Core& c) 
			{
				c.output(out);
				return out;
			}
		private:
			// yes this can be overriden in subclasses >_< but not called :D
			virtual void output( std::ostream& ) const = 0;
	 };
}
#endif
