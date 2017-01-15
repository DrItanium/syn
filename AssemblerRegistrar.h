#ifndef IRIS_ASM_INTERACT_H
#define IRIS_ASM_INTERACT_H
#include <string>
#include <cstdio>
#include <iostream>
#include <functional>
#include <map>
namespace syn {
	class AssemblerRegistrar {
		public:
			using OnEachEntry = std::function<void(const std::string&)>;
			using Operation = std::function<void(const std::string&, FILE*, std::ostream*)>;
			using AssemblerList = std::map<std::string, Operation>;
			AssemblerRegistrar();
			virtual ~AssemblerRegistrar();
			void assemble(const std::string& target, const std::string& inputFilePath, FILE* input, std::ostream* output);
			void forEachAssembler(OnEachEntry fn);
			void addToRegistry(const std::string& target, Operation op);
		private:
			AssemblerList assemblers;
	};

	extern AssemblerRegistrar assemblerRegistry;
}

#endif // end IRIS_ASM_INTERACT_H
