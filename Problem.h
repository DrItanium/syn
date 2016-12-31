// base class of all exceptions to be thrown
#ifndef IRIS_PROBLEM_H
#define IRIS_PROBLEM_H
#include <string>
namespace syn {

	class Problem {
		public:
			Problem(const std::string& message) : _msg(message) { }
			std::string what() const { return _msg; }
		private:
			std::string _msg;
	};
	
}

#endif // end IRIS_PROBLEM_H
