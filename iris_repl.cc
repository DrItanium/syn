extern "C" {
	#include "clips.h"
}
#include "iris_clips.h"

static void *mainEnv;

int main(int argc, char* argv[]) {
	mainEnv = CreateEnvironment();
	// install features here
	iris::installExtensions(mainEnv);
	RerouteStdin(mainEnv, argc, argv);
	CommandLoop(mainEnv);
	DestroyEnvironment(mainEnv);
	return -1;
}
