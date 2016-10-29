extern "C" {
	#include "clips.h"
}
#include "iris16.h"
#include "iris19.h"

static void *mainEnv;

int main(int argc, char* argv[]) {
	mainEnv = CreateEnvironment();
	// install features here
	iris19::installExtensions(mainEnv);
	iris16::installExtensions(mainEnv);
	RerouteStdin(mainEnv, argc, argv);
	CommandLoop(mainEnv);
	DestroyEnvironment(mainEnv);
	return -1;
}
