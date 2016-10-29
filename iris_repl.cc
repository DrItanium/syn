extern "C" {
	#include "clips.h"
}
#include "iris16.h"
#include "iris19.h"
#include "iris18.h"

static void *mainEnv;

int main(int argc, char* argv[]) {
	mainEnv = CreateEnvironment();
	// install features here
	iris16::installExtensions(mainEnv);
	iris18::installExtensions(mainEnv);
	iris19::installExtensions(mainEnv);
	RerouteStdin(mainEnv, argc, argv);
	CommandLoop(mainEnv);
	DestroyEnvironment(mainEnv);
	return -1;
}
