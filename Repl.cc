extern "C" {
	#include "clips.h"
}
#include "syn_clips.h"

static void *mainEnv;

int main(int argc, char* argv[]) {
	mainEnv = CreateEnvironment();
	// install features here
	syn::installExtensions(mainEnv);
	RerouteStdin(mainEnv, argc, argv);
	CommandLoop(mainEnv);
	DestroyEnvironment(mainEnv);
	return -1;
}
