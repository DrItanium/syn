PROJECTS = iris

all: platform_info ${PROJECTS} 

iris:
	cd src/iris && ${MAKE}

platform_info:
	@echo platform info:
	@uname -a

