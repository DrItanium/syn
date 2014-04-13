PROJECTS = iris

all: ${PROJECTS} 

iris:
	cd src/iris && ${MAKE}
