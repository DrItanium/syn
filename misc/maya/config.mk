CC := cc
OUTPUT := maya
PREFIX := /usr/local
GENERIC_DEFINES := -DMAXIMUM_EXTERNAL_ADDRESS_TYPES=256
GENFLAGS := -g3 -Os
CFLAGS := ${GENFLAGS} -std=c99
LDFLAGS := -lm -lrt
CXXEXTENSIONS ?= TRUE
ifeq ($(CXXEXTENSIONS), TRUE)
	CXX := c++
	LDFLAGS += -lboost_system -lboost_filesystem
	CXXFLAGS := ${GENFLAGS} -std=c++11
	LD := $(CXX)
else
	CFLAGS += -DBOOST_EXTENSIONS=0 -DFUNCTIONAL_EXTENSIONS=0
	LD := $(CC)
endif
COMMAND_PROMPT := "maya> "
BANNER_STRING := "\"     maya (based off of CLIPS \" VERSION_STRING \" \" CREATION_DATE_STRING \". Built on \" __DATE__ \" at \" __TIME__ \")\n\""

