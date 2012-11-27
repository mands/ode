# Simple makefile for Ode Tools
CC            = clang
CFLAGS        = -std=c11 -O3 -pedantic -Weverything -integrated-as -fno-math-errno -ffast-math
LDLIBS        = -lm
INC_DIRS      = -I$(ODE_LIB_DIR) -I$(PROJ_INC_DIR)
LIB_DIRS      = -L$(PROJ_LIB_DIR)

####### Ode Support
ODE_LIB_DIR = ../StdLib
ODE_LIB_OBJ = $(ODE_LIB_DIR)/OdeLibrary.o
CRTFAST     = -Wl,--no-as-needed,$(ODE_LIB_DIR)/crtfastmath.o

# Project Root
PROJ_ROOT_DIR   = ../../../root
PROJ_LIB_DIR    = $(PROJ_ROOT_DIR)/lib
PROJ_INC_DIR    = $(PROJ_ROOT_DIR)/include

# CVODE Libs
CVODE_LIBS      = -lsundials_cvode -lsundials_nvecserial -lm

all: CvodeSim.bc OdeSolver.exe OdeModelDump.exe

#CvodeSim.exe: CvodeSim.c OdeModel.o OdeModel.h
#	$(CC) $(CFLAGS) -o $@ $(INC_DIRS) CvodeSim.c OdeModel.o $(ODE_LIB_OBJ) $(LIB_DIRS) $(CVODE_LIBS) $(CRTFAST)

CvodeSim.bc: CvodeSim.c OdeModel.h
	$(CC) $(CFLAGS) -o $@ $(INC_DIRS) CvodeSim.c OdeModel.o $(ODE_LIB_OBJ) $(LIB_DIRS) $(CVODE_LIBS) $(CRTFAST)

OdeSolver.exe: OdeSolver.c OdeModel.o OdeModel.h
	$(CC) $(CFLAGS) -o $@ $(INC_DIRS) OdeSolver.c OdeModel.o $(ODE_LIB_OBJ) $(LIB_DIRS) -lm $(CRTFAST)

OdeModelDump.exe: OdeModelDump.c OdeModel.o OdeModel.h
	$(CC) $(CFLAGS) -o $@ $(INC_DIRS) OdeModelDump.c OdeModel.o $(ODE_LIB_OBJ) $(LIB_DIRS) -lm $(CRTFAST)

clean:
	rm -f CvodeSim.exe OdeSolver.exe OdeModelDump.exe

.PHONY: clean

