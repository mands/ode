# Simple makefile for Ode Tools
CC            = clang
CFLAGS        = -std=c11 -O3 -pedantic -Weverything -ffast-math -integrated-as
LDLIBS        = -lm
INC_DIRS      = -I$(ODE_LIB_DIR) -I$(PROJ_INC_DIR)
LIB_DIRS      = -L$(PROJ_LIB_DIR) -L$(ODE_LIB_DIR)

####### Ode Support
ODE_LIB_DIR = ../StdLib
ODE_LIB     = -lOde
CRTFAST     = -Wl,--no-as-needed,$(ODE_LIB_DIR)/crtfastmath.o

# Project Root
PROJ_ROOT_DIR   = ../../../root
PROJ_LIB_DIR    = $(PROJ_ROOT_DIR)/lib
PROJ_INC_DIR    = $(PROJ_ROOT_DIR)/include

# CVODE Libs
CVODE_LIBS      = -lsundials_cvode -lsundials_nvecserial -lm

all: CvodeSim.bc OdeSolver.exe OdeModelDump.exe

# dummy front-end from Cvode solver
CvodeSim.exe: CvodeSimMain.c CvodeSim.c OdeModel.o OdeModel.h
	$(CC) $(CFLAGS) -o $@ $(INC_DIRS) CvodeSimMain.c CvodeSim.c OdeModel.o $(LIB_DIRS) $(CVODE_LIBS) $(ODE_LIB) $(CRTFAST)

CvodeSim.bc: CvodeSim.c OdeModel.h
	$(CC) $(CFLAGS) -emit-llvm -c -o $@ $(INC_DIRS) CvodeSim.c
	llvm-dis $@

OdeSolver.exe: OdeSolver.c OdeModel.o OdeModel.h
	$(CC) $(CFLAGS) -o $@ $(INC_DIRS) OdeSolver.c OdeModel.o $(LIB_DIRS) -lm $(ODE_LIB) $(CRTFAST)

OdeModelDump.exe: OdeModelDump.c OdeModel.o OdeModel.h
	$(CC) $(CFLAGS) -o $@ $(INC_DIRS) OdeModelDump.c OdeModel.o $(LIB_DIRS) -lm $(ODE_LIB) $(CRTFAST)

clean:
	rm -f CvodeSim.exe OdeSolver.exe OdeModelDump.exe

.PHONY: clean

