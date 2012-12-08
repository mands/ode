###### Compiler, tools and options
CC            = clang
CFLAGS        = -std=c11 -O3 -pedantic -Weverything -ffast-math -integrated-as
CPP           = clang++
CPPFLAGS      = -std=c++11 -O3 -pedantic -ffast-math -Weverything -integrated-as
GCC           = gcc
GCCFLAGS      = -std=c11 -pedantic -Wall -Wextra -ffast-math

# Ode StdLib/FFI Support
ODE_LIB_DIR   = $(ROOT_DIR)/../../../StdLib
ODE_FFI_DIR   = $(ROOT_DIR)/../../../FFI
ODE_LIB_OBJ   = $(ODE_LIB_DIR)/OdeLibrary.o
ODE_LIB       = -lOde
CRTFAST       = -Wl,--no-as-needed,$(ODE_LIB_DIR)/crtfastmath.o

# Project Root
PROJ_ROOT_DIR   = $(ROOT_DIR)/../../../../../root
PROJ_INC_DIR    = $(PROJ_ROOT_DIR)/include
PROJ_LIB_DIR    = $(PROJ_ROOT_DIR)/lib

# CVODE Libs
CVODE_LIBS      = -lsundials_cvode -lsundials_nvecserial

# Library Paths
LDLIBS        = -lm
INC_DIRS      = -I$(ODE_FFI_DIR) -I$(ODE_LIB_DIR) -I$(PROJ_INC_DIR)
LIB_DIRS      = -L$(ODE_LIB_DIR) -L$(PROJ_LIB_DIR)

