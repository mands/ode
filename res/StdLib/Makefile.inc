####### Compiler, tools and options
CC            = clang
CFLAGS        = -std=c11 -O3 -pedantic -Weverything -ffast-math -integrated-as

####### Ode Support
ODE_LIB_DIR = $(ROOT_DIR)/lib
ODE_INC_DIR = $(ROOT_DIR)/include
ODE_BIN_DIR = $(ROOT_DIR)/bin
ODE_LIB     = -lOde

# Project Root
PROJ_ROOT_DIR   = $(ROOT_DIR)/../../../root
PROJ_LIB_DIR    = $(PROJ_ROOT_DIR)/lib
PROJ_INC_DIR    = $(PROJ_ROOT_DIR)/include

# CVODE Libs
CVODE_LIBS      = -lsundials_cvode -lsundials_nvecserial

# Library Paths
LDLIBS        = -lm # $(CRTFAST)
INC_DIRS      = -I$(ODE_INC_DIR) -I$(PROJ_INC_DIR)
LIB_DIRS      = -L$(ODE_LIB_DIR) -L$(PROJ_LIB_DIR)

