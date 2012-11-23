###### Compiler, tools and options
CC            = clang
CFLAGS        = -std=c11 -pedantic -Weverything -ffast-math -integrated-as
LIBS          = -lm
LD            = ld.gold
GCC           = gcc
GCCFLAGS      = -std=c11 -pedantic -Wall -Wextra -ffast-math
ICC           = icc
ICCFLAGS      = -std=c99 -pedantic -Wall -Wextra
MINCFLAGS     = -std=c99 -pedantic -Wall -Wextra
LLCFLAGS      = -integrated-as

####### Ode Support
ODE_LIB_DIR = $(ROOT_DIR)/../../StdLib
ODE_LIB_OBJ = $(ODE_LIB_DIR)/OdeLibrary.o
ODE_INC_DIR = $(ROOT_DIR)/../../StdLib
CRTFAST     = -Wl,--no-as-needed,$(ODE_LIB_DIR)/crtfastmath.o

