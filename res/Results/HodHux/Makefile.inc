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

####### OdeLib Support
ODE_INC_DIR = $(ROOT_DIR)/../../StdLib/include
ODE_LIB_DIR = $(ROOT_DIR)/../../StdLib/lib
ODE_LIB = -lOde

