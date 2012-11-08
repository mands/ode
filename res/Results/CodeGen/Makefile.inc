###### Compiler, tools and options
CC            = clang
CFLAGS        = -std=c11 -pedantic -Weverything -ffast-math -integrated-as
LIBS          = -lm
LD            = ld.gold
GCC           = gcc
GCCFLAGS      = -std=c11 -pedantic -Wall -Wextra -ffast-math

####### Ode Support
ODE_LIB_DIR = $(ROOT_DIR)/../../stdlib
ODE_INC_DIR = $(ROOT_DIR)/../../stdlib

