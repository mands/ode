####### Compiler, tools and options
CC            = clang
CFLAGS        = -std=c99 -O3 -pedantic -Wall -Wextra -W
INCPATH       = -I/usr/local/include -Iinclude -I.
LIBS          = -lode -lm
LD            = ld.gold
ODE_LIB_DIR = $(ROOT_DIR)/stdlib
ODE_INC_DIR = $(ROOT_DIR)/stdlib

