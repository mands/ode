# Simple makefile for benchmarks

####### Compiler, tools and options
CC          = clang
CFLAGS      = -std=c11 -pedantic -Weverything -Werror -integrated-as -fno-math-errno -ffast-math
LIBS        = -lm
LD          = ld.gold
ODE_LIB_DIR = ../../../stdlib
ODE_INC_DIR = ../../../stdlib
GCC         = gcc
GCCFLAGS    = -std=c11 -pedantic -Wall -fno-math-errno -ffast-math

all: hodgkin_gen

hodgkin_gen: hodgkin_gen_model.c hodgkin_gen_solver.c
	@echo "** Building Hodkgin Generic **"
	# compile each separately to object files
	$(CC) $(CFLAGS) -I$(ODE_INC_DIR) -O3 -c hodgkin_gen_model.c
	$(CC) $(CFLAGS) -I$(ODE_INC_DIR) -O3 -c hodgkin_gen_solver.c
	# now link
	$(CC) $(CFLAGS) -I$(ODE_INC_DIR) -O3 -o sim.exe hodgkin_gen_model.o hodgkin_gen_solver.o $(ODE_LIB_DIR)/odelibrary.o $(LIBS)

bench: hodgkin_gen
	dumbbench -v -v ./sim.exe >> bench.out

clean:
	rm -f *.exe *.bc
	rm -f *.bin
	rm -f callgrind.* perf.data llvmprof.out bench.out
	rm -f *~

.PHONY: clean

