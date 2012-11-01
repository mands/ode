# Simple makefile for benchmarks

####### Compiler, tools and options
CC          = clang
CFLAGS      = -std=c99 -pedantic -Weverything -Werror -integrated-as -fno-math-errno -ffast-math
LIBS        = -lm
LD          = ld.gold
ODE_LIB_DIR = ../../../stdlib
ODE_INC_DIR = ../../../stdlib
GCC         = gcc
GCCFLAGS    = -std=c99 -pedantic -Wall -fno-math-errno -ffast-math

all: sim exe

sim: model.ode.src.ll model.clang.src.ll
	@echo "** Building Sim **"
	# Ode Sim
	opt -o model.ode.opt.bc -std-compile-opts model.ode.src.ll
	llvm-link -o sim.ode.bc model.ode.opt.bc $(ODE_LIB_DIR)/odelibrary.bc
	opt -S -o sim.ode.lto.ll -std-link-opts -std-compile-opts sim.ode.bc
	# Clang Sim
	opt -o model.clang.opt.bc -std-compile-opts model.clang.src.ll
	llvm-link -o sim.clang.bc model.clang.opt.bc $(ODE_LIB_DIR)/odelibrary.bc
	opt -S -o sim.clang.lto.ll -std-link-opts -std-compile-opts sim.clang.bc

exe: sim.ode.src.ll sim.clang.src.ll
	@echo "** Building Exe **"
	$(CC) $(CFLAGS) -O3 -fno-math-errno -ffast-math -o sim.ode.exe sim.ode.src.ll $(LIBS)
	$(CC) $(CFLAGS) -O3 -fno-math-errno -ffast-math -o sim.clang.exe sim.clang.src.ll $(LIBS)

bench: exe
	dumbbench -v -v ./sim.clang.exe >> bench.out
	dumbbench -v -v ./sim.ode.exe >> bench.out

clean:
	rm -f *.exe *.bc
	rm -f *.bin
	rm -f callgrind.* perf.data llvmprof.out bench.out
	rm -f *~

.PHONY: clean

