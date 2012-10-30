# Simple makefile for benchmarks

####### Compiler, tools and options
CC          = clang
CFLAGS      = -std=c99 -pedantic -Weverything -integrated-as -fno-math-errno -ffast-math
LIBS        = -lm
LD          = ld.gold
ODE_LIB_DIR = ../../stdlib
ODE_INC_DIR = ../../stdlib
GCC         = gcc
GCCFLAGS    = -std=c99 -pedantic -Wall -fno-math-errno -ffast-math

all: sim.o0.exe sim.opt.exe sim.lto.exe sim.gcc.exe

# unoptimised
sim.o0.exe: hodgkin.c
	$(CC) $(CFLAGS) -I$(ODE_INC_DIR) -O0 -emit-llvm -c -o model.o0.bc $<
	llvm-dis -o model.o0.ll model.o0.bc
	llvm-link -o sim.o0.bc model.o0.bc $(ODE_LIB_DIR)/odelibrary.bc 
	llvm-dis -o sim.o0.ll sim.o0.bc
	$(CC) $(CFLAGS) -O0 -o $@ sim.o0.bc $(LIBS)

# 03 opts
sim.opt.exe: sim.o0.exe
	opt -o sim.opt.bc -std-compile-opts sim.o0.bc
	llvm-dis -o sim.opt.ll sim.opt.bc
	$(CC) $(CFLAGS) -O3 -fno-math-errno -ffast-math -o $@ sim.opt.bc $(LIBS)

# 04 opts - O3 + LTO
sim.lto.exe: sim.opt.exe
	opt -o sim.lto.bc -std-link-opts -std-compile-opts sim.opt.bc
	llvm-dis -o sim.lto.ll sim.lto.bc
	$(CC) $(CFLAGS) -O3 -fno-math-errno -ffast-math -o $@ sim.lto.bc $(LIBS)

sim.gcc.exe: hodgkin.c
	$(GCC) $(GCCFLAGS) -I$(ODE_INC_DIR) -O3 -fno-math-errno -ffast-math -o $@ $< $(ODE_LIB_DIR)/odelibrary.o -lm

bench: sim.o0.exe sim.opt.exe sim.lto.exe
	dumbbench -v -v ./sim.o0.exe >> bench.out
	dumbbench -v -v ./sim.opt.exe >> bench.out
	dumbbench -v -v ./sim.lto.exe >> bench.out
	dumbbench -v -v ./sim.gcc.exe >> bench.out

clean:
	rm -f *.exe *.bc *.ll
	rm -f *.bin
	rm -f callgrind.* perf.data llvmprof.out bench.out
	rm -f *~

.PHONY: clean

