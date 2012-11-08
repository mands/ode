# Simple makefile for benchmarks
ROOT_DIR = .
include $(ROOT_DIR)/Makefile.inc

all: sim.gcc.exe

sim.gcc.exe: HodHux.c
	@echo "** Building GCC **"
	$(GCC) $(GCCFLAGS) -O3 -o $@ -I$(ODE_INC_DIR) $< $(ODE_LIB_DIR)/odelibrary.o -lm

bench: all
	dumbbench -vv ./sim.gcc.exe >> bench.out

clean:
	rm -f *.exe *.bc
	rm -f *.bin
	rm -f callgrind.* perf.data llvmprof.out bench.out
	rm -f *~

.PHONY: clean

