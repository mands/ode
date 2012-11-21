# Simple makefile for benchmarks
ROOT_DIR = ..
include $(ROOT_DIR)/Makefile.inc

all: BuildSims

BuildSims:
	# ** Building Simulations **
	./SimScript.sh

bench:
	dumbbench -vv ./Sim_HodHux_Euler.exe > bench.out
	dumbbench -vv ./Sim_HodHux_RK4.exe >> bench.out


clean:
	rm -f *.bc *.o
	rm -f *.bin
	rm -f callgrind.* perf.data llvmprof.out bench.out
	rm -f *~

.PHONY: clean

