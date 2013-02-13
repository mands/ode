# Simple makefile for benchmarks
ROOT_DIR = .
include $(ROOT_DIR)/Makefile.inc

all: HodHuxC.exe

HodHuxC.exe: HodHuxC.c
	# ** Building Clang Opt **
	$(CC) $(CFLAGS) -O3 -o $@ -I$(ODE_INC_DIR) $< -L$(ODE_LIB_DIR) $(ODE_LIB) $(LIBS)

clean:
	rm -f *.exe *.bc
	rm -f *.bin
	rm -f callgrind.* perf.data llvmprof.out bench.out
	rm -f *~

.PHONY: clean

