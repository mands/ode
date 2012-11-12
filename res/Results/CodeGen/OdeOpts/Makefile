# Simple makefile for benchmarks
ROOT_DIR = ..
include $(ROOT_DIR)/Makefile.inc

all: SimGCC.exe SimOde.exe

SimGCC.exe: HodHux.c
	# ** Building GCC **
	$(GCC) $(GCCFLAGS) -O3 -o $@ -I$(ODE_INC_DIR) $< $(ODE_LIB_OBJ) $(LIBS)

SimOde.exe: HodHuxOde.ll
	# ** Building Ode **
	$(CC) $(CFLAGS) -O3 -o $@ $< $(ODE_LIB_OBJ) $(LIBS) $(CRTFAST)

bench:
	dumbbench -vv ./SimGCC.exe > bench.out
	dumbbench -vv ./SimOde.exe >> bench.out

clean:
	rm -f *.exe *.bc *.o
	rm -f *.bin
	rm -f callgrind.* perf.data llvmprof.out bench.out
	rm -f *~

.PHONY: clean

