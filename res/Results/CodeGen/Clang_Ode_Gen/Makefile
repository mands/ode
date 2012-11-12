# Simple makefile for benchmarks
ROOT_DIR = ..
include $(ROOT_DIR)/Makefile.inc

all: SimClangGen.exe SimClang.exe

SimClangGen.exe: HodHuxGenModel.c HodHuxGenSolver.c
	@echo "** Building Clang-Generic **"
	# compile each separately to object files
	$(CC) $(CFLAGS) -O3 -c -I$(ODE_INC_DIR) HodHuxGenModel.c
	$(CC) $(CFLAGS) -O3 -c -I$(ODE_INC_DIR) HodHuxGenSolver.c
	# now link
	$(CC) $(CFLAGS) -O3 -o $@ -I$(ODE_INC_DIR) HodHuxGenModel.o HodHuxGenSolver.o $(ODE_LIB_OBJ) $(LIBS)

SimClang.exe: HodHux.c
	@echo "** Building Clang Normal **"
	$(CC) $(CFLAGS) -O3 -o $@ -I$(ODE_INC_DIR) $< $(ODE_LIB_OBJ) $(LIBS)

bench: all
	dumbbench -vv ./SimClangGen.exe > bench.out
	dumbbench -vv ./SimClang.exe    >> bench.out

clean:
	rm -f *.exe *.bc *.o
	rm -f *.bin
	rm -f callgrind.* perf.data llvmprof.out bench.out
	rm -f *~

.PHONY: clean

