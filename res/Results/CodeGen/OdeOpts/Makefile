# Simple makefile for benchmarks
ROOT_DIR = ..
include $(ROOT_DIR)/Makefile.inc

all: BuildSims

SimGCC.exe: HodHux.c
	# ** Building GCC **
	$(GCC) $(GCCFLAGS) -O3 -o $@ -I$(ODE_INC_DIR) $< $(ODE_LIB_OBJ) $(LIBS)

SimOde.exe: HodHuxOde.ll
	# ** Building Ode **
	$(CC) $(CFLAGS) -O3 -o $@ $< $(ODE_LIB_OBJ) $(LIBS) $(CRTFAST)

BuildSims:
	# ** Building Simulations **
	./SimScript.sh

# Test errs in pow expansion
# 1 - x^4 opt expanded
ManualSims: Sim_PowerExpa1.ll
	$(CC) $(CFLAGS) -O3 -o Sim_PowerExpa1.exe Sim_PowerExpa1.ll $(ODE_LIB_OBJ) $(LIBS) $(CRTFAST)

bench:
	dumbbench -vv ./Sim_Unopt.exe > bench.out
	dumbbench -vv ./Sim_OptStrict.exe >> bench.out
	dumbbench -vv ./Sim_OptStrictLTO.exe >> bench.out
	dumbbench -vv ./Sim_FastMath.exe >> bench.out
	dumbbench -vv ./Sim_ShortCircuit.exe >> bench.out
	dumbbench -vv ./Sim_PowerExpan.exe >> bench.out
	dumbbench -vv ./Sim_PowerExpa1.exe >> bench.out


clean:
	rm -f *.exe *.bc *.o
	rm -f *.bin
	rm -f callgrind.* perf.data llvmprof.out bench.out
	rm -f *~

.PHONY: clean

