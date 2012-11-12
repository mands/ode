# Simple makefile for benchmarks
ROOT_DIR = ..
include $(ROOT_DIR)/Makefile.inc

all: SimClang.ll SimOde.ll exe
# noalias & nocapturre
# unoptimised
HodHuxClangOrig.ll: HodHux.c
	@echo "** Building LLVM Model **"
	$(CC) $(CFLAGS) -O0 -emit-llvm -S -c -o $@ -I$(ODE_INC_DIR) $<

SimClang.ll: HodHuxClang.ll
	# Clang Sim
	opt -o HodHuxClangOpt.bc -std-compile-opts $<
	llvm-link -o SimClang.bc HodHuxClangOpt.bc $(ODE_LIB_DIR)/OdeLibrary.bc
	opt -S -o SimClangLTO.ll -std-link-opts -std-compile-opts SimClang.bc

SimOde.ll: HodHuxOde.ll
	# Ode Sim
	opt -o HodHuxOdeOpt.bc -std-compile-opts $<
	llvm-link -o SimOde.bc HodHuxOdeOpt.bc $(ODE_LIB_DIR)/OdeLibrary.bc
	opt -S -o SimOdeLTO.ll -std-link-opts -std-compile-opts SimOde.bc

exe: SimClangFinal.ll SimOdeFinal.ll
	@echo "** Building Exe **"
	$(CC) $(CFLAGS) -O3 -o SimClang.exe SimClangFinal.ll $(LIBS)
	$(CC) $(CFLAGS) -O3 -o SimOde.exe SimOdeFinal.ll $(LIBS)

bench: exe
	dumbbench -vv ./SimClang.exe    > bench.out
	dumbbench -vv ./SimOde.exe      >> bench.out

clean:
	rm -f *.exe *.bc
	rm -f *.bin
	rm -f callgrind.* perf.data llvmprof.out bench.out
	rm -f *~

.PHONY: clean

