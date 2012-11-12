# Simple makefile for benchmarks
ROOT_DIR = ..
include $(ROOT_DIR)/Makefile.inc

all: SimICC.exe SimICC1.exe SimOde.exe SimOde1.exe SimOde2.exe SimOde3.exe

# uses ICC precise math
SimICC.exe: HodHux.c
	# ** Building ICC **
	$(ICC) $(ICCFLAGS) -fp-model precise -O3 -o $@ -I$(ODE_INC_DIR) $< $(ODE_LIB_OBJ) $(LIBS)

# uses ICC fast math
SimICC1.exe: HodHux.c
	# ** Building ICC **
	$(ICC) $(ICCFLAGS) -fp-model double -fp-model fast=2 -no-prec-div -no-prec-sqrt -O3 -o $@ -I$(ODE_INC_DIR) $< $(ODE_LIB_OBJ) $(LIBS)

# Ode using GCC libm
SimOde.exe: HodHuxOde.ll
	# ** Building Ode **
	$(CC) $(CFLAGS) -O3 -o $@ $< $(ODE_LIB_OBJ) $(LIBS) $(CRTFAST)

# Ode using Intel libimf
SimOde1.exe: HodHuxOde1.ll
	# ** Building Ode **
	$(CC) $(CFLAGS) -O3 -o $@ $< $(ODE_LIB_OBJ) -L/opt/intel/lib/intel64 -limf $(CRTFAST)

# Ode using GCC libm precise
SimOde2.exe: HodHuxOde1.ll
	# ** Building Ode **
	$(CC) -integrated-as -O3 -o $@ $< $(ODE_LIB_OBJ) $(LIBS)

# Ode using Intel libimf precise
SimOde3.exe: HodHuxOde1.ll
	# ** Building Ode **
	$(CC) -integrated-as -O3 -o $@ $< $(ODE_LIB_OBJ) -L/opt/intel/lib/intel64 -limf

bench:
	dumbbench -vv ./SimICC.exe  > bench.out
	dumbbench -vv ./SimICC1.exe >> bench.out
	dumbbench -vv ./SimOde.exe  >> bench.out
	dumbbench -vv ./SimOde1.exe >> bench.out
	dumbbench -vv ./SimOde2.exe >> bench.out
	dumbbench -vv ./SimOde3.exe >> bench.out

clean:
	rm -f *.exe *.bc *.o
	rm -f *.bin
	rm -f callgrind.* perf.data llvmprof.out bench.out
	rm -f *~

.PHONY: clean

