# Simple makefile for benchmarks
ROOT_DIR = ..
include $(ROOT_DIR)/Makefile.inc

all: sim.clang_gen.exe

sim.clang_gen.exe: hodgkin_gen_model.c hodgkin_gen_solver.c
	@echo "** Building Clang-Generic **"
	# compile each separately to object files
	$(CC) $(CFLAGS) -I$(ODE_INC_DIR) -O3 -c hodgkin_gen_model.c
	$(CC) $(CFLAGS) -I$(ODE_INC_DIR) -O3 -c hodgkin_gen_solver.c
	# now link
	$(CC) $(CFLAGS) -I$(ODE_INC_DIR) -O3 -o sim.exe hodgkin_gen_model.o hodgkin_gen_solver.o $(ODE_LIB_DIR)/odelibrary.o $(LIBS)

bench: hodgkin_gen
	dumbbench -v -v ./sim.exe >> bench.out

clean:
	rm -f *.exe *.bc
	rm -f *.bin
	rm -f callgrind.* perf.data llvmprof.out bench.out
	rm -f *~

.PHONY: clean

