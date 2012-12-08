# Simple makefile for benchmarks
ROOT_DIR = ..
include $(ROOT_DIR)/Makefile.inc

all: mahajan.exe
# original PG model - converted to C, unused variables removed, using OdeLib
# -D__extern_always_inline=inline
mahajan.exe: mahajan.c
	$(CC) $(CFLAGS) -o $@ $(INC_DIRS) $< $(LIB_DIRS) $(CVODE_LIBS) $(ODE_LIB) $(LDLIBS) $(CRTFAST)

bench: mahajan.exe Sim.exe
	/usr/bin/time -v ./mahajan.exe
	/usr/bin/time -v ./Sim.exe

#mahajan_opt.exe: mahajan_opt.c
#	$(CC) $(CFLAGS) -o $@ $(INC_DIRS) $< $(LIB_DIRS) $(CVODE_LIBS)

clean:
	rm *.o *~ *.exe *.txt *.bin

