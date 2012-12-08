# Simple makefile for benchmarks
ROOT_DIR = ..
include $(ROOT_DIR)/Makefile.inc

all: mahajan.exe mahajan_ode.exe
# original PG model - converted to C, unused variables removed, using OdeLib
# -D__extern_always_inline=inline
mahajan.exe: mahajan.c
	$(CC) $(CFLAGS) -o $@ $(INC_DIRS) $< $(LIB_DIRS) $(CVODE_LIBS) $(ODE_LIB) $(LDLIBS) $(CRTFAST)

# modified version of PG model, with COR generated code replaced with Ode ObjectFile output, using OdeLib
mahajan_ode.exe: mahajan_ode.c OdeModel.o
	$(CC) $(CFLAGS) -o $@ $(INC_DIRS) $< OdeModel.o $(LIB_DIRS) $(CVODE_LIBS) $(ODE_LIB) $(LDLIBS) $(CRTFAST)

bench: mahajan.exe mahajan_ode.exe
	/usr/bin/time -v ./mahajan.exe
	/usr/bin/time -v ./mahajan_ode.exe

#mahajan_opt.exe: mahajan_opt.c
#	$(CC) $(CFLAGS) -o $@ $(INC_DIRS) $< $(LIB_DIRS) $(CVODE_LIBS)

clean:
	rm *.o *~ *.exe *.txt *.bin

