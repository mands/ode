# Simple makefile for benchmarks
ROOT_DIR=../..
include $(ROOT_DIR)/Makefile.inc

all: SSASolver.exe OdeSolver.exe CvodeSim.bc CvodeSim.exe

SSASolver.exe: SSASolver.c
	$(CC) $(CFLAGS) -o $@ $(INC_DIRS) $< $(LIB_DIRS) $(ODE_LIB) $(LDLIBS) $(CRTFAST)

OdeSolver.exe: OdeSolver.c OdeModel.o
	$(CC) $(CFLAGS) -o $@ $(INC_DIRS) $< OdeModel.o $(LIB_DIRS) $(ODE_LIB) $(LDLIBS) $(CRTFAST)

CvodeSim.bc: CvodeSim.c
	$(CC) $(CFLAGS) -emit-llvm -c -o $@ $(INC_DIRS) $<

CvodeSim.exe: CvodeSim.c CvodeSimMain.c OdeModel.o
	$(CC) $(CFLAGS) -o $@ $(INC_DIRS) CvodeSim.c CvodeSimMain.c OdeModel.o $(LIB_DIRS) $(CVODE_LIBS) $(ODE_LIB) $(LDLIBS) $(CRTFAST)

install: SSASolver.exe OdeSolver.exe CvodeSim.exe CvodeSim.bc
	cp SSASolver.exe $(ODE_BIN_DIR)
	cp OdeSolver.exe $(ODE_BIN_DIR)
	cp CvodeSim.exe $(ODE_BIN_DIR)
	cp CvodeSim.bc $(ODE_LIB_DIR)

clean:
	rm -f *.o *~ *.bin *.exe
