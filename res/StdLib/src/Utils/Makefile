ROOT_DIR=../..
include $(ROOT_DIR)/Makefile.inc

all: RandTest.exe ProjectTest.exe OdeModelDump.exe

# unused - aot stub
AOTStub.bc: AOTStub.ll
	llvm-as -o $@ $<

RandTest.exe: RandTest.c
	$(CC) $(CFLAGS) -o $@ $(INC_DIRS) $< $(LIB_DIRS) $(ODE_LIB) $(LDLIBS)

ProjectTest.exe: ProjectTest.c
	$(CC) $(CFLAGS) -o $@ $(INC_DIRS) $< $(LIB_DIRS) $(ODE_LIB) $(LDLIBS)

OdeModelDump.exe: OdeModelDump.c OdeModel.o
	$(CC) $(CFLAGS) -o $@ $(INC_DIRS) $< OdeModel.o $(LIB_DIRS) $(ODE_LIB) $(LDLIBS)

install: RandTest.exe ProjectTest.exe OdeModelDump.exe
	cp RandTest.exe $(ODE_BIN_DIR)
	cp ProjectTest.exe $(ODE_BIN_DIR)
	cp OdeModelDump.exe $(ODE_BIN_DIR)

clean:
	rm -f *.o *~ *.bin *.exe

