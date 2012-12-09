ROOT_DIR=../..
include $(ROOT_DIR)/Makefile.inc

all: libOde.bc libOde.a

# build bitcode version of library
libOde.bc: OdeStdLib.c OdeStdLib.h WELL512a.c WELL512a.h
	$(CC) $(CFLAGS) -emit-llvm -c -o OdeStdLib.bc OdeStdLib.c
	$(CC) $(CFLAGS) -emit-llvm -c -o WELL512a.bc WELL512a.c
	# link into bc archive
	llvm-link -o $@ OdeStdLib.bc WELL512a.bc
	llvm-dis $@

# build a C-version of the stdlib
libOde.a: OdeStdLib.c OdeStdLib.h WELL512a.c WELL512a.h
	$(CC) $(CFLAGS) -c OdeStdLib.c
	$(CC) $(CFLAGS) -c WELL512a.c
	ar rcs $@ OdeStdLib.o WELL512a.o

# unused - generate an optimised version of the odelibrary
#OdeLibraryOpt.bc: OdeLibraryOpt.ll
#	llvm-as -o $@ $<

install: libOde.bc libOde.a OdeStdLib.h
	cp libOde.bc $(ODE_LIB_DIR)
	cp libOde.a $(ODE_LIB_DIR)
	cp OdeStdLib.h $(ODE_INC_DIR)

clean:
	rm -f *.o *~ *.bin libOde.a libOde.bc libOde.ll

