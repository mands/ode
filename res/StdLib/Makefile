####### Compiler, tools and options
CC            = clang
CFLAGS        = -std=c11 -pedantic -Weverything -ffast-math -integrated-as

all: OdeLibrary.bc OdeLibrary.o AOTStub.bc

OdeLibrary.bc: OdeLibrary.c OdeLibrary.h
	$(CC) $(CFLAGS) -emit-llvm -c -o $@ $<
	llvm-dis $@

# generate an optimised version of the odelibrary
OdeLibraryOpt.bc: OdeLibraryOpt.ll
	llvm-as -o $@ $<

OdeLibrary.o: OdeLibrary.c OdeLibrary.h
	$(CC) $(CFLAGS) -O3 -c -o $@ $<

AOTStub.bc: AOTStub.ll
	llvm-as -o $@ $<

VecMath: VecMathCog.py
	cog.py -r -c -e VecMath_GNU.ll
	cog.py -r -c -e VecMath_AMD.ll
	cog.py -r -c -e VecMath_Intel.ll
	llvm-as VecMath_GNU.ll
	llvm-as VecMath_AMD.ll
	llvm-as VecMath_Intel.ll

clean:
	rm -f *.o *~

.PHONY: clean


