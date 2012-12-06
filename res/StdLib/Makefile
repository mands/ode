####### Compiler, tools and options
CC            = clang
CFLAGS        = -std=c11 -O3 -pedantic -Weverything -ffast-math -integrated-as

all: OdeLibrary.bc OdeLibrary.o AOTStub.bc

OdeLibrary.bc: OdeLibrary.c OdeLibrary.h WELL512a.c WELL512a.h
	$(CC) $(CFLAGS) -emit-llvm -c -o OdeLibrary.bc OdeLibrary.c
	$(CC) $(CFLAGS) -emit-llvm -c -o WELL512a.bc WELL512a.c
	llvm-dis $@
	# link into bc archive
	llvm-link -o OdeLibraryFinal.bc OdeLibrary.bc WELL512a.bc
	llvm-dis OdeLibraryFinal.bc

# generate an optimised version of the odelibrary
#OdeLibraryOpt.bc: OdeLibraryOpt.ll
#	llvm-as -o $@ $<

OdeLibrary.o: OdeLibrary.c OdeLibrary.h WELL512a.c WELL512a.h
	$(CC) $(CFLAGS) -c OdeLibrary.c
	$(CC) $(CFLAGS) -c WELL512a.c
	ar rcs libOde.a OdeLibrary.o WELL512a.o

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


