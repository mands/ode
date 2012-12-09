ROOT_DIR=../..
include $(ROOT_DIR)/Makefile.inc

all: VecMath

VecMath: VecMath_GNU.bc VecMath_AMD.bc VecMath_Intel.bc

VecMath_GNU.bc: VecMathCog.py VecMath_GNU.ll
	cog.py -r -c -e VecMath_GNU.ll
	llvm-as VecMath_GNU.ll

VecMath_AMD.bc: VecMathCog.py VecMath_AMD.ll
	cog.py -r -c -e VecMath_AMD.ll
	llvm-as VecMath_AMD.ll

VecMath_Intel.bc: VecMathCog.py VecMath_Intel.ll
	cog.py -r -c -e VecMath_Intel.ll
	llvm-as VecMath_Intel.ll

install: VecMath
	cp VecMath_GNU.bc $(ODE_LIB_DIR)
	cp VecMath_AMD.bc $(ODE_LIB_DIR)
	cp VecMath_Intel.bc $(ODE_LIB_DIR)

clean:
	rm -f *.o *~ *.bc

.PHONY: clean install VecMath

