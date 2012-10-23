# Simple makefile for ode runtime library
#ROOT_DIR = ..
#include $(ROOT_DIR)/Makefile.inc

####### Compiler, tools and options
CC            = clang
CFLAGS        = -std=c99 -pedantic -Weverything -integrated-as -fno-math-errno -ffast-math 
LD            = ld.gold
#ODE_LIB_DIR = $(ROOT_DIR)/stdlib
#ODE_INC_DIR = $(ROOT_DIR)/stdlib


all: odelibrary.bc odelibrary.c.ll

odelibrary.c.ll: odelibrary.c odelibrary.h
	$(CC) $(CFLAGS) -S -O3 -emit-llvm -c -o $@ $<

odelibrary.bc: odelibrary.opt.ll
	llvm-as -o $@ $<

vecmath: VecMathCog.py
	cog.py -r -c -e vecmath_libm.ll
	cog.py -r -c -e vecmath_amd.ll
	cog.py -r -c -e vecmath_intel.ll
	llvm-as vecmath_libm.ll
	llvm-as vecmath_amd.ll
	llvm-as vecmath_intel.ll

clean:
	rm -f *.bc *.ll *~

.PHONY: clean

