# Simple makefile for ode runtime library
#ROOT_DIR = ..
#include $(ROOT_DIR)/Makefile.inc

####### Compiler, tools and options
CC            = clang
CFLAGS        = -std=c99 -pedantic -Weverything -integrated-as -fno-math-errno -ffast-math 
LD            = ld.gold
#ODE_LIB_DIR = $(ROOT_DIR)/stdlib
#ODE_INC_DIR = $(ROOT_DIR)/stdlib


all: odelibrary.bc

odelibrary.bc: odelibrary.c odelibrary.h
	$(CC) $(CFLAGS) -O0 -emit-llvm -c -o $@ $<
	llvm-dis $@

clean:
	rm -f *.bc *.ll *~

.PHONY: clean

