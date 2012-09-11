# Simple makefile for benchmarks
ROOT_DIR = ..
include $(ROOT_DIR)/Makefile.inc

all: accum

accum: accum.c
	$(CC) $(CFLAGS) -I$(ODE_INC_DIR) -o $@ $< -L$(ODE_LIB_DIR) $(LIBS)

clean:
	rm -f accum

.PHONY: clean


