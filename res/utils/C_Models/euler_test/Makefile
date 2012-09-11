# Simple makefile for benchmarks
# Euler Test Compilation
ROOT_DIR = ..
include $(ROOT_DIR)/Makefile.inc

all: euler_test

euler_test: euler_test
	$(CC) $(CFLAGS) -I$(ODE_INC_DIR) -o $@ $< -L$(ODE_LIB_DIR) $(LIBS)

clean:
	rm -f euler_test
	rm -f *.bin

.PHONY: clean

