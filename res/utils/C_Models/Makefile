# Simple makefile for benchmarks
# benchmarks subdirectories
DIRS = euler_test hodgkin

all:
	-for d in $(DIRS); do (cd $$d; $(MAKE)); done

clean:
	-for d in $(DIRS); do (cd $$d; $(MAKE) clean); done

.PHONY: clean

