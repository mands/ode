# Simple makefile for ode_file_reader
CFLAGS = -Wall -Wextra -pedantic -std=gnu99 -O3 

TARGET = LLVMTest

all: $(TARGET)

$(TARGET): LLVMTest.hs
	ghc --make $<

clean:
	rm -f *.o *.hi
	rm -f $(TARGET)

.PHONY: clean

