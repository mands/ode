# Simple makefile for Ode Tools
CC            = clang
CFLAGS        = -std=c11 -O3 -pedantic -Weverything -integrated-as -fno-math-errno -ffast-math

all: FileReader

FileReader.exe: FileReader.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f *.exe *.o

#install:
#	cp $(TARGET) $(ODE_BIN_DIR)

.PHONY: clean

