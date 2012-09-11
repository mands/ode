# Simple makefile for ode runtime library
ROOT_DIR = ..
include $(ROOT_DIR)/Makefile.inc

all: libode.a libode.so.1.0.1

libode.a: odelibrary.c odelibrary.h WELL512a.c WELL512a.h
	$(CC) $(CFLAGS) -o odelibrary.o -c odelibrary.c
	$(CC) $(CFLAGS) -o WELL512a.o -c WELL512a.c
	ar rcs $@ odelibrary.o WELL512a.o

libode.so.1.0.1: odelibrary.c odelibrary.h WELL512a.c WELL512a.h
	$(CC) $(CFLAGS) -fPIC -c -o odelibrary.o odelibrary.c
	$(CC) $(CFLAGS) -fPIC -c -o WELL512a.o WELL512a.c
	$(CC) $(CFLAGS) -shared -Wl,-soname,libode.so.1 -o $@ odelibrary.o WELL512a.o

clean:
	rm -f *.o
	rm -f libode.a libode.so.*

#install:
#	cp libode.a $(ODE_LIB_DIR)
#	cp libode.so.1.0.1 $(ODE_LIB_DIR)
#	cp odelibrary.h $(ODE_INC_DIR)

.PHONY: clean

