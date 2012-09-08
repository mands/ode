# Simple makefile for ode_file_reader
#ROOT_DIR = ../..
#include $(ROOT_DIR)/Makefile.inc
CC            = clang
CFLAGS        = -std=c99 -O3 -pedantic -Wall -Wextra

TARGET = FileReader

all: $(TARGET)

$(TARGET): FileReader.c

clean:
	rm -f $(TARGET) FileReader.o

install:
	cp $(TARGET) $(ODE_BIN_DIR)

.PHONY: clean

