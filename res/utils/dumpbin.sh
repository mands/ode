#!/bin/sh

# Dumps the first 10 values from a bin file
hexdump -v -e '1/4 "%d\n" 10/8 "%g\n" "\n"' $1 
