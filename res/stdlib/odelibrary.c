// Run-time library for the ODE Compiler
// Written in C99 rathn than LLVM for quick development
// Mainly utility functions for outputting balues to screen and files
#include "odelibrary.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
// #define M_PI 3.14159265358979323846264338327

// Add library support for...
// multiple file opens within single ode file - needs compiler support
// 

// sets up the global enviornment for all simluations
void init(void) {
  puts("Initialising the Ode Solver environment");
  // any other global initialisation
}

void shutdown(void) {
  puts("Shutting down the Ode Solver environment");
}

// data for individual simluations
// holds the block of data to output
static FILE* out_file;

void start_sim(const char* filename, const uint64_t n_args) {
  printf("Starting simluation with output to %s\n", filename);
  //char* filename = "outfile.bin";
  out_file = fopen(filename, "wb");

  // setup file header/first_run init here
  // write the num of cols as the header of the output file
  fwrite(&n_args, sizeof(uint64_t), 1, out_file);
}

void end_sim(void) {
  fclose(out_file);
  puts("Finished simulation");
}

void write_dbls(const double *dbls, const uint64_t n_args) {
  // we use fwrite rather than write syscall as want buffering as the data byte-count is small
  fwrite(dbls, sizeof(double), n_args, out_file);  
}

