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

void first_run(const uint32_t n_args);

// sets up the global enviornment for all simluations
void init(void) {
  printf("Initialising the ODE environment\n");
  // any other global initialisation
}

void shutdown(void) {
  printf("Shutting down the ODE environment\n");

}

// data for individual simluations
// holds the block of data to output
bool is_first_run = true;
static FILE* out_file;

void start_sim(const char* filename) {
  printf("Starting simluation with output to %s\n", filename);
  //char* filename = "outfile.bin";
  out_file = fopen(filename, "wb");
  // should do first_run init here
}

void end_sim(void) {
  fclose(out_file);
  is_first_run = true;
  printf("Finished simulation\n");
}

void first_run(const uint32_t n_args) {
  is_first_run = false;
  printf("First run called...\n");

  // write the num of cols as the header of the output file
  fwrite(&n_args, sizeof(uint32_t), 1, out_file);
}

void write_dbls(const uint32_t n_args, const double *dbls) {
  if (is_first_run) first_run(n_args);
  // we use fwrite rather than write syscall as want buffering as the data byte-count is small
  fwrite(dbls, sizeof(double), n_args, out_file);  
}

