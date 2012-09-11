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
#define M_PI 3.14159265358979323846264338327

#include "WELL512a.h"

// Add library support for...
// multiple file opens within single ode file - needs compiler support
// 
double c_test(double arg) {
  printf("Hello world from C!!\n");
  printf("Recieved a %f from ODE!!\n", arg);
  return 0.0;
}

// sets up the global enviornment for all simluations
void init(void) {
  printf("Initialising the ODE environment\n");

  // seed the prng using /dev/urandom
  unsigned int seed[16];
  FILE *file = fopen("/dev/urandom", "rb");
  fread(seed, sizeof(unsigned int), 16, file);
  fclose(file);
  InitWELLRNG512a(seed);

  // any other global initialisation
}

void shutdown(void) {
  printf("Shutting down the ODE environment\n");

}

// gets a (fast) random number between 0 and 1 with uniform distribution
// todo, inline this func call at some point
double uni_rand(void) {
  return WELLRNG512a();
}

// gets a (fast) random number between 0 and 1 with normal distribution
double norm_rand(void) {
	static double y;
	static bool y_exists = false;
	if (y_exists == true) {
		y_exists = false;
		return y;
	} else {
		double ex1 = sqrt(-2*log(uni_rand()));
		double ex2 = 2*M_PI*uni_rand();
		y = ex1*sin(ex2);
		y_exists = true;
		return ex1*cos(ex2); // return x
	}
}

// data for individual simluations
// holds the block of data to output
bool is_first_run = true;
static FILE* out_file;
double *out_data;
uint32_t out_params = 0;
char *print_fmt;

void start_sim(const char* filename) {
  printf("Starting simluation with output to %s\n", filename);
  //char* filename = "outfile.bin";
  out_file = fopen(filename, "wb");
  // should do first_run init here
}

void first_run(uint32_t n_args) {
  is_first_run = false;
  printf("First run called...\n");
  out_params = n_args;

  // setup the format string
  print_fmt = calloc(out_params*4 + 4, sizeof(char));
  if (!print_fmt) {
    fprintf(stderr, "Memory allocation error\n");
    exit(EXIT_FAILURE);  
  }
  print_fmt[0]='\0';
  // add to the format string
  for (uint32_t i=0; i < n_args - 1; i++) {
    strncat(print_fmt, "%f, ", 4);
  }
  strncat(print_fmt, "%f\n", 4);

  // alloc the buffer of doubles
  out_data = calloc(out_params, sizeof(double));
  if (!out_data) {
    fprintf(stderr, "Memory allocation error\n");
    exit(EXIT_FAILURE);  
  }

  // write the num of cols to the output file
  fwrite(&n_args, sizeof(uint32_t), 1, out_file);  
}

void end_sim(void) {
  fclose(out_file);
  free(print_fmt);
  free(out_data);
  is_first_run = true;
  printf("Finished simulation\n");
}

void write_dbl(double d) {
  fwrite(&d, sizeof(double), 1, out_file);
}

void write_dbls(uint32_t n_args, ...) {
  if (is_first_run) first_run(n_args);
  va_list args;
  va_start (args, n_args);

  double t;
  // add to the array
  for(uint32_t i=0; i < out_params; i++) {
    //out_data[i] = va_arg(args, double);
    t = va_arg(args, double);
    *(out_data+i) = t;
  }
  fwrite(out_data, sizeof(double), out_params, out_file);  

  va_end(args);
}

// prints a new line
void print_nl(void) {
  printf("\n");
}

// takes a single double and prints it to stdout
void print_dbl(double arg) {
  printf("%f, ", arg);
}

// takes an inital param value and varlist of doubles and attempts to print them to stdout
void print_dbls(uint32_t n_args, ...) {
  if (is_first_run) first_run(n_args);
  va_list args;
  va_start (args, n_args);
  vprintf(print_fmt, args);
  va_end(args);
}


