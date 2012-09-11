// ODE Runtime Library Header file
#ifndef ODE_LIBRARY_H
#define ODE_LIBRARY_H
#include <stdint.h>

double c_test(double arg);
// system specific
void init(void);
void shutdown(void);

// math funcs
double uni_rand(void);
double norm_rand(void);

// individual iteration specific
void start_sim();
void end_sim();

// iteration output
void write_dbl(double d);
void write_dbls(uint32_t n_args, ...);
void print_nl(void);
void print_dbl(double arg);
void print_dbls(uint32_t n_args, ...);

#endif // ODE_LIBRARY_H
