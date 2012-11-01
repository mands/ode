// ODE Runtime Library Header file
#ifndef ODE_LIBRARY_H
#define ODE_LIBRARY_H
#include <stdint.h>

// system specific
void init(void);
void shutdown(void);

// individual iteration specific
void start_sim(const char* filename);
void end_sim(void);

// iteration output
void write_dbls(const uint64_t n_args, const double *dbls);

#endif // ODE_LIBRARY_H
