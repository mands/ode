// ODE Runtime Library Header file
#ifndef ODE_LIBRARY_H
#define ODE_LIBRARY_H
#include <stdint.h>

// system specific
void init(void);
void shutdown(void);

// individual iteration specific
void startSim(const char* const restrict filename, const uint64_t nArgs);
void endSim(void);

// iteration output
void writeDbls(const double* const restrict dbls, const uint64_t nArgs);

#endif // ODE_LIBRARY_H

