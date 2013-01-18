// ODE Runtime Library Header file
#ifndef ODE_STDLIB_H
#define ODE_STDLIB_H
#include <stdint.h>

// system specific
extern void OdeInit(void);
extern void OdeShutdown(void);

// individual iteration specific
extern void OdeStartSim(const char* const restrict filename, const uint64_t numArgs);
extern void OdeStopSim(void);

// iteration output
extern void OdeWriteState(const double time, const double* const restrict state);

// Random Number Generation
extern inline double OdeRandUniform(void);
extern double OdeRandNormal(void);

// Solver helper functions
void OdeProjectVector(double* const restrict xs, const uint64_t xsSize);

#endif // ODE_STDLIB_H

