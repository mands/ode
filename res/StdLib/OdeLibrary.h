// ODE Runtime Library Header file
#ifndef ODE_LIBRARY_H
#define ODE_LIBRARY_H
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

#endif // ODE_LIBRARY_H

