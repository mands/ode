// Run-time library for the ODE Compiler
// Written in C99 rathn than LLVM for quick development
// Mainly utility functions for outputting balues to screen and files
#include "OdeStdLib.h"
#include "WELL512a.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#define M_PI 3.14159265358979323846264338327

// internal func declarations
extern void sincos (double x, double *sinx, double *cosx);
void OdeRandInit(void);

// Add library support for...
// multiple file opens within single ode file - needs compiler support
// 

// Library Lifecycle ///////////////////////////////////////////////////////////////////////////////
// sets up the global enviornment for all simluations
void OdeInit(void) {
    puts("Initialising the Ode Solver environment");

    OdeRandInit();
    // any other global initialisation
}

void OdeShutdown(void) {
    puts("Shutting down the Ode Solver environment");
}


// Simulation Lifecycle & File Output //////////////////////////////////////////////////////////////
// data for individual simluations
static FILE* outFile;
static double* outData;
static uint64_t outSize;

void OdeStartSim(const char* const restrict filename, const uint64_t numArgs) {
    printf("Starting simulation with output to %s\n", filename);
    outFile = fopen(filename, "wb");

    // allocate the outData buffer
    outSize = numArgs+1;
    outData = calloc(sizeof(double), outSize);

    // setup file header/first_run init here
    // write the num of cols as the header of the output file
    fwrite(&outSize, sizeof(uint64_t), 1, outFile);
}

void OdeStopSim(void) {
    printf("Finished simulation, final stored time - %g\n", outData[0]);
    fclose(outFile);
    free(outData);
}

// write state to disk
void OdeWriteState(const double time, const double* const restrict state) {
    static uint64_t outIdx;
    // build the output buffer
    outData[0] = time;
    for (outIdx = 1; outIdx < outSize; ++outIdx) {
        outData[outIdx] = state[outIdx-1];
    }
    // we use fwrite rather than write syscall as want buffering due to small data byte-count
    fwrite(outData, sizeof(double), outSize, outFile);  
}

// Random Number Generation ////////////////////////////////////////////////////////////////////////
// seed the PRNG using /dev/urandom
void OdeRandInit(void) {
    uint32_t seed[16];
    FILE *file = fopen("/dev/urandom", "rb");
    fread(seed, sizeof(uint32_t), 16, file);
    fclose(file);
    InitWELLRNG512a(seed);
}

// return a random number between 0 and 1 with uniform distribution
inline double OdeRandUniform(void) {
  return WELLRNG512a();
}

// return a random number between 0 and 1 with normal distribution
// used to generate weiner processes
double OdeRandNormal(void) {
    static double y, cosVal, sinVal;
    static bool yExists = false;
    if (yExists == true) {
        yExists = false;
        return y;
    } else {
        double ex1 = sqrt(-2*log(OdeRandUniform()));
        double ex2 = 2*M_PI*OdeRandUniform();
        sincos(ex2, &sinVal, &cosVal);
        y = ex1*sinVal;
        yExists = true;
        return ex1*cosVal;
    }
}

// Solver Helper Functions /////////////////////////////////////////////////////////////////////////
// used by the Reflected SDE Solver to project an array of doubles onto a simplex (0 < x_n < 1 and sum(x) == 1)
void OdeProjectVector(double* xs, const double xsSize) {
    // sort into new array y
    ys = calloc(sizeof(double), xsSize);
    memcpy(ys, xs, xsSize * sizeof(double));
    qsort(ys, xsSize, sizeof(double), cmpDouble);

    // iterate over y elems w/ running sum

    for (uint64_t i = 0; i < xsSize; ++i) {


    }

    // break on cond - tmax >= next elem


    // if didn't break - set tmax

    // update x based on y array - tmax
}

// double compare fucntion for qsort
int cmpDouble(const void *x, const void *y) {
    double xx = *(double*)x, yy = *(double*)y;
    if (xx < yy) return -1;
    if (xx > yy) return  1;
    return 0;
}
