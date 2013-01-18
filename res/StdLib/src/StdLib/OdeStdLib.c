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

// Global variables

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
static uint64_t stateSize;

void OdeStartSim(const char* const restrict filename, const uint64_t numArgs) {
    printf("Starting simulation with output to %s\n", filename);
    outFile = fopen(filename, "wb");

    // allocate the outData buffer
    outSize = numArgs+1;
    stateSize = numArgs;
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
    // build the output buffer
    outData[0] = time;
    memcpy(&outData[1], state, stateSize * sizeof(double));
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
// adapted from paper - "Projection Onto A Simplex" - Chen, Yunmei & Ye, Xiaojing
int cmpInvDouble(const void * const x, const void * const y);

void OdeProjectVector(double* const restrict xs, const uint64_t xsSize) {
    // stack-alloc the tmp array  - should be fine as array not large, plus v.quick, only dec the SP
    double ss[xsSize];
    // sort into decending order in new array s
    memcpy(ss, xs, xsSize * sizeof(double));
    qsort(ss, xsSize, sizeof(double), cmpInvDouble);

    // iterate over y elems w/ running sum
    double tmpSum = 0;
    double tMax = 0;
    for (uint64_t i = 0; i < (xsSize-1); ++i) {
        tmpSum += ss[i];
        tMax = (tmpSum-1)/(i+1);
        //printf("Iter - tmpSum - %g, tMax - %g\n", tmpSum, tMax);
        // break on cond - tmax >= next elem
        if (tMax >= ss[i+1]) goto finish;
    }
    // if didn't break early - set tmax
    tMax = (tmpSum + ss[xsSize-1] - 1) / xsSize;

finish:
    //printf("Final - tMax - %g\n", tMax);
    // update x in place
    for (uint64_t i = 0; i < xsSize; ++i) {
        xs[i] = fmax(xs[i] - tMax, 0);
    }

}

// double compare fucntion for qsort
int cmpInvDouble(const void * const x, const void * const y) {
    double xx = *(double*)x, yy = *(double*)y;
    if (xx < yy) return 1;
    if (xx > yy) return -1;
    return 0;
}
