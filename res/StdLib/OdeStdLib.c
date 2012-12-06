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
static uint64_t outIdx;
void OdeWriteState(const double time, const double* const restrict state) {
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
double OdeRandNormal(void) {
    static double y;
    static bool y_exists = false;
    if (y_exists == true) {
        y_exists = false;
        return y;
    } else {
        double ex1 = sqrt(-2*log(OdeRandUniform()));
        double ex2 = 2*M_PI*OdeRandUniform();
        y = ex1*sin(ex2);
        y_exists = true;
        return ex1*cos(ex2); // return x
    }
}
