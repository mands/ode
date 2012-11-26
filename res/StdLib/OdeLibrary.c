// Run-time library for the ODE Compiler
// Written in C99 rathn than LLVM for quick development
// Mainly utility functions for outputting balues to screen and files
#include "OdeLibrary.h"

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

// sets up the global enviornment for all simluations
void OdeInit(void) {
    puts("Initialising the Ode Solver environment");
    // any other global initialisation
}

void OdeShutdown(void) {
    puts("Shutting down the Ode Solver environment");
}

// data for individual simluations
// holds the block of data to output
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
    fclose(outFile);
    free(outData);
    puts("Finished simulation");
}

void OdeWriteState(const double time, const double* const restrict state) {
    // build the output buffer
    outData[0] = time;
    for (uint64_t i = 1; i < outSize; ++i) {
        outData[i] = state[i-1];
    }
    // we use fwrite rather than write syscall as want buffering due to small data byte-count
    fwrite(outData, sizeof(double), outSize, outFile);  
}

