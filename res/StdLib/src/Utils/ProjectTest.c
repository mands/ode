// generate a set of random numbers to plot and test the PRNG
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <inttypes.h>
#include <string.h>
#include <math.h>

#include "OdeStdLib.h"

void printArray(const double* const restrict xs, const uint64_t xsSize);
double sumArray(const double* const restrict xs, const uint64_t xsSize);
void randArray(double* const restrict xs, const uint64_t xsSize);
void projectBench(void);

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage - ProjectTest [nums]\n");
        exit(EXIT_FAILURE);
    }

    const uint64_t xsSize = (uint64_t)(argc - 1);
    double* xs = calloc(sizeof(double), xsSize);

    // fill the xs array
    for (uint64_t i = 0; i < xsSize; ++i) {
        xs[i] = strtod(argv[i+1], NULL);
    }

    OdeInit();

    // print the init array
    printArray(xs, xsSize);
    // project then print again
    OdeProjectVector(xs, xsSize);
    printArray(xs, xsSize);

    // run the benchmark
    // projectBench();

    OdeShutdown();
}



void printArray(const double* const restrict xs, const uint64_t xsSize) {
    printf("[");
    for (uint64_t i = 0; i < (xsSize-1); ++i) {
        printf("%g, ", xs[i]);
    }
    printf("%g]\n", xs[xsSize-1]);
}

double sumArray(const double* const restrict xs, const uint64_t xsSize) {
    double sum = 0;
    for (uint64_t i = 0; i < xsSize; ++i) {
        sum += xs[i];
    }
    return sum;
}

void randArray(double* const restrict xs, const uint64_t xsSize) {
    for (uint64_t i = 0; i < xsSize; ++i) {
        xs[i] = OdeRandNormal();
    }
}

void projectBench(void) {

    const uint64_t xsSize = 50;
    double xs[xsSize], ys[xsSize];
    // gen the rand array
    randArray(xs, xsSize);

    const uint64_t maxIters = 65536;
    // run the benchmark
    for (uint64_t i = 0; i < maxIters; ++i) {
        // reset the array then project
        memcpy(ys, xs, xsSize * sizeof(double));
        OdeProjectVector(ys, xsSize);
    }
    puts("Done");
    printArray(xs, xsSize);
    printArray(ys, xsSize);
    printf("Prev Sum - %g, Project Sum - %g\n", sumArray(xs, xsSize), sumArray(ys, xsSize));
}
