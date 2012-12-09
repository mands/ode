// generate a set of random numbers to plot and test the PRNG
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#include "OdeStdLib.h"

const uint64_t MaxIters = 100000;

void genUniformRands(void);
void genNormalRands(void);

int main(void) {
    OdeInit();
    genUniformRands();
    genNormalRands();
    OdeShutdown();
}

void genUniformRands(void) {
    OdeStartSim("uniform_rands.bin", 0);
    double v;
    for (uint64_t i = 0; i < MaxIters; ++i) {
        v = OdeRandUniform();
        OdeWriteState(v, NULL);
    }
    OdeStopSim();
}

void genNormalRands(void) {
    OdeStartSim("normal_rands.bin", 0);
    double v;
    for (uint64_t i = 0; i < MaxIters; ++i) {
        v = OdeRandNormal();
        OdeWriteState(v, NULL);
    }
    OdeStopSim();
}
