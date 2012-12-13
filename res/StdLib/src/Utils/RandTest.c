// generate a set of random numbers to plot and test the PRNG
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#include "OdeStdLib.h"

void genUniformRands(const uint64_t maxIters);
void genNormalRands(const uint64_t maxIters);

int main(int argc, char** argv) {
	if (argc != 2) {
		fprintf(stderr, "Usage - RandTest num\n");
		exit(EXIT_FAILURE);
	}

    const uint64_t maxIters = (uint64_t)strtoul(argv[1], NULL, 0);
    OdeInit();
    genUniformRands(maxIters);
    genNormalRands(maxIters);
    OdeShutdown();
}

void genUniformRands(const uint64_t maxIters) {
    OdeStartSim("uniform_rands.bin", 0);
    double v;
    for (uint64_t i = 0; i < maxIters; ++i) {
        v = OdeRandUniform();
        OdeWriteState(v, NULL);
    }
    OdeStopSim();
}

void genNormalRands(const uint64_t maxIters) {
    OdeStartSim("normal_rands.bin", 0);
    double v;
    for (uint64_t i = 0; i < maxIters; ++i) {
        v = OdeRandNormal();
        OdeWriteState(v, NULL);
    }
    OdeStopSim();
}
