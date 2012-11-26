// This program plus implements a basic euler solver using OdeLibrary and OdeModel.o

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h> 
#include <string.h>
#include <math.h>

#include "OdeLibrary.h"
#include "OdeModel.h"

void solverInit(void);
void solverRun(void);
void solverShutdown(void);

int main(void) {
    // main code to setup, run, and shutdown the simulation
    // initialise Solver and Ode StdLib
    solverInit();
    // main solver loop
    solverRun();
    // shutdown solvers
    solverShutdown();
    return 0;
}

// initialise Ode StdLib
void solverInit(void) {
    // Ode Stdlib init & setup file output
    OdeInit();
    OdeStartSim(OdeParamOutput, OdeParamStateSize);
    // populate the initial vals and write to disk
    double state[OdeParamStateSize];
    OdeModelInitials(OdeParamStartTime, state);
    OdeWriteState(OdeParamStartTime, state);
}

void solverRun(void) {
    // alloc state vals - using C99 VLA
    double state[OdeParamStateSize];
    double delta[OdeParamStateSize];
    // euler loop params
    double time = OdeParamStartTime;
    const uint64_t periodInterval = (uint64_t)(floor(OdeParamPeriod / OdeParamTimestep));
    uint64_t curPeriod = 1;
    uint64_t curLoop = 0;

    // main forward euler loop
    do {
        // set the time
        curLoop++;
        time = OdeParamStartTime + curLoop * OdeParamTimestep;

        // update the deltas
        OdeModelLoop(time, state, delta);

        // update the state
        for (uint64_t i = 0; i < OdeParamStateSize; ++i) {
            state[i] += delta[i] * OdeParamTimestep;
        }

        // write out at sample period
        if (curPeriod == periodInterval) {
            OdeWriteState(time, state);
            curPeriod = 1;
        } else {
            curPeriod ++;
        }
    } while (time < OdeParamStopTime);
}

// shutdown simulation - free mem, etc.
void solverShutdown(void) {
    OdeEndSim();
    OdeShutdown();
}
