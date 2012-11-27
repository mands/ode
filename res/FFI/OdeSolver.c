// This program plus implements a basic euler solver using OdeLibrary and OdeModel.o

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h> 
#include <string.h>
#include <math.h>

#include "OdeLibrary.h"
#include "OdeModel.h"

void solverInit(double* const restrict state);
void solverRun(double* const restrict state);
void solverShutdown(void);

int main(void) {
    // main code to setup, run, and shutdown the simulation
    // main data structs passed around solver
    double state[OdeParamStateSize];
    // initialise Solver and Ode StdLib
    solverInit(state);
    // main solver loop
    solverRun(state);
    // shutdown solvers
    solverShutdown();
    return 0;
}

// initialise Ode StdLib
void solverInit(double* const restrict state) {
    // Ode Stdlib init & setup file output
    OdeInit();
    OdeStartSim(OdeParamOutput, OdeParamStateSize);
    // populate the initial vals and write to disk
    OdeModelInitials(OdeParamStartTime, state);
    OdeWriteState(OdeParamStartTime, state);
}

void solverRun(double* const restrict state) {
    // alloc delta vals - using C99 VLA
    double delta[OdeParamStateSize];
    // euler loop params
    double time;
    uint64_t curPeriod = 1;
    uint64_t curLoop = 0;
    uint64_t stateIdx;

    // main forward euler loop
    do {
        // set the time
        ++curLoop;
        time = OdeParamStartTime + curLoop * OdeParamTimestep;

        // update the deltas
        OdeModelLoop(time, state, delta);

        // update the state
        for (stateIdx = 0; stateIdx < OdeParamStateSize; ++stateIdx) {
            state[stateIdx] += delta[stateIdx] * OdeParamTimestep;
        }

        // write out at sample period
        if (curPeriod == OdeParamPeriodInterval) {
            OdeWriteState(time, state);
            curPeriod = 1;
        } else {
            ++curPeriod;
        }
    } while (time < OdeParamAdjustedStopTime);
    printf("Simulation stop time %g, OdeParamAdjustedStopTime - %g\n", time, OdeParamAdjustedStopTime);
}

// shutdown simulation - free mem, etc.
void solverShutdown(void) {
    OdeStopSim();
    OdeShutdown();
}
