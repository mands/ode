// This program plus implements a basic euler (ODE&SDE) solver using OdeLibrary and OdeModel.o
// Acts as a test for the OdeModel.h FFI
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h> 
#include <string.h>
#include <math.h>

#include "OdeStdLib.h"
#include "OdeModel.h"

void solverInit(double* const restrict state);
void solverRun(double* const restrict state);
void solverShutdown(void);

int main(void) {
    if (OdeParamSimType == Rre) {
        fprintf(stderr, "Cannot simulation an RRE model\n");
        return(1);
    }
    if (OdeParamTimestep == 0.0) {
        fprintf(stderr, "Timestep must be greater than 0 for constant solvers\n");
        return(1);
    }

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
    // alloc delta and weiner vals - using C99 VLA
    double delta[OdeParamStateSize];
    double weiner[OdeParamStateSize];

    // euler loop params
    double time;
    const double sqrtTimestep = sqrt(OdeParamTimestep);
    uint64_t curPeriod = 1;
    uint64_t curLoop = 0;
    uint64_t stateIdx;

    // main forward euler loop
    do {
        // set the time
        ++curLoop;
        time = OdeParamStartTime + curLoop * OdeParamTimestep;

        switch (OdeParamSimType) {
        case Ode:
            // update the deltas
            OdeModelRHS(time, state, delta, NULL);
            // update the state - y' = y + dy*h
            for (stateIdx = 0; stateIdx < OdeParamStateSize; ++stateIdx) {
                state[stateIdx] += delta[stateIdx] * OdeParamTimestep;
            }
            break;
        case Sde:
            // update the deltas and weiners
            OdeModelRHS(time, state, delta, weiner);
            //puts("Start Loop");
            // update the state - y' = y + dy*h + dW*sqrt(h)*rand
            for (stateIdx = 0; stateIdx < OdeParamStateSize; ++stateIdx) {
                state[stateIdx] += (delta[stateIdx] * OdeParamTimestep)
                        + (weiner[stateIdx]*sqrtTimestep*OdeRandNormal());
                //printf("Weiner - %g\n", weiner[stateIdx]);
            }
            break;
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