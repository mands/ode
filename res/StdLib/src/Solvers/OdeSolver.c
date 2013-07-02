// This program plus implements a basic euler (ODE&SDE) solver using OdeLibrary and OdeModel.o
// Acts as a test for the OdeModel.h FFI
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <inttypes.h> 
#include <string.h>
#include <math.h>

#include "OdeStdLib.h"
#include "OdeModel.h"

void solverInit(double* const restrict state);
void solverRun(double* const restrict state);
void solverShutdown(void);

// DEBUG CODE
//void _printArray(const double* const restrict xs, const uint64_t xsSize);
//void _printLongArray(const double* const restrict xs, const uint64_t xsSize);
//double min(const double* const restrict xs, const uint64_t xsSize);
//double max(const double* const restrict xs, const uint64_t xsSize);

int main(void) {
    if (OdeParamSimType == Rre || OdeParamSimType == Hybrid) {
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
    if (OdeParamStartTime >= OdeParamStartOutputTime) OdeWriteState(OdeParamStartTime, state);
}

void solverRun(double* const restrict state) {
    // alloc delta and weiner vals - using C99 VLA
    double delta[OdeParamStateSize];
    double weiner[OdeParamStateSize];

    // euler loop params
    double time;
    // const double sqrtTimestep = sqrt(OdeParamTimestep); -- not needed anymore
    uint64_t curPeriod = 1;
    uint64_t curLoop = 0;
    uint64_t stateIdx;
    uint64_t numProj = 0;
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
                // wiener val already includes *sqrt(dt)*OdeRandNormal()
                state[stateIdx] += (delta[stateIdx] * OdeParamTimestep) + (weiner[stateIdx]); 
                //printf("Weiner - %g\n", weiner[stateIdx]);
            }
            break;
        case ProjSde:
            // update the deltas and weiners
            OdeModelRHS(time, state, delta, weiner);
            //puts("In ProjSDE Loop");
            // update the state - y' = y + dy*h + dW*sqrt(h)*rand
            for (stateIdx = 0; stateIdx < OdeParamStateSize; ++stateIdx) {
                // wiener val already includes *sqrt(dt)*OdeRandNormal()
                state[stateIdx] += (delta[stateIdx] * OdeParamTimestep) + (weiner[stateIdx]);
                //printf("Weiner - %g\n", weiner[stateIdx]);
            }
            // now check if we're out, proj if so
            for (stateIdx = 0; stateIdx < OdeParamStateSize; ++stateIdx) {
                // NOTE - edit the bounds check to be similar to Ode/LLVM one
                //if ((state[stateIdx] < 0) || (state[stateIdx] > 1)) {
                if (!((state[stateIdx] >= 0) && (state[stateIdx] <= 1))) {
                    //puts("State Val is < 0 || > 1");
                    //puts("Projecting vector");
                    //_printArray(state, OdeParamStateSize);
                    numProj++;
                    OdeProjectVector(state, OdeParamStateSize);
                    //_printArray(state, OdeParamStateSize);
                    break;
                }
            }

/*            if ((min(state, OdeParamStateSize) < 0) || (min(state, OdeParamStateSize) > 1)) {*/
/*                numProj++;*/
/*                OdeProjectVector(state, OdeParamStateSize);*/
/*            }*/
            break;
        }

        // write out at sample period
        if (curPeriod == OdeParamPeriodInterval) {
            if (time >= OdeParamStartOutputTime) OdeWriteState(time, state);
            curPeriod = 1;
        } else {
            ++curPeriod;
        }
    } while (time < OdeParamAdjustedStopTime);
    printf("Simulation stop time %g, OdeParamAdjustedStopTime - %g\n", time, OdeParamAdjustedStopTime);
    printf("Number of Projections - %" PRIu64 ", Number of timesteps - %g\n", numProj, (OdeParamStopTime-OdeParamStartTime) / OdeParamTimestep);
}

/*
// DEBUG CODE
double min(const double* const restrict xs, const uint64_t xsSize) {
  double cur = xs[0];
  for (uint64_t i = 0; i < xsSize; ++i) { 
    if (xs[i] < cur) cur = xs[i];
  }
  return cur;
}


double max(const double* const restrict xs, const uint64_t xsSize) {
  double cur = xs[0];
  for (uint64_t i = 0; i < xsSize; ++i) { 
    if (xs[i] > cur) cur = xs[i];
  }
  return cur;
}

void _printLongArray(const double* const restrict xs, const uint64_t xsSize) {
    printf("[");
    for (uint64_t i = 0; i < (xsSize-1); ++i) {
        printf("%.17g, ", xs[i]);
    }
    printf("%.17g]\n", xs[xsSize-1]);
}
*/


// shutdown simulation - free mem, etc.
void solverShutdown(void) {
    OdeStopSim();
    OdeShutdown();
}

