// This program plus implements a basic euler solver using OdeLibrary and OdeModel.o

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h> 
#include <string.h>
#include <math.h>

#include "OdeLibrary.h"
#include "OdeModel.h"

void modelSolver(void);

int main(void) {
    modelSolver();
    return(0);
}

void modelSolver(void) {
    init();
    // setup file output
    uint64_t out_num = OdeParamStateSize+1;
    startSim(&OdeParamOutput, out_num);
    // statically alloc the buffer of doubles
    double out_data[out_num]; // [OdeParamNumParams+1];

    // alloc state vals - using C99 VLA
    double STATE[OdeParamStateSize];
    double DELTA[OdeParamStateSize];

    // euler loop params
    static double time;
    time = OdeParamStartTime;
    static uint64_t cur_period = 1;
    static uint64_t cur_loop = 0;

    // setup the initial vals
    OdeModelInitials(time, STATE);

    // copy data into output buffer
    out_data[0] = time;
    for (uint64_t i = 1; i < out_num; ++i) {
        out_data[i] = STATE[i-1];
    }
    writeDbls(out_data, out_num);

    // main forward euler loop
    do {
        // set the time
        cur_loop++;
        time = OdeParamStartTime + cur_loop * OdeParamTimestep;

        // update the deltas
        OdeModelLoop(time, STATE, DELTA);

        // update the state
        for (uint64_t i = 0; i < OdeParamStateSize; ++i) {
            STATE[i] += DELTA[i] * OdeParamTimestep;
        }

        // write out at sample period
        if (cur_period == OdeParamPeriod) {
            // copy data into output buffer
            out_data[0] = time;
            for (uint64_t i = 1; i < out_num; ++i) {
                out_data[i] = STATE[i-1];
            }
            writeDbls(out_data, out_num);
            cur_period = 1;
        } else {
            cur_period ++;
        }
    } while (time < OdeParamStopTime);
    endSim();
    shutdown();
}
