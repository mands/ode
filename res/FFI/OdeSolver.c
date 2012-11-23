// This dummy peice of code both acts as a test for the OdeModel object file plus implements a basic
// euler solver using OdeLibrary

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h> 
#include <string.h>
#include <math.h>

#include "OdeLibrary.h"
#include "OdeModel.h"

void dumpParams(void);
void modelSolver(void);

int main(int argc, char** argv) {
    switch(argc) {
        case 2:            
            // we actually don't care what the arg value is, just that it exists
            dumpParams();
            break;
        case 1:
            modelSolver();
            break;
        default:
    		fprintf(stderr, "Usage - OdeSolver [DUMP]\n");
            return(1);
    }
    return(0);
}

void dumpParams(void) {
    puts("Dumping Ode Model Params\n");
    puts("Ode Model Timing");
    printf("OdeParamStartTime\t- %g\n", OdeParamStartTime);
    printf("OdeParamEndTime\t\t- %g\n", OdeParamEndTime);
    printf("OdeParamTimeStep\t- %g\n\n", OdeParamTimestep);

    puts("Ode Model Output");
    printf("OdeParamPeriod\t\t- %" PRIu64 "\n", OdeParamPeriod);
    printf("OdeParamOutput\t\t- %s\n\n", &OdeParamOutput);

    puts("Ode Model Num Params");
    printf("OdeParamNumParams\t- %" PRIu64 "\n", OdeParamNumParams);

}

void modelSolver(void) {
    init();
    // setup file output
    uint64_t out_num = OdeParamNumParams+1;
    startSim(&OdeParamOutput, out_num);
    // statically alloc the buffer of doubles
    double out_data[out_num]; // [OdeParamNumParams+1];

    // alloc state vals - using C99 VLA
    double STATE[OdeParamNumParams];
    double DELTA[OdeParamNumParams];

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
        for (uint64_t i = 0; i < OdeParamNumParams; ++i) {
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
    } while (time < OdeParamEndTime);
    endSim();
    shutdown();
}
