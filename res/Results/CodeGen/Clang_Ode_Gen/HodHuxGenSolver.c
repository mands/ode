/* Hodgkin Model
** A simple ODE model for benchmarking purposes
** MODIFIED - to make more complex for benchmarking
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#include "OdeLibrary.h"

// func declarations
void modelSolver(const uint64_t num, double* restrict STATE, double* restrict DELTA, const double start_time, const double stop_time, const double time_step, const uint64_t period);
void modelInitials(double* restrict STATE);
void modelLoop (double time, const double* restrict STATE, double* restrict DELTA);

// we assume this function would be optimised as is a library func
void modelSolver(const uint64_t num, double* restrict STATE, double* restrict DELTA, const double start_time, const double stop_time, const double time_step, const uint64_t period) {
    init();
    // setup file output
    uint64_t out_num = num+1;
    // alloc the buffer of doubles
    double out_data[out_num];
    startSim("HodHuxCGen.bin", out_num);

    // euler loop params
    static double time;
    time = start_time;
    static uint64_t cur_period = 1;
    static uint64_t cur_loop = 0;

    // setup init vals and write out
    modelInitials(STATE);
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
        time = start_time + cur_loop * time_step;

        // update the deltas
        modelLoop(time, STATE, DELTA);

        // update the state
        for (uint64_t i = 0; i < num; ++i) {
            STATE[i] += DELTA[i] * time_step;
        }

        // write out at sample period
        if (cur_period == period) {
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
    } while (time < stop_time);
    endSim();
    shutdown();
}

