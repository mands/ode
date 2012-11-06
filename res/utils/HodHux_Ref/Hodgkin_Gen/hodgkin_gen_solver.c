/* Hodgkin Model
** A simple ODE model for benchmarking purposes
** MODIFIED - to make more complex for benchmarking
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#include "odelibrary.h"

// func declarations
void modelInitials(double* STATE);
void modelLoop (double time, double* STATE, double* DELTA);
void modelSolver(uint64_t num, double* STATE, double* DELTA);

// we assume this function would be optimised as is a library func
void modelSolver(uint64_t num, double* STATE, double* DELTA) {
    init();
    // setup file output
    uint64_t out_num = num+1;
    start_sim("hodhux_c_gen.bin", out_num);

    // sim params
    static const double start_time = 0; 
    static const double stop_time = 60; // switch upto 600s for 10m of sim, 20 APs and long (~1m) computational time
    static const double time_step = 0.00001; 
    static const uint64_t period = 100000;

    // alloc state vals
    static double time;

    // alloc the buffer of doubles
    double out_data[out_num];

    // euler loop params
    time = start_time;
    uint64_t cur_period = 1;
    uint64_t cur_loop = 0;

    modelInitials(STATE);
    // copy data into output buffer
    out_data[0] = time;
    for (uint64_t i = 1; i < out_num; ++i) {
        out_data[i] = STATE[i-1];
    }
    write_dbls(out_data, out_num);

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
            write_dbls(out_data, out_num);
            cur_period = 1;
        } else {
            cur_period ++;
        }
    } while (time < stop_time);
    end_sim();
    shutdown();
}

