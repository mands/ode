/* Hodgkin Model
** A simple ODE model for benchmarking purposes
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#include "odelibrary.h"

// func declarations
void simulate(double start, double stop, double step, double sample, const char* out_file);

int main(void) {
    double start = 0; 
    double stop = 1000;
    double step = 0.001; 
    uint32_t sample = 10;
    const char* filename = "output.bin";

    init();
    simulate(start, stop, step, sample, filename);
    shutdown();

    exit(EXIT_SUCCESS);
}

void simulate(double start_time, double stop_time, double time_step, double period, const char* out_file) {
    // euler loop params
    uint32_t cur_period = 1;
    uint32_t cur_loop = 0;
    const double max_time = stop_time + (time_step/2);
    start_sim(out_file);

    // setup initial values
    write_dbls(2, start_time, start_time + (cur_loop * time_step));

    // main forward euler loop
    for (double time = start_time + time_step; time < max_time; time += time_step) {

        cur_loop++;
        double calc_time = start_time + (cur_loop * time_step);

        // write out at sample period
        if (cur_period == period) {
            write_dbls(5, time, calc_time);
            cur_period = 1;
        } else {
            cur_period ++;
        }
    }
    end_sim();
}

