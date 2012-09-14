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
    double stop = 50000;
    double step = 0.001; 
    uint32_t sample = 100;
    const char* filename = "output.bin";

    init();
    simulate(start, stop, step, sample, filename);
    shutdown();

    exit(EXIT_SUCCESS);
}

void simulate(double start_time, double stop_time, double time_step, double period, const char* out_file) {
    // euler loop params
    uint64_t cur_period = 1;
    uint64_t cur_loop = 0;
    double time = start_time;
    start_sim(out_file);

    // setup initial values
    double m = 0.05;
    double h = 0.6;
    double n = 0.325;
    double V = -75;
    write_dbls(5, time, V, h, m, n);

    // main forward euler loop
    do {
        // set the time
        cur_loop++;
        time = start_time + cur_loop * time_step;

        // global model constants
        const double E_R = -75;
        const double Cm = 1;
        const double g_Na = 120;
        const double g_K = 36;
        const double g_L = 0.3;

        // setup deltas
        // sodium channel
        const double E_Na = E_R + 115;
        const double i_Na = g_Na*pow(m,3)*h*(V - E_Na);

        // sodium channel m gate
        const double alpha_m = (-0.1 * (V + 50)) / (exp((-1 * (V+50))/10) - 1);
        const double beta_m = 4 * exp((-1 * (V+75))/18);
        const double d_m = alpha_m * (1 - m) - (beta_m * m);

        // sodium channel h gate
        const double alpha_h = 0.07 * exp((-1 * (V+75)) / 20);
        const double beta_h = 1 / (exp((-1*(V+45))/ 10) + 1);
        const double d_h = alpha_h*(1-h) - (beta_h*h);

        // potassium channel
        const double E_K = E_R - 12;
        const double i_K = g_K * pow(n,4) * (V - E_K);

        // potassium channel n gate
        const double alpha_n = (-0.01 * (V + 65)) / (exp((-1 * (V + 65)) / 10) - 1);
        const double beta_n = 0.125 * exp((V+75) / 80);
        const double d_n = alpha_n*(1-n) - (beta_n*n);

        // leakage current
        const double E_L = E_R + 10.613;
        const double i_L = g_L * (V - E_L);

        // how to represent i_Stim
        const double i_Stim = (time >= 10 && time <= 10.5) ? 20 : 0;
        // main double
        const double d_V = (-1 * (-1 * i_Stim + i_Na + i_K + i_L)) / Cm;

        // update doubles to next double
        m += d_m * time_step;
        h += d_h * time_step;
        n += d_n * time_step;
        V += d_V * time_step;  

        // write out at sample period
        if (cur_period == period) {
            write_dbls(5, time, V, h, m, n);
            cur_period = 1;
        } else {
            cur_period ++;
        }
    } while (time < stop_time);
    end_sim();
}

