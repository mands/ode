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
#define NUM_PARAMS 5

// func declarations
void modelSolver(void);
void modelInitials(const double time, double* V, double* m, double* h, double* n);
void modelLoop (const double time, const double V, const double m, const double h, const double n
                , double* restrict d_V, double* restrict d_m, double* restrict d_h, double* restrict d_n);

int main(void) {
    modelSolver();
    //exit(EXIT_SUCCESS);
    return(0);
}


void modelSolver(void) {
    init();
    // setup file output
    start_sim("hodhux_c.bin", NUM_PARAMS);

    // sim params
    static const double start_time = 0; 
    static const double stop_time = 60; // switch upto 600s for 10m of sim, 20 APs and long (~1m) computational time
    static const double time_step = 0.00001; 
    static const uint64_t period = 100000;

    // alloc state vals
    static double time;
    static double V;
    static double m;
    static double h;
    static double n;
    static double d_V;
    static double d_m;
    static double d_h;
    static double d_n;

    // statically alloc the buffer of doubles
    static double out_data[NUM_PARAMS];

    // euler loop params
    time = start_time;
    uint64_t cur_period = 1;
    uint64_t cur_loop = 0;

    modelInitials(time, &V, &m, &h, &n);
    // copy data into output buffer
    out_data[0] = time;
    out_data[1] = V;
    out_data[2] = m;
    out_data[3] = h;
    out_data[4] = n;
    write_dbls(out_data, NUM_PARAMS);

    // main forward euler loop
    do {
        // set the time
        cur_loop++;
        time = start_time + cur_loop * time_step;

        // update the deltas
        modelLoop(time, V, m, h, n, &d_V, &d_m, &d_h, &d_n);

        // update the state
        m += d_m * time_step;
        h += d_h * time_step;
        n += d_n * time_step;
        V += d_V * time_step;  

        // write out at sample period
        if (cur_period == period) {
            // copy data into output buffer
            out_data[0] = time;
            out_data[1] = V;
            out_data[2] = m;
            out_data[3] = h;
            out_data[4] = n;
            write_dbls(out_data, NUM_PARAMS);
            cur_period = 1;
        } else {
            cur_period ++;
        }
    } while (time < stop_time);
    end_sim();
    shutdown();
}


void modelInitials(const double time, double* V, double* m, double* h, double* n) {
    // load initial values
    *V = -75.0;
    *m = 0.05;
    *h = 0.6;
    *n = 0.325;
}


void modelLoop ( const double time, const double V, const double m, const double h, const double n
                , double* restrict d_V, double* restrict d_m, double* restrict d_h, double* restrict d_n) {
    // global model constants
    static const double E_R = -75.0;
    static const double Cm = 1.0;

    // setup deltas

    // sodium channel m gate
    const double alpha_m = (-0.1 * (V + 50.0)) / (exp((-1.0 * (V+50.0))/10.0) - 1.0);
    const double beta_m = 4.0 * exp((-1.0 * (V+75.0))/18.0);
    *d_m = alpha_m * (1.0 - m) - (beta_m * m);

    // sodium channel h gate
    const double alpha_h = 0.07 * exp((-1.0 * (V+75.0)) / 20.0);
    const double beta_h = 1.0 / (exp((-1.0*(V+45.0))/ 10.0) + 1.0);
    *d_h = alpha_h*(1.0-h) - (beta_h*h);

    // sodium channel
    static const double g_Na = 120.0;
    const double E_Na = E_R + 115.0;
    const double i_Na = g_Na*pow(m, 3.000001)*h*(V - E_Na);


    // potassium channel n gate
    const double alpha_n = (-0.01 * (V + 65.0)) / (exp((-1.0 * (V + 65.0)) / 10.0) - 1.0);
    const double beta_n = 0.125 * exp((V+75.0) / 80.0);
    *d_n = alpha_n*(1.0-n) - (beta_n*n);

    // potassium channel
    static const double g_K = 36.0;
    const double E_K = E_R - 12.0;
    const double i_K = g_K * pow(n, 4.000001) * (V - E_K);


    // leakage current
    static const double g_L = 0.3;
    const double E_L = E_R + 10.613;
    const double i_L = g_L * (V - E_L);


    // stimulus current
    const double i_Stim = (time >= 10.0 && time <= 10.5) ? 20.0 : 0.0;
    // main double
    *d_V = ((-1.0 * (-1.0 * i_Stim + i_Na + i_K + i_L)) / Cm);
}

