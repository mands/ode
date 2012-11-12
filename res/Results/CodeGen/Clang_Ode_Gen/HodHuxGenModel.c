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
#define NUM_PARAMS 4

#define V 0
#define M 1
#define H 2
#define N 3

// func declarations
void modelSolver(const uint64_t num, double* restrict STATE, double* restrict DELTA, const double start_time, const double stop_time, const double time_step, const uint64_t period);
void modelInitials(double* restrict STATE);
void modelLoop (double time, const double* restrict STATE, double* restrict DELTA);

int main(void) {
    // need to alloc the data here
    double STATE[NUM_PARAMS];
    double DELTA[NUM_PARAMS];

    // sim params
    static const double start_time = 0; 
    static const double stop_time = 60; // switch upto 600s for 10m of sim, 20 APs and long (~1m) computational time
    static const double time_step = 0.00001; 
    static const uint64_t period = 100000;

    modelSolver(NUM_PARAMS, STATE, DELTA, start_time, stop_time, time_step, period);
    return(0);
}



void modelInitials(double* restrict STATE) {
    // load initial values
    STATE[V] = -75.0;
    STATE[M] = 0.05;
    STATE[H] = 0.6;
    STATE[N] = 0.325;
}


void modelLoop (double time, const double* restrict STATE, double* restrict DELTA) {
    // global model constants
    double E_R = -75.0;
    double Cm = 1.0;

    // setup deltas

    // sodium channel m gate
    double alpha_m = (-0.1 * (STATE[V] + 50.0)) / (exp((-1.0 * (STATE[V]+50.0))/10.0) - 1.0);
    double beta_m = 4.0 * exp((-1.0 * (STATE[V]+75.0))/18.0);
    DELTA[M] = alpha_m * (1.0 - STATE[M]) - (beta_m * STATE[M]);

    // sodium channel h gate
    double alpha_h = 0.07 * exp((-1.0 * (STATE[V]+75.0)) / 20.0);
    double beta_h = 1.0 / (exp((-1.0*(STATE[V]+45.0))/ 10.0) + 1.0);
    DELTA[H] = alpha_h*(1.0-STATE[H]) - (beta_h*STATE[H]);

    // sodium channel
    double g_Na = 120.0;
    double E_Na = E_R + 115.0;
    double i_Na = g_Na*pow(STATE[M], 3.000001)*STATE[H]*(STATE[V] - E_Na);

    // potassium channel n gate
    double alpha_n = (-0.01 * (STATE[V] + 65.0)) / (exp((-1.0 * (STATE[V] + 65.0)) / 10.0) - 1.0);
    double beta_n = 0.125 * exp((STATE[V]+75.0) / 80.0);
    DELTA[N] = alpha_n*(1.0-STATE[N]) - (beta_n*STATE[N]);

    // potassium channel
    double g_K = 36.0;
    double E_K = E_R - 12.0;
    double i_K = g_K * pow(STATE[N], 4.000001) * (STATE[V] - E_K);

    // leakage current
    double g_L = 0.3;
    double E_L = E_R + 10.613;
    double i_L = g_L * (STATE[V] - E_L);

    // stimulus current
    double i_Stim = (time >= 10.0 && time <= 10.5) ? 20.0 : 0.0;
    // main double
    DELTA[V] = ((-1.0 * (-1.0 * i_Stim + i_Na + i_K + i_L)) / Cm);
}

