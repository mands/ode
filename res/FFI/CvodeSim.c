#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <inttypes.h>
#include <string.h>
#include <math.h>

// CVODE Libraries
// ===============
#include <cvode/cvode.h>				/* Prototypes for CVODE fcts., consts. */
#include <cvode/cvode_dense.h>       /* use CVDENSE linear solver */
#include <cvode/cvode_band.h>        /* use CVBAND linear solver */
#include <cvode/cvode_diag.h>        /* use CVDIAG linear solver */
#include <nvector/nvector_serial.h>		/* Serial N_Vector types, fcts., macros */
// #include "sundials/sundials_dense.h"	/* Definitions DlsMat DENSE_ELEM */
// #include "sundials/sundials_types.h"	/* Definition of type realtype */

// Ode Model Interface
#include "OdeLibrary.h"
#include "OdeModel.h"

// prototypes
int odeWrapperF(double time, N_Vector y, N_Vector ydot, void* user_data);
void checkFlag(int flagvalue, const char *funcname);
void checkAlloc(void* ptr, const char *funcname);
void writeOutData(double* out_data, uint64_t out_num, double time, const double* restrict STATE);
void solverInit(double* STATE, void** cvode_mem_out, N_Vector* y0_out);
void solverRun(void* cvode_mem, N_Vector yout);
void solverShutdown(void* cvode_mem, N_Vector y0);

// globals - should be consts only


// CVODE Helper Functions ///////////////////////////////////////////////////////
// this function is pass to Cvode to solve the ODE, and is a wrapper around the OdeModelLoop function that
// computes the RHS, i.e. y' = f(t, y)
int odeWrapperF(double time, N_Vector y, N_Vector ydot, void* user_data) {
    // we use NV_DATA_S macro to accress the internal arrays
    OdeModelLoop(time, NV_DATA_S(y), NV_DATA_S(ydot));
    return 0;
}

// code to check the flags values return by various CVODE functions
void checkFlag(int flag, const char *funcname) {
    if (flag < 0) {
        fprintf(stderr, "\nSUNDIALS_ERROR: %s() failed with flag = %d\n\n", funcname, flag);
        exit(EXIT_FAILURE);
    }
}

void checkAlloc(void* ptr, const char *funcname) {
    if (ptr == NULL) {
        fprintf(stderr, "\nSUNDIALS_ERROR: %s() failed - returned NULL pointer\n\n", funcname);
        exit(EXIT_FAILURE);
    }
}


// save current state
void writeOutData(double* out_data, uint64_t out_num, double time, const double* restrict STATE) {
    // build the output array
    out_data[0] = time;
    for (uint64_t i = 1; i < out_num; ++i) {
        out_data[i] = STATE[i-1];
    }
    // write it to disk
    writeDbls(out_data, out_num);
}


int main(int argc, char** argv) {
    // parse command-line args ?
    // allows setting some dynamic cvode features
    // thse include, error tolerances, timestep min/max, stiff/non-stiff

    // main data structs passed around solver
    void* cvode_mem = NULL;
    N_Vector y0 = NULL;
    double STATE[OdeParamStateSize];     // stack alloc the buffer of doubles

    // main code to setup, run, and shutdown the simulation
    // initialise CVODE and Ode StdLib
    solverInit(STATE, &cvode_mem, &y0);
    // main solver loop
    solverRun(cvode_mem, y0);
    // shutdown solvers
    solverShutdown(cvode_mem, y0);
    return 0;
}


// initialise CVODE and Ode StdLib
void solverInit(double* STATE, void** cvode_mem_out, N_Vector* y0_out) {
    // Ode Stdlib init & setup file output
    const uint64_t out_num = OdeParamStateSize+1;
    double out_data[out_num];     // stack alloc the buffer of doubles
    init();
    startSim(&OdeParamOutput, out_num);
    // populate the initial vals and write to disk
    OdeModelInitials(OdeParamStartTime, STATE);
    writeOutData(out_data, out_num, OdeParamStartTime, STATE);


    // Setup CVODE
    int32_t flag;
    const int64_t N = (int64_t)OdeParamStateSize; // number of state params
    // setup y here - this func doesn't copy the data array!
    N_Vector y0 = N_VMake_Serial(N, STATE);
    // create Cvode solver
    void *cvode_mem = CVodeCreate(CV_BDF, CV_NEWTON);
    checkAlloc(cvode_mem, "CVodeCreate");

    // set user data?? - unused
    // init the CVode solver
    flag = CVodeInit(cvode_mem, &odeWrapperF, OdeParamStartTime, y0);
    checkFlag(flag, "CVodeInit");

    // setup error tolerances and timestep params
    static const double reltol = 1e-7;
    static const double abstol = 1e-9;
    flag = CVodeSStolerances(cvode_mem, reltol, abstol);
    checkFlag(flag, "CVodeSStolerances");
    flag = CVodeSetMinStep(cvode_mem, OdeParamTimestep);
    checkFlag(flag, "CVodeSetMinStep");
    // need to set max step - OdeParamMaxTimestep
    // todo - set end time
    // setup the linear sovler module for newton iteration - we choose CVDense
    flag = CVDense(cvode_mem, N);
    checkFlag(flag, "CVDense");

    // write thru the outputs
    *cvode_mem_out = cvode_mem;
    *y0_out = y0;
}

// shutdown simulation - free mem, etc.
void solverShutdown(void* cvode_mem, N_Vector y0) {
    N_VDestroy_Serial(y0);
    CVodeFree(&cvode_mem);
    endSim();
    shutdown();
}

void solverRun(void* cvode_mem, N_Vector yout) {
    const uint64_t out_num = OdeParamStateSize+1;
    double out_data[out_num]; // [OdeParamNumParams+1];

    static uint64_t cur_loop = 0;
    static double next_time;
    // todo -have OdeParamPeriod equal to the writeout timestep
    const double out_time_delta = OdeParamTimestep * OdeParamPeriod;
    double t_ret;
    int32_t flag;

    // main solver loop
    while(true) {
        cur_loop++;
        next_time = OdeParamStartTime + cur_loop * out_time_delta;

        // call CVode to solve ODE upto time=t_ret
        flag = CVode(cvode_mem, next_time, yout, &t_ret, CV_NORMAL);

        // check flag manually here
        if (flag < 0) {
            fprintf(stderr, "\nSUNDIALS_ERROR: CVode solving failed with flag = %d\n\n", flag);
            break;
        }

        // next_time == t_ret ?
        writeOutData(out_data, out_num, t_ret, NV_DATA_S(yout));
        if (t_ret >= OdeParamStopTime) break;
    }
}
