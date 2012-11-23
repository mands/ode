#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <inttypes.h>
#include <string.h>
#include <math.h>

// CVODE Libraries
// ===============
#include "cvode/cvode.h"				/* Prototypes for CVODE fcts., consts. */
#include "cvode/cvode_dense.h"			/* Prototype for CVDense */
#include "nvector/nvector_serial.h"		/* Serial N_Vector types, fcts., macros */
// #include "sundials/sundials_dense.h"	/* Definitions DlsMat DENSE_ELEM */
// #include "sundials/sundials_types.h"	/* Definition of type realtype */

// Ode Model Interface
#include "OdeLibrary.h"
#include "OdeModel.h"

// prototypes
// inc OdeWrappers here
//int f(realtype t, N_Vector y, N_Vector ydot, void* f_data);
//int check_flag(void *flagvalue, char *funcname, int opt);
void modelSolver(void);
int cvodeOdeWrapperF(double time, N_Vector y, N_Vector ydot, void* user_data);

// globals
// any?


int main(int argc, char** argv) {
    puts("Hello World from CVODE");

    // parse command-line args
    // allows setting some dynamic cvode features
    // thse include, error tolerances, timestep min/max, stiff/non-stiff


    modelSolver();

    return 0;

}

// main code to setup, run, and shutdown the simulation
void modelSolver(void) {
    init();
    // setup file output
    uint64_t out_num = OdeParamNumParams+1;
    startSim(&OdeParamOutput, out_num);
    // statically alloc the buffer of doubles
    double out_data[out_num]; // [OdeParamNumParams+1];



    // initialise the model
    // ====================
    int flag;
    int64_t N = OdeParamNumParams; // number of state params
    // stack allocate and populate the initial vals
    double STATE[OdeParamNumParams];
    OdeModelInitials(OdeParamStartTime, STATE);

    // setup y here - this func actually allocates the data anyway
    N_Vector y0 = N_VMake_Serial(N, STATE);
    N_Vector yout = y0; // = NULL;

    // create Cvode solver
    void *cvode_mem = CVodeCreate(CV_BDF, CV_NEWTON);
    // set user data?? - unused
    // init the CVode solver
    CVodeInit(cvode_mem, &cvodeOdeWrapperF, OdeParamStartTime, y0);

    // setup error tolerances and timestep params
    // TODO
    double reltol = 1e-7;
    double abstol = 1e-9;
    flag = CVodeSStolerances(cvode_mem, reltol, abstol);

    // setup the linear sovler module for newton iteration - we choose CVDense
    CVDense(cvode_mem, OdeParamNumParams);


    // run the solution
    // ================
    // save initial state
    out_data[0] = OdeParamStartTime;
    for (uint64_t i = 1; i < out_num; ++i) {
        out_data[i] = STATE[i-1];
    }
    writeDbls(out_data, out_num);

    static uint64_t cur_period = 1;
    static uint64_t cur_loop = 0;
    static double next_time;
    double out_time_delta = OdeParamTimestep * OdeParamPeriod;
    double t_ret;

    while(true) {
        cur_loop++;
        next_time = OdeParamStartTime + cur_loop * out_time_delta;
        
        flag = CVode(cvode_mem, next_time, yout, &t_ret, CV_NORMAL);

        if (flag != CV_SUCCESS) break;

        // next_time == t_ret ?

        // save current state
        out_data[0] = t_ret;
        for (uint64_t i = 1; i < out_num; ++i) {
            out_data[i] = NV_DATA_S(yout)[i-1];
        }
        writeDbls(out_data, out_num);

        if (t_ret >= OdeParamEndTime) break;
    }



    // shutdown simulation - free mem, etc.
    // ===================
    N_VDestroy_Serial(y0);
    CVodeFree(&cvode_mem);

    endSim();
    shutdown();


}

// this function is pass to Cvode to solve the ODE, and is a wrapper around the OdeModelLoop function that
// computes the RHS, i.e. y' = f(t, y)
int cvodeOdeWrapperF(double time, N_Vector y, N_Vector ydot, void* user_data) {
    // we use NV_DATA_S macro to accress the internal arrays
    OdeModelLoop(time, NV_DATA_S(y), NV_DATA_S(ydot));
    return 0;
}

