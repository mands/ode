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
int odeWrapperF(double time, N_Vector const restrict y, N_Vector const restrict yDot, void* userData);
void checkFlag(const int flag, const char* const restrict funcname);
void checkAlloc(const void* const ptr, const char* const restrict funcname);
void solverInit(double* const restrict state, void** const restrict cvodeMemOut,
                N_Vector* const restrict y0Out);
void solverRun(void* const restrict cvodeMem, N_Vector const restrict yOut);
void solverShutdown(void* cvodeMem, N_Vector const restrict y0);
void modelSolver(void);
// globals - should be consts only


// CVODE Helper Functions ///////////////////////////////////////////////////////
// this function is pass to Cvode to solve the ODE, and is a wrapper around the OdeModelLoop function that
// computes the RHS, i.e. y' = f(t, y)
int odeWrapperF(double time, N_Vector const restrict y, N_Vector const restrict yDot, void* userData){
    // we use NV_DATA_S macro to accress the internal arrays
    OdeModelRHS(time, NV_DATA_S(y), NV_DATA_S(yDot));
    return 0;
}

// code to check the flags values return by various CVODE functions
void checkFlag(const int flag, const char* const restrict funcname) {
    if (flag < 0) {
        fprintf(stderr, "\nSUNDIALS_ERROR: %s() failed with flag = %d\n\n", funcname, flag);
        exit(EXIT_FAILURE);
    }
}

void checkAlloc(const void* const ptr, const char* const restrict funcname) {
    if (ptr == NULL) {
        fprintf(stderr, "\nSUNDIALS_ERROR: %s() failed - returned NULL pointer\n\n", funcname);
        exit(EXIT_FAILURE);
    }
}

//// Main function
//int main(void) {
//    return 0;
//}

// main entry point
void modelSolver(void) {
    // TODO - parse command-line args ?
    // main data structs passed around solver
    void* cvodeMem;
    N_Vector y0;
    double state[OdeParamStateSize];     // stack alloc the buffer of doubles

    // main code to setup, run, and shutdown the simulation
    // initialise CVODE and Ode StdLib
    solverInit(state, &cvodeMem, &y0);
    // main solver loop
    solverRun(cvodeMem, y0);
    // shutdown solvers
    solverShutdown(cvodeMem, y0);
}


// initialise CVODE and Ode StdLib
void solverInit(double* const restrict state, void** const restrict cvodeMemOut,
                N_Vector* const restrict y0Out) {
    // Ode Stdlib init & setup file output
    OdeInit();
    OdeStartSim(OdeParamOutput, OdeParamStateSize);
    // populate the initial vals and write to disk
    OdeModelInitials(OdeParamStartTime, state);
    OdeWriteState(OdeParamStartTime, state);

    // Setup CVODE
    int32_t flag;
    const int64_t N = (int64_t)OdeParamStateSize; // number of state params
    // setup y here - this func doesn't copy the data array!
    N_Vector y0 = N_VMake_Serial(N, state);
    // create Cvode solver
    void *cvodeMem;
    switch (OdeParamModelType) {
    case Stiff:
        cvodeMem = CVodeCreate(CV_BDF, CV_NEWTON);
        break;
    case NonStiff:
        cvodeMem = CVodeCreate(CV_ADAMS, CV_FUNCTIONAL);
        break;
    }
    checkAlloc(cvodeMem, "CVodeCreate");

    // set user data?? - unused
    // init the CVode solver
    flag = CVodeInit(cvodeMem, &odeWrapperF, OdeParamStartTime, y0);
    checkFlag(flag, "CVodeInit");

    // setup (scalar) error tolerances
    flag = CVodeSStolerances(cvodeMem, OdeParamRelativeError, OdeParamAbsoluteError);
    checkFlag(flag, "CVodeSStolerances");

    // optional inputs (see CVODE user guide) - inc timestep params
    flag = CVodeSetMinStep(cvodeMem, OdeParamTimestep);
    //checkFlag(flag, "CVodeSetMinStep");
    flag = CVodeSetMaxStep(cvodeMem, OdeParamMaxTimestep);
    checkFlag(flag, "CVodeSetMaxStep");
    flag = CVodeSetStopTime(cvodeMem, OdeParamAdjustedStopTime);
    checkFlag(flag, "CVodeSetStopTime");

    // Additional params (taken from PG Mahajan CVODE model)
    /* Alter the number of maximum steps the solver can take before it reaches tout */
    flag = CVodeSetMaxNumSteps(cvodeMem, OdeParamMaxNumSteps);
    checkFlag(flag, "CVodeSetMaxNumSteps");

    /* Set the maximum number of error test fails that may be taken (default = 7) */
//    flag = CVodeSetMaxErrTestFails(cvodeMem, 25);
    /* Set the number of error messages that t = t+h will be issued (default = 10, negative means no error messages will be given) */
//    flag = CVodeSetMaxHnilWarns(cvodeMem, -1);
    /* Set the maximum number of nonlinear solver convergence failures permitted during one step (default = 10) */
//    flag = CVodeSetMaxConvFails(cvodeMem, 250);

    // if newtonian iteration, attach linear solver
    // we choose CVDense with default dense Jacobian approx func, could also use CVDiag or CVBand
    if (OdeParamModelType == Stiff) {
        flag = CVDense(cvodeMem, N);
        checkFlag(flag, "CVDense");
    }

    // write thru the outputs
    *cvodeMemOut = cvodeMem;
    *y0Out = y0;
}

void solverRun(void* const restrict cvodeMem, N_Vector const restrict yOut) {
    uint64_t curLoop = 0;
    double tNext;
    double tRet;
    int32_t flag;

    // main solver loop
    while (true) {
        ++curLoop;
        tNext = OdeParamStartTime + curLoop * OdeParamPeriod;
        // call CVode to solve ODE upto time=t_ret
        flag = CVode(cvodeMem, tNext, yOut, &tRet, CV_NORMAL);

        // check ret flag manually here
        if (flag == CV_TSTOP_RETURN) break;
        if (flag < 0) {
            fprintf(stderr, "\nSUNDIALS_ERROR: CVode solving failed with flag = %d\n\n", flag);
            break;
        }
        // save state to disk
        OdeWriteState(tRet, NV_DATA_S(yOut));
    }
    printf("Simulation stop time - %g, OdeParamAdjustedStopTime - %g\n", tRet, OdeParamAdjustedStopTime);
}

// shutdown simulation - free mem, etc.
void solverShutdown(void* cvodeMem, N_Vector const restrict y0) {
    N_VDestroy_Serial(y0);
    CVodeFree(&cvodeMem);
    OdeStopSim();
    OdeShutdown();
}
