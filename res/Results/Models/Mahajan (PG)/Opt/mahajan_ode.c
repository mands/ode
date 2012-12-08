// "Optimised" varient of PG's Mahajan model, convert to C11

// Standard use libraries
// ======================
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

// CVODE Libraries
// ===============
#include "cvode/cvode.h"				/* Prototypes for CVODE fcts., consts. */
#include "nvector/nvector_serial.h"		/* Serial N_Vector types, fcts., macros */
#include "cvode/cvode_dense.h"			/* Prototype for CVDense */
#include "sundials/sundials_dense.h"	/* Definitions DlsMat DENSE_ELEM */
#include "sundials/sundials_types.h"	/* Definition of type realtype */

// Ode FFI
#include "OdeModel.h"
#include "OdeStdLib.h"

// CVODE Set-up
// ============
#define stim_length	RCONST(1000000.0)	/* Total simulation time */
#define bcl		RCONST(1000.0)			/* BCL */
//#define bcl_number	2					/* Number of APs to be recorded */

//#define offset		stim_length-floor(stim_length/bcl)*bcl

#define Ith(v,i)	NV_Ith_S(v,i-1)			/* Ith numbers components 1...NEQ */
//#define IJth(A,i,j)	DENSE_ELEM(A,i-1,j-1)	/* IJth numbers rows,cols 1...NEQ */
#define NEQ			33						/* Number of equations (plus six ion channel currents) */
#define T0			RCONST(0.0)				/* Initial time */
#define out_step	RCONST(0.1)				/* Output time step */
#define RTOL		RCONST(1e-9)			/* Scalar relative tolerance (original = 1e-7) */
#define ATOL		RCONST(1e-11)			/* Vector absolute tolerance components (original = 1e-9) */

#define T1		stim_length-(2*bcl)		/* First output time */
#define NOUT	(2*bcl)/out_step		/* Number of output times */


/* Private helper function */
static int f(realtype t, N_Vector y, N_Vector ydot, void* f_data);
static int check_flag(void *flagvalue, char *funcname, int opt);

int main(void)
{

    realtype reltol, t, tout;
    N_Vector y, abstol;
    void *cvode_mem;
    int flag, iout;

    // Ode model check
    if (OdeParamSimType != Ode) {
        puts("Can only simulate an ODE model");
        return(1);
    }
    // Ode setup
    OdeInit();
    OdeStartSim("output_ode.bin", NEQ);


    /* Allocate y, abstol vectors, and create serial vectors of length NEQ for I.C. and abstol */
    y = abstol = NULL;		cvode_mem = NULL;

    y = N_VNew_Serial(NEQ);
    if (check_flag((void *)y, "N_VNew_Serial", 0))
    {
        return(1);
    }
    abstol = N_VNew_Serial(NEQ);
    if (check_flag((void *)abstol, "N_VNew_Serial", 0))
    {
        return(1);
    }

    /* Initialise y */
    OdeModelInitials(T0, NV_DATA_S(y));

    /* Set the error tolerances */
    reltol = RTOL;
    for (int i=1; i<=NEQ; i++)
    {
        Ith(abstol,i) = ATOL;
    }

    /* Call CVodeCreate to create the solver memory and specify the Backward Differentiation Formula and the use of a Newton iteration */
    cvode_mem = CVodeCreate(CV_BDF, CV_NEWTON);
    if (check_flag((void *)cvode_mem, "CVodeCreate", 0))
    {
        return(1);
    }

    /* Call CVodeInit to initialize the integrator memory and specify the user's right hand side function in y'=f(t,y), the inital time T0, and the initial dependent variable vector y. */
    flag = CVodeInit(cvode_mem, f, T0, y);
    if (check_flag(&flag, "CVodeInit", 1))
    {
        return(1);
    }

    /* Call CVodeSVtolerances to specify the scalar relative tolerance and vector absolute tolerances */
    flag = CVodeSVtolerances(cvode_mem, reltol, abstol);
    if (check_flag(&flag, "CVodeSVtolerances", 1))
    {
        return(1);
    }

    /* Alter the number of maximum steps the solver can take before it reaches tout */
    flag = CVodeSetMaxNumSteps(cvode_mem, 10000000000);

    /* Set the maximum/minimum step size the solver may take, such that the stimulus current will not be missed, and the initial step size */
    // set to 1/2 the stimulus period - 1.5
    //	flag = CVodeSetMaxStep(cvode_mem, 3.0);
    flag = CVodeSetMaxStep(cvode_mem, 1.5);
    //flag = CVodeSetMinStep(cvode_mem, 0.0001);
    //flag = CVodeSetInitStep(cvode_mem, 0.001);

    /* Set the maximum number of error test fails that may be taken (default = 7) */
    flag = CVodeSetMaxErrTestFails(cvode_mem, 25);

    /* Set the number of error messages that t = t+h will be issued (default = 10, negative means no error messages will be given) */
    flag = CVodeSetMaxHnilWarns(cvode_mem, -1);

    /* Set the maximum number of nonlinear solver convergence failures permitted during one step (default = 10) */
    flag = CVodeSetMaxConvFails(cvode_mem, 250);

    /* Call CVDense to specify the CVDENSE dense linear solver */
    flag = CVDense(cvode_mem, NEQ);
    if (check_flag(&flag, "CVDense", 1))
    {
        return(1);
    }

    //flag = CVodeMalloc(cvode_mem, f, T0, y0, CV_SV, reltol, abstol);

    iout = 0;
    tout = T1;
    while(1)
    {
        flag = CVode(cvode_mem, tout, y, &t, CV_NORMAL);

        OdeWriteState(t, NV_DATA_S(y));

        if (check_flag(&flag, "CVode", 1))
        {
            break;
        }
        if (flag == CV_SUCCESS)
        {
            iout++;
            tout += out_step;
        }

        if (iout == NOUT)
        {
            break;
        }
    }

    if (flag == CV_SUCCESS)
    {
        puts("Simulation complete!");
    }
    else if (flag != CV_SUCCESS)
    {
        puts("Simulation failed!");
    }

    /* Free y and abstol vectors */
    N_VDestroy_Serial(y);
    N_VDestroy_Serial(abstol);

    /* Free integrator memory */
    CVodeFree(&cvode_mem);

    // Ode Shutdown
    OdeStopSim();
    OdeShutdown();

    return(0);
}

static int f(realtype t, N_Vector y, N_Vector ydot, void *f_data)
{
    OdeModelRHS(t, NV_DATA_S(y), NV_DATA_S(ydot), NULL);
    return(0);
}

static int check_flag(void *flagvalue, char *funcname, int opt)
{
  int *errflag;

  /* Check if SUNDIALS function returned NULL pointer - no memory allocated */
  if (opt == 0 && flagvalue == NULL) {
    fprintf(stderr, "\nSUNDIALS_ERROR: %s() failed - returned NULL pointer\n\n",
        funcname);
    return(1); }

  /* Check if flag < 0 */
  else if (opt == 1) {
    errflag = (int *) flagvalue;
    if (*errflag < 0) {
      fprintf(stderr, "\nSUNDIALS_ERROR: %s() failed with flag = %d\n\n",
          funcname, *errflag);
      return(1); }}

  /* Check if function returned NULL pointer - no memory allocated */
  else if (opt == 2 && flagvalue == NULL) {
    fprintf(stderr, "\nMEMORY_ERROR: %s() failed - returned NULL pointer\n\n",
        funcname);
    return(1); }

  return(0);
}
