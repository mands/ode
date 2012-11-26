/*
** ODE Model Header file
** This file presents a C-based interface into the object file that may be created by Ode
** The object file file is called OdeModel.o and can be found in the ode directory
** Using this interface and linking to OdeModel.o allows use of Ode with external solvers, such as CVODE and Chaste
*/
#ifndef ODE_MODEL_H
#define ODE_MODEL_H
#include <stdint.h>

/* Simulation Time Parameters *******************************************************/
// These are all read-only parameters that represent the simulation parameters specified when compiling the model
// They may be ignored if required within the external solver
extern const double OdeParamStartTime;
extern const double OdeParamStopTime;
// the timestep to be used for a fixed solver, or the minimum timestep within an adaptive solver
extern const double OdeParamTimestep;

/* Adative Solver Parameteres (i.e. for use with CVODE)************************/
// the max timestep to be used for an adaptive solver, used to ensure we don't skip
// pass the stimulus current - set to half the stimulus interval
extern const double OdeParamMaxTimestep;

// error toleraances for adaptive solvers
extern const double OdeParamRelativeError;
extern const double OdeParamAbsoluteError;

// model type - Note, this is of type int
enum OdeParamModelTypes {
    Stiff = 0,
    NonStiff = 1
};

extern const enum OdeParamModelTypes OdeParamModelType;

/* Simulation Output Parameters ****************************************************** */
// this incdiates the time period at which output should create, ideally should be an
// integer multiple of the timestep for fixed solvers
extern const double OdeParamPeriod;
// this is actually statically alloc'd string literal, need to take the address to use it as a string
extern const char OdeParamOutput[];

// This parameter indicates the number of STATE parameters contatined in the model
// It is to be used when allocating memory to hold the STATE and DELTA arrays
extern const uint64_t OdeParamStateSize;


/* Simulation Functions ********************************************************/
// Functions to be called by external solvers, these both takes pointers to arrays of data that must be set to the size contained in OdeParamNumParams
// (this can be done on stack as VLA in C99 - i.e. double STATE[OdeParamNumParams];)

// This function sets up the initial values for the problem
// i.e. y0 = y(t), where y0=STATE
extern void OdeModelInitials(double time, double* STATE);

// This function calculates the delta, based on the current time and STATE values
// i.e. y' = f(t, y), where y'=DELTA and y=STATE
extern void OdeModelLoop(double time, double* const restrict STATE, double* const restrict DELTA);

#endif // ODE_MODEL_H
