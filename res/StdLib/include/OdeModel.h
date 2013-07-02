/*
** ODE Model Header file
** This file presents a C-based interface into the object file that may be created by Ode
** The object file file is called OdeModel.o and can be found in the ode directory
** Using this interface and linking to OdeModel.o allows use of Ode with external solvers, such as CVODE and Chaste
*/
#ifndef ODE_MODEL_H
#define ODE_MODEL_H
#include <stdint.h>

// Simulation Time Parameters //////////////////////////////////////////////////////////////////////
// These are all read-only parameters that represent the simulation parameters specified when compiling the model
// They may be ignored if required within the external solver
extern const double OdeParamStartTime;
extern const double OdeParamStopTime;
// an adjusted stop time that is a rounded-up multiple of the output period
extern const double OdeParamAdjustedStopTime;

// the timestep to be used for a fixed solver, or the minimum timestep within an adaptive solver
extern const double OdeParamTimestep;

// Adative Solver Parameteres (i.e. for use with CVODE) ////////////////////////////////////////////
// the max timestep to be used for an adaptive solver, used to ensure we don't skip
// pass the stimulus current - set to half the stimulus interval
extern const double OdeParamMaxTimestep;
// maximum num of steps to be taken in solving from curTime->tOut
extern const uint64_t OdeParamMaxNumSteps;

// error toleraances for adaptive solvers
extern const double OdeParamRelativeError;
extern const double OdeParamAbsoluteError;

// model type - note, this is of type int
enum OdeParamModelTypes {
    Stiff = 0,
    NonStiff = 1
};

extern const enum OdeParamModelTypes OdeParamModelType;


// Simulation Output Parameters ////////////////////////////////////////////////////////////////////
// this incdiates the time period at which output should create, ideally should be an
// integer multiple of the timestep for fixed solvers
extern const double OdeParamPeriod;

// pre-calculated period interval in terms of multiples of the timestep
extern const uint64_t OdeParamPeriodInterval;

// the time at which to start output to data file
extern const double OdeParamStartOutputTime;

// this is actually statically alloc'd string literal, need to take the address to use it as a string
extern const char OdeParamOutput[];


// High-level Simulation/Model Parameters //////////////////////////////////////////////////////////
// Simulation Type - indicates how to strucutre the solver and interact with the model functions
enum OdeParamSimTypes {
    Ode = 0,
    Sde = 1,
    Rre = 2,
    Hybrid = 3,
    ProjSde = 4
};

extern const enum OdeParamSimTypes OdeParamSimType;

// This parameter indicates the number of STATE parameters contatined in the model
// It is to be used when allocating memory to hold the STATE and DELTA arrays
extern const uint64_t OdeParamStateSize;


// Simulation Functions ////////////////////////////////////////////////////////////////////////////
// Functions to be called by external solvers, these both takes pointers to arrays of data that must be set to the size contained in OdeParamNumParams
// (this can be done on stack as VLA in C99 - i.e. double STATE[OdeParamNumParams];)

// This function sets up the initial values for the problem
// i.e. y0 = y(t), where y0=STATE
extern void OdeModelInitials(const double time, double* const restrict state);

// This function calculates the delta value based on the current time and STATE values
// if SimType=SDE, also returns the weiner values, else pass a NULL pointer
// i.e. (y', w) = f(t, y), where y'=DELTA and y=STATE
extern void OdeModelRHS(const double time, const double* const restrict state,
                         double* const restrict delta, double* const restrict weiner);


#endif // ODE_MODEL_H
