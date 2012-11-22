/*
** ODE Model Header file
** This file presents a C-based interface into the object file that may be created by Ode
** The object file file is called OdeModel.o and can be found in the ode directory
** Using this interface and linking to OdeModel.o allows use of Ode with external solvers, such as CVODE and Chaste
*/
#ifndef ODE_MODEL_H
#define ODE_MODEL_H
#include <stdint.h>

/* Simulation Parameters ******************************************************
** These are all read-only parameters that represent the simulation parameters specified when compiling the model
** They may be ignored if required within the external solver
*/
// These parameters indicate the basic timing informatio for the simulation
const double OdeParamStartTime;
const double OdeParamEndTime;
const double OdeParamTimestep;

// These parameters indicate both the name of the output file, and the expected period at which to output the STATE data
const uint64_t OdeParamPeriod;
const char OdeParamOutput; // this is actually statically alloc'd string literal, need to take the address to use it as a string

// This parameter indicates the number of STATE parameters contatined in the model
// It is to be used when allocating memory to hold the STATE and DELTA arrays
const uint64_t OdeParamNumParams;


/* Simulation Functions *******************************************************
** Functions to be called by external solvers, these both takes pointers to arrays of data that must be set to the size contained in OdeParamNumParams
** (this can be done on stack as VLA in C99 - i.e. double STATE[OdeParamNumParams];)
*/

// This function sets up the initial values for the problem
// i.e. y0 = y(t), where y0=STATE
void OdeModelInitials(double time, double* STATE);

// This function calculates the delta, based on the current time and STATE values
// i.e. y' = f(t, y), where y'=DELTA and y=STATE
void OdeModelLoop(double time, double* const restrict STATE, double* const restrict DELTA);

#endif // ODE_MODEL_H

