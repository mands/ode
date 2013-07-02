#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h> 
#include <string.h>
#include <math.h>

#include "OdeStdLib.h"
#include "OdeModel.h"


// Simulation Time Parameters //////////////////////////////////////////////////////////////////////
const double OdeParamStartTime = 0.0;
const double OdeParamStopTime = 1000.0;
const double OdeParamAdjustedStopTime = 1000.0;
const double OdeParamTimestep = 0.01;
// unused
/*const double OdeParamMaxTimestep = 0.01;*/
/*const uint64_t OdeParamMaxNumSteps = 1;*/
/*const double OdeParamRelativeError = 0;*/
/*const double OdeParamAbsoluteError = 0;*/
/*const enum OdeParamModelTypes OdeParamModelType = Stiff;*/


// Simulation Output Parameters ////////////////////////////////////////////////////////////////////
const double OdeParamPeriod = 1.0;
const uint64_t OdeParamPeriodInterval = (uint64_t) (OdeParamPeriod / OdeParamTimestep);
const double OdeParamStartOutputTime = OdeParamStartTime;
const char OdeParamOutput[] = "CTest1SDE.bin";

// High-level Simulation/Model Parameters //////////////////////////////////////////////////////////
const enum OdeParamSimTypes OdeParamSimType = Sde;
const uint64_t OdeParamStateSize = 2;

// Simulation Functions ////////////////////////////////////////////////////////////////////////////
// This function sets up the initial values for the problem
// i.e. y0 = y(t), where y0=STATE
void OdeModelInitials(const double time, double* const restrict state) {




}

// This function calculates the delta value based on the current time and STATE values
// if SimType=SDE, also returns the weiner values, else pass a NULL pointer
// i.e. (y', w) = f(t, y), where y'=DELTA and y=STATE
void OdeModelRHS(const double time, const double* const restrict state,
                         double* const restrict delta, double* const restrict weiner) {
                         
                         

}

