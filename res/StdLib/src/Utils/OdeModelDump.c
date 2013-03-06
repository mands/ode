// This dummy peice of code both acts as a test for the OdeModel object file

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <string.h>
#include <math.h>

#include "OdeStdLib.h"
#include "OdeModel.h"

void dumpParams(void);

int main(void) {
    dumpParams();
    return(0);
}

void dumpParams(void) {
    puts("Dumping Ode Model Params");
    puts("\nOde Model High Level Params");
    printf("OdeParamStateSize\t\t- %" PRIu64 "\n", OdeParamStateSize);
    switch (OdeParamSimType) {
    case Ode:
        printf("OdeParamSimType\t\t\t- %s\n", "ODE");
        break;
    case Sde:
        printf("OdeParamSimType\t\t\t- %s\n", "SDE");
        break;
    case Rre:
        printf("OdeParamSimType\t\t\t- %s\n", "RRE");
        break;
    case Hybrid:
        printf("OdeParamSimType\t\t\t- %s\n", "Hybrid");
        break;
    }

    puts("\nOde Model Timing");
    printf("OdeParamStartTime (s)\t\t- %g\n", OdeParamStartTime);
    printf("OdeParamStopTime (s)\t\t- %g\n", OdeParamStopTime);
    printf("OdeParamAdjustedStopTime (s)\t- %g\n", OdeParamAdjustedStopTime);
    printf("OdeParamTimeStep (s)\t\t- %g\n", OdeParamTimestep);

    puts("\nOde Adaptive Params");
    printf("OdeParamMaxTimestep (s)\t\t- %g\n", OdeParamMaxTimestep);
    printf("OdeParamMaxNumSteps\t\t- %" PRIu64 "\n", OdeParamMaxNumSteps);
    printf("OdeParamRelativeError\t\t- %g\n", OdeParamRelativeError);
    printf("OdeParamAbsoluteError\t\t- %g\n", OdeParamAbsoluteError);

    switch (OdeParamModelType) {
    case Stiff:
        printf("OdeParamModelType\t\t- %s\n", "Stiff");
        break;
    case NonStiff:
        printf("OdeParamModelType\t\t- %s\n", "NonStiff");
        break;
    }

    puts("\nOde Model Output");
    printf("OdeParamPeriod (s)\t\t- %g\n", OdeParamPeriod);
    printf("OdeParamPeriodInterval\t\t- %" PRIu64 "\n", OdeParamPeriodInterval);
    printf("OdeParamStartOutputTime (s)\t- %g\n", OdeParamStartOutputTime);
    printf("OdeParamOutput\t\t\t- %s\n", OdeParamOutput);

// dummy code - ignore
//    puts("\nSizeof declarations");
//    printf("int\t\t- %" PRIu64 "\n", 8*sizeof(int));
//    printf("long\t\t- %" PRIu64 "\n", 8*sizeof(long));

}
