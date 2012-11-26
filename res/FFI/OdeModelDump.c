// This dummy peice of code both acts as a test for the OdeModel object file

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <string.h>
#include <math.h>

#include "OdeLibrary.h"
#include "OdeModel.h"

void dumpParams(void);

int main(void) {
    dumpParams();
    return(0);
}

void dumpParams(void) {
    puts("Dumping Ode Model Params");
    puts("\nOde Model Timing");
    printf("OdeParamStartTime\t- %g\n", OdeParamStartTime);
    printf("OdeParamStopTime\t- %g\n", OdeParamStopTime);
    printf("OdeParamTimeStep\t- %g\n", OdeParamTimestep);

    puts("\nOde Adaptive Params");
    printf("OdeParamMaxTimestep\t- %g\n", OdeParamMaxTimestep);
    printf("OdeParamRelativeError\t- %g\n", OdeParamRelativeError);
    printf("OdeParamAbsoluteError\t- %g\n", OdeParamAbsoluteError);

    switch (OdeParamModelType) {
    case Stiff:
        printf("OdeParamModelType\t- %s\n", "Stiff");
        break;
    case NonStiff:
        printf("OdeParamModelType\t- %s\n", "NonStiff");
        break;
    }

    puts("\nOde Model Output");
    printf("OdeParamPeriod\t\t- %g\n", OdeParamPeriod);
    printf("OdeParamOutput\t\t- %s\n", OdeParamOutput);

    puts("\nOde Model Num Params");
    printf("OdeParamStateSize\t- %" PRIu64 "\n", OdeParamStateSize);
}
