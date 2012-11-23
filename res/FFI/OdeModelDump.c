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
    puts("Dumping Ode Model Params\n");
    puts("Ode Model Timing");
    printf("OdeParamStartTime\t- %g\n", OdeParamStartTime);
    printf("OdeParamEndTime\t\t- %g\n", OdeParamEndTime);
    printf("OdeParamTimeStep\t- %g\n\n", OdeParamTimestep);

    puts("Ode Model Output");
    printf("OdeParamPeriod\t\t- %" PRIu64 "\n", OdeParamPeriod);
    printf("OdeParamOutput\t\t- %s\n\n", &OdeParamOutput);

    puts("Ode Model Num Params");
    printf("OdeParamNumParams\t- %" PRIu64 "\n", OdeParamNumParams);

}
