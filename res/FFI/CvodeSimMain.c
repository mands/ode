#include "OdeModel.h"

void modelSolver(void);

int main(void)
{
    if (OdeParamSimType != Ode) {
        puts("Can only simulate an ODE model");
        return(1);
    }

    modelSolver();
    return 0;
}
