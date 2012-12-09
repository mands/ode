// a basic SSA solver, includes hardcoded models and utilises Ode Stdlib
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#include "OdeStdLib.h"

int main(void) {
    OdeInit();
    OdeShutdown();
}
