#!/bin/bash

## Setup ######################################################################################
# Default vars
RES_DIR=`pwd`
SIM=
ODE_DIR=$HOME/ode/ode3/
ODE_EXE=./dist/build/ode3/ode3
cd $ODE_DIR

# common setup
INIT="
:disableUnits
:addRepo ../res/Results/CodeGen
import HodHux.HodHux
:startTime 0
:endTime 60
:timestep 0.00001
:period 100000
:solver \"euler\"
:disableExecute
:backend \"aotcompiler\"
"

FINI="
:simulate HodHux
:quit
"

function call_ode {
  $ODE_EXE <<< "$INIT$1$FINI"
  cp Sim.exe $RES_DIR/$2
}


## Simulations ################################################################################
# UNOPT
OPT="
:output \"./OutputOde_Unopt.bin\"
:disableOptimise
:mathModel \"strict\"
:disableShortCircuit
:disablePowerExpan
"
#call_ode "$OPT" "Sim_Unopt.exe"

# STRICT MATH (NO LTO) - MANUAL
OPT="
:output \"./OutputOde_OptStrict.bin\"
:mathModel \"strict\"
:disableShortCircuit
:disablePowerExpan
"
#call_ode "$OPT" "Sim_OptStrict.exe"

# STRICT MATH LTO
OPT="
:output \"./OutputOde_OptStrictLTO.bin\"
:mathModel \"strict\"
:disableShortCircuit
:disablePowerExpan
"
#call_ode "$OPT" "Sim_OptStrictLTO.exe"

# FAST MATH
OPT="
:output \"./OutputOde_FastMath.bin\"
:mathModel \"fast\"
:disableShortCircuit
:disablePowerExpan
"
#call_ode "$OPT" "Sim_FastMath.exe"

# SHORT CIRCUITING
OPT="
:output \"./OutputOde_ShortCircuit.bin\"
:mathModel \"fast\"
:disablePowerExpan
"
#call_ode "$OPT" "Sim_ShortCircuit.exe"

# BASIC OPTS (POW EXPANSION)
OPT="
:output \"./OutputOde_PowerExpan.bin\"
:mathModel \"fast\"
"
call_ode "$OPT" "Sim_PowerExpan.exe"

# STATIC LINKING
OPT="
:output \"./OutputOde_Static.bin\"
:mathModel \"fast\"
:linker \"static\"
"
#call_ode "$OPT" "Sim_Static.exe"


echo "done"
