#!/bin/bash

## Setup ######################################################################################
# Default vars
RES_DIR=`pwd`
SIM=
ODE_DIR=$HOME/ode/ode3/
ODE_EXE=./ode3
cd $ODE_DIR

# common setup
INIT="
:disableUnits
:addRepo ../res/Results/CodeGen
import HodHux.HodHux
:startTime 0
:stopTime 60
:timestep 0.00001
:period 1
:disableExecute
:backend \"aotcompiler\"
:mathModel \"fast\"
:mathLib \"gnu\"
:linker \"dynamic\"
:disablePowerExpan
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

# BASIC ODEs

ODES_SCRIPT=$(<$RES_DIR/odes.od3s)
EULER="
:solver \"euler\"
"
RK4="
:solver \"rk4\"
"

$ODE_EXE <<< "$EULER$ODES_SCRIPT"
mv output_Ode1.bin $RES_DIR/output_Ode1_Euler.bin
mv output_Ode2.bin $RES_DIR/output_Ode2_Euler.bin
mv output_PopGrowth.bin $RES_DIR/output_PopGrowth_Euler.bin
mv output_PredPrey.bin $RES_DIR/output_PredPrey_Euler.bin

$ODE_EXE <<< "$RK4$ODES_SCRIPT"
mv output_Ode1.bin $RES_DIR/output_Ode1_RK4.bin
mv output_Ode2.bin $RES_DIR/output_Ode2_RK4.bin
mv output_PopGrowth.bin $RES_DIR/output_PopGrowth_RK4.bin
mv output_PredPrey.bin $RES_DIR/output_PredPrey_RK4.bin

# EULER SOLVER
OPT="
:output \"./Output_HodHux_Euler.bin\"
:solver \"euler\"
"
call_ode "$OPT" "Sim_HodHux_Euler.exe"

# RK4 SOLVER
OPT="
:output \"./Output_HodHux_RK4.bin\"
:solver \"rk4\"
"
call_ode "$OPT" "Sim_HodHux_RK4.exe"

echo "done"
