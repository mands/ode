#!/usr/bin/env python3
#
#  main.py
#  
#  Author:
#       Mandeep Gill <mangil@comlab.ox.ac.uk>
# 
#  Copyright (c) 2011 Mandeep Gill
# 
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
# 
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
# 
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""
    Benchmarks for the HodHux model
"""

import os
import logging
import argparse
import sh
import io
from Benchmarks import *

if __name__ == '__main__':
    init()
    # HodHux model
    hod_hux_params = get_ode_params(timestep=0.00001, output="./HodHuxOde.bin", exeOutput="./HodHuxOde.exe")
    hod_hux1_params = get_ode_params(timestep=0.001, output="./HodHuxOde1.bin", exeOutput="./HodHuxOde1.exe")

    # run set of simulations
    sims = [OdeSimulation("HodHuxOde", "HodHux", hod_hux_params, run_root=True, num_sims=1, res_ref="./HodHuxRef.bin"),
            #OdeSimulation("HodHuxOde1", "HodHux", hod_hux1_params, src_name="./HodHuxOde.od3", num_sims=2, res_ref="./HodHuxRef.bin"),
            Simulation("HodHuxC", src_name="./HodHuxC.c", num_sims=1, run_root=True, res_ref="./HodHuxRef.bin")]

    runSims(sims)
