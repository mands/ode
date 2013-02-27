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
    Benchmarks for the HH52 model
"""

import os
import logging
import argparse
import sh
import io
from Benchmarks import *
import Build

if __name__ == '__main__':
    init()

    # ode setup
    ode_params = Build.get_ode_params("HodHuxOde", startTime=0.0, stopTime=600.0, period=0.06, timestep=0.01, disablePowerExpan=True)

    # src locations
    c_model_name = 'HodHuxC'

    # run set of simulations
    sims = [OdeSimulation("HodHuxOde", "Model", ode_params),
            Simulation(c_model_name, src_name=c_model_name+'.c', res_ref=c_model_name+'.bin'),
            ]

    runSims(sims)
