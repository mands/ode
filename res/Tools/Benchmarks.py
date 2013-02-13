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
    anaylse program that takes a results files of binary numbers, and plots it using scipy
    NOTE - this should only work on amd64 systems as uses hardcoded element sizes and (little-)endineness
"""

import os
import logging
import argparse
import sh
import io
from functools import wraps
from itertools import chain
import inspect
from time import gmtime, strftime


# Globals
ODE_DIR = os.path.abspath("/home/mandeep/DPhil/Projects/ode/ode3")
CUR_DIR = os.getcwd()


# TOTO - get working initializer decorator
def initializer1(fun):
    a = inspect.getfullargspec(fun)
    print(a)

# def initializer(fun):
#     """Setup initializer lists for init args"""
#     names, varargs, keywords, defaults = inspect.getargspec(fun)
#     @wraps(fun)
#     def wrapper(self, *args, **kargs):
#         for name, arg in chain(zip(names[1:], args), kargs.items()):
#             setattr(self, name, arg)
#         fun(self, *args, **kargs)
#     return wrapper


class Simulation:
    def __init__(self, sim_name, *, do_build=True, do_benchmark=True, do_analyse=True, exe_sim=None, res_name=None, run_root = False,
                 num_sims=5):
        self.sim_name = sim_name
        self.do_build = do_build
        self.do_benchmark = do_benchmark
        self.do_analyse = do_analyse
        self.run_root = run_root
        self.exe_sim = exe_sim if exe_sim else ("./" + sim_name + ".exe")
        self.res_name = res_name if res_name else ("./" + sim_name + ".bin")
        self.num_sims = num_sims

    def execute(self):
        logging.debug("Running performance benchmarks for {}, with results output to {}".format(self.sim_name, self.res_name))
        print("Results from {}".format(self.sim_name), file=res_file)
        if self.do_build: self.build()
        if self.do_benchmark: self.benchmark()
        if self.do_analyse: self.analyse()
        print("Finished {}\n".format(self.sim_name), file=res_file)

    def build(self):
        pass

    def parse_time_res(self, time_buf):
        time_lines = time_buf.splitlines()
        user_time = time_lines[1].split(sep=':')[1]
        sys_time = time_lines[2].split(sep=':')[1]
        return float(user_time) + float(sys_time)

    def benchmark(self):
        if self.run_root:
            (run_cmd, run_opts) = ("sudo", ("chrt", "-f", "99", "/usr/bin/time", "-v", self.exe_sim))
        else:
            (run_cmd, run_opts) = ("/usr/bin/time", ("-v", self.exe_sim))

        total_time = 0.0
        for i in range(self.num_sims):
            print(run_cmd, run_opts)

            time_err_buf = io.StringIO()

            time_out_buf = sh.Command(run_cmd)(run_opts, _err=time_err_buf)

            log.debug("Simulation output - \n{}".format(time_out_buf))
            time_buf = time_err_buf.getvalue()
            time_err_buf.close()
            # accum the time
            total_time += self.parse_time_res(time_buf)

        avg_time = total_time / self.num_sims
        print("Average time taken : {:.3g} for {} simulations".format(avg_time, self.num_sims), file=res_file)

    def analyse(self):
        pass



class OdeSimulation(Simulation):
    def __init__(self, sim_name, mod_name, sim_params, **kwargs):
        super().__init__(sim_name, **kwargs)
        self.mod_name = mod_name
        self.sim_params = sim_params

    def gen_sim_script(self):
        script = ""
        for (k, v) in self.sim_params.items():
            if v is True:
                script += ":{}\n".format(k)
            elif v is False:
                pass
            elif isinstance(v, str):
                script += ":{} \"{}\"\n".format(k, v)
            else:
                script += ":{} {}\n".format(k, v)

        script += "import {}.{}\n".format(self.sim_name, self.mod_name)
        script += ":simulate {}\n".format(self.mod_name)
        script += ":quit\n"
        logging.debug("Sim Script - \n{}".format(script))
        return script

    def build(self):
        script_name = self.sim_name + ".od3s"
        src_name = self.sim_name + ".od3"
        exe_name = self.sim_name + ".exe"

        # need gen the config script
        script_path = os.path.join(CUR_DIR, script_name)
        # build the exe
        os.chdir(ODE_DIR)
        # TODO - update to stdin, generate the od3s file dynamically

        # build the script
        script = self.gen_sim_script()

        sh.Command("./ode3")(_in=script)
        os.chdir(CUR_DIR)
        # move exe to cur dir
        sh.mv(os.path.join(ODE_DIR, exe_name), CUR_DIR)


def get_ode_params(**kwargs):
    params = {'disableUnits': True,  # disable unit checking
              'addRepo': os.path.relpath(CUR_DIR, ODE_DIR),  # setup the repo
              # set sim params
              'startTime': 0,
              'stopTime': 60,
              'timestep': 0.1,
              'period': 1,
              'output': "./output.bin",
              'exeOutput': "./Sim.exe",
              # sim backend params
              'odeSolver': 'euler',  # odeSolver - euler, rk4, adaptive
              'sdeSolver': 'projem',  # sdeSolver - eulerm, projem
              'linker': 'dynamic',  # linker - dynamic, static
              'disableExecute': True,
              'disableOptimise': False,
              'disableShortCircuit': False,
              'disablePowerExpan': False,
              'mathModel': 'fast',  # math model - strict, fast
              'vecMath' : False,
              'mathLib': 'gnu',  # mathLib - gnu, amd, intel
              'backend': 'aotcompiler'  # backend to use - interpreter, jitcompiler, aotcompiler, obj
              }

    # override params with func kwargs and return merge
    return dict(list(params.items()) + list(kwargs.items()))

# class GenericSimulation(Simulation):
#     """
#     A generic simulation class that assumes exe already build and so runs the simulation and analyses results
#     """
#     def __init__(self, name, run_sim=None):
#         super().__init__(name, run_sim)

def init():
    logging.basicConfig(level=logging.DEBUG)

    # arg parsing
    parser = argparse.ArgumentParser(
        description='Ode Benchmarking Util, compiles a set of simulations, executes them and performs a set of '
                    'benchmarking tests and analysis',  # main description for help
        epilog='Tested on Linux only'  # displayed after help
    )

    parser.add_argument("-v", "--verbose", action="store_true", help="Verbose mode")
    parser.add_argument("-o", "--ode-dir", type=str, help="Location of Ode compiler")

    # parser.add_argument("-c", "--compare", action="store_true", help="Compare results to reference file (ref.bin)")
    # parser.add_argument("-n", "--num-iterations", type=int, default=5,  help="Num iterations when running benchmark")
    # parser.add_argument("-f", "--compare-file", type=str, default="ref.bin", help="Reference file for comparison")
    # parser.add_argument("-r", "--run-root", action="store_true", default=False, help="Run as high-priority process under root for more accurate results")

    args = parser.parse_args()

    ## setup globals
    if args.ode_dir:
        ODE_DIR = args.ode_dir

    # make all non-Ode exes
    logging.debug("Running make all")
    sh.make('all')


def runSims(sims, res_file_name = "results.txt"):

    global res_file
    res_file = open(res_file_name, 'w')
    print("Running set of simulation at {}\n".format(strftime("%H:%M:%S on %d/%m/%Y", gmtime())), file=res_file)

    for sim in sims:
        sim.execute()

    # run a sim (<filename.exe>)
    # sh.Command('HodHux_C.exe')
    # analyse results (assume output is <filename>.bin)
    # run perf benchmark multiple times

    res_file.close()
    logging.debug("Done")
