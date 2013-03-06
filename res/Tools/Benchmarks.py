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
import platform
import sh
import io
import inspect
import re
from time import gmtime, strftime
from Compare import compare_files
import Build


# Globals
ODE_DIR = os.path.abspath("/home/mandeep/DPhil/Projects/ode/ode3")
CUR_DIR = os.path.abspath(os.getcwd())


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
    def __init__(self, sim_name, *, do_build=True, do_benchmark=True, do_analyse=True, exe_cmd=None, exe_name=None,
                 is_exe=True, res_name=None, res_ref=None, src_name=None, num_sims=5):
        self.sim_name = sim_name
        self.do_build = do_build
        self.do_benchmark = do_benchmark
        self.do_analyse = do_analyse
        self.exe_cmd = exe_cmd if exe_cmd else ("./" + sim_name + ".exe", )

        # check the executable name, if exists, for analysis
        if exe_name:
            self.exe_name = exe_name
        elif is_exe:
            self.exe_name = ("./" + sim_name + ".exe")
        else:
            self.exe_name = None

        self.res_name = res_name if res_name else ("./" + sim_name + ".bin")
        self.res_ref = res_ref
        self.src_name = src_name
        self.num_sims = num_sims
        self.startup_offset = 0.0

    def execute(self):
        logging.debug("Running performance benchmarks for {}, with results output to {}".format(self.sim_name, self.res_name))

        print("**********", file=res_file)
        print("Model Name : {}".format(self.sim_name), file=res_file)
        if self.do_build: self.build()
        if self.do_benchmark: self.benchmark()
        if self.do_analyse: self.analyse()
        print("**********\n", file=res_file)

    def build(self):
        pass

    def parse_time_res(self, time_buf):
        user_time = re.compile('User time \(seconds\): ([\d.]+)$', re.MULTILINE).search(time_buf).group(1)
        sys_time = re.compile('System time \(seconds\): ([\d.]+)$', re.MULTILINE).search(time_buf).group(1)
        max_rss = re.compile('Maximum resident set size \(kbytes\): ([\d.]+)$', re.MULTILINE).search(time_buf).group(1)

        # logging.debug("user time - {}, sys time {}".format(user_time, sys_time))
        return (float(user_time) + float(sys_time), int(max_rss) / 1024)

    def run_sim(self):
        (run_cmd, run_opts) = ("/usr/bin/time", ("-v", ) + self.exe_cmd)
        print(run_cmd, run_opts)
        time_err_buf = io.StringIO()
        time_out_buf = sh.Command(run_cmd)(run_opts, _err=time_err_buf)
        logging.debug("Simulation output - \n{}".format(time_out_buf))
        time_buf = time_err_buf.getvalue()
        time_err_buf.close()
        return time_buf

    def benchmark(self):
        total_time = 0.0
        total_mem = 0
        for i in range(self.num_sims):
            time_buf = self.run_sim()
            # accum the time
            (run_time, run_mem) = self.parse_time_res(time_buf)
            total_time += run_time
            total_mem += run_mem

        avg_time = (total_time / self.num_sims) - self.startup_offset
        avg_mem = (total_mem / self.num_sims)
        print("* Average time taken (s) : {:.3f} (determined over {} simulations)".format(avg_time, self.num_sims), file=res_file)
        print("* Average max RSS (Mb) : {:.3f} (determined over {} simulations)".format(avg_mem, self.num_sims), file=res_file)

    def analyse(self):
        if self.exe_name:
            # check compile size
            stat_res = sh.stat('-c', '%s', self.exe_name)
            print("* Sim exe ({}) file size (bytes) : {}".format(self.exe_name, stat_res), file=res_file, end="")

            # check exe inst size
            size_res = sh.size(self.exe_name)
            print("* ELF segment breakdown : \n{}".format(size_res), file=res_file, end="")
        else:
            print("* No exe file given\n", file=res_file, end="")

        # check loc
        if self.src_name:
            wc_res = sh.wc('-l', self.src_name)
            print("* Source LOC : {}".format(wc_res), file=res_file, end="")

        # check against ref results
        if self.res_name and self.res_ref:
            col = 1  # only compare V (starts at col 1)
            _, diffMax, diffEps = compare_files(self.res_name, self.res_ref, col, col)
            print("* Max difference between results files {} and {} for col {} :\n"
                  "\t{:+.16g} ({:+.16g} machine epsilons)".format(self.res_name, self.res_ref, col, diffMax, diffEps),
                  file=res_file)


class OdeSimulation(Simulation):
    def __init__(self, sim_name, mod_name, sim_params, log_prefix="ode", **kwargs):
        # update the src name with the default if not given
        # kwargs['src_name'] = kwargs.get('src_name', sim_name + ".od3")
        # NOTE - each Ode simulation must have it's own src file (use symlinks if req'd)
        kwargs['src_name'] = sim_name + ".od3"
        super().__init__(sim_name, **kwargs)
        self.mod_name = mod_name
        self.sim_params = sim_params
        self.log_prefix = log_prefix

    def build(self):
        # just a wrapper around the build module
        Build.build(self.sim_name, self.mod_name, self.sim_params, self.log_prefix)


class OdeIntSimulation(Simulation):
    def __init__(self, sim_name, mod_name, sim_params, **kwargs):
        # update the src name with the default if not given
        # kwargs['src_name'] = kwargs.get('src_name', sim_name + ".od3")
        # NOTE - each Ode simulation must have it's own src file (use symlinks if req'd)
        kwargs['src_name'] = sim_name + ".od3"
        kwargs['is_exe'] = False
        super().__init__(sim_name, **kwargs)
        self.startup_offset = 1.0  # on average for comp. time
        self.mod_name = mod_name
        self.sim_params = sim_params
        self.sim_params['backend'] = 'interpreter'
        self.script = Build._gen_sim_script(self.sim_name, self.mod_name, self.sim_params, True)

    def run_sim(self):
        os.chdir(ODE_DIR)
        time_err_buf = io.StringIO()
        time_out_buf = sh.Command("/usr/bin/time")("-v", "./ode3", _in=self.script, _err=time_err_buf)
        os.chdir(CUR_DIR)
        logging.debug("Simulation output - \n{}".format(time_out_buf))
        time_buf = time_err_buf.getvalue()
        time_err_buf.close()
        return time_buf


class MatSimulation(Simulation):
    def __init__(self, sim_name, **kwargs):
        # update the src name with the default if not given
        # kwargs['src_name'] = kwargs.get('src_name', sim_name + ".od3")
        kwargs['exe_cmd'] = ('matlab', '-nodisplay', '-nojvm', '-r "{}; exit"'.format(sim_name))
        kwargs['is_exe'] = False
        super().__init__(sim_name, **kwargs)
        self.startup_offset = 2.22


class PySimulation(Simulation):
    def __init__(self, sim_name, **kwargs):
        # update the src name with the default if not given
        # kwargs['src_name'] = kwargs.get('src_name', sim_name + ".od3")
        kwargs['exe_cmd'] = ('./{}.py'.format(sim_name), )
        kwargs['is_exe'] = False
        super().__init__(sim_name, **kwargs)
        self.startup_offset = 0.02


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
    # parser.add_argument("-o", "--ode-dir", type=str, help="Location of Ode compiler")
    args = parser.parse_args()
    ## setup globals


def runSims(sims, make=True, res_file_name="results"):
    res_file_name = "{}_{}.txt".format(res_file_name, platform.node())

    # quit is results already exist, don't want to clobber existing results
    if os.path.isfile(res_file_name):
        raise Exception("Results file {} already exists, please delete and rerun".format(res_file_name))
    
    # make all non-Ode exes
    if make:
        logging.debug("Running make -B all")
        sh.make('-B', 'all')

    global res_file
    res_file = open(res_file_name, 'w')
    print("Running set of simulations at {}".format(strftime("%H:%M:%S on %d/%m/%Y", gmtime())), file=res_file)
    print("Benchmarks run on {} under OS {}".format(platform.node(), platform.release()), file=res_file)
    print("", file=res_file)

    for sim in sims:
        res_file.flush()
        sim.execute()

    # run a sim (<filename.exe>)
    # sh.Command('HodHux_C.exe')
    # analyse results (assume output is <filename>.bin)
    # run perf benchmark multiple times

    res_file.close()
    logging.debug("Done")
