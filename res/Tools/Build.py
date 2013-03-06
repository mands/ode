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

# Globals
ODE_DIR = os.path.abspath("/home/mandeep/DPhil/Projects/ode/ode3")


def _gen_sim_script(filename, module, params, abs_output=False):
    """Generate the simulation parameters script from the params dict"""
    script = ""
    # hacky type testing
    for (k, v) in params.items():
        if v is True:
            script += ":{}\n".format(k)
        elif v is False:
            pass
        elif isinstance(v, str):
            script += ":{} \"{}\"\n".format(k, v)
        else:
            script += ":{} {}\n".format(k, v)

    # add filename specific params
    cur_dir = os.path.abspath(os.getcwd())    
    out_path = os.path.join(cur_dir, filename+'.bin') if abs_output else filename+'.bin'
    script += ":output {}\n".format(out_path)
    script += ":exeOutput {}\n".format(filename + '.exe')
    script += ":addRepo {}\n".format(os.path.relpath(cur_dir, ODE_DIR))  # setup the repo
    script += "import {}.{}\n".format(filename, module)
    script += ":simulate {}\n".format(module)
    script += ":quit\n"
    logging.debug("Sim Script - \n{}".format(script))
    return script


def get_ode_params(**alt_params):
    """General Ode simulation params, not filename specific"""
    params = {'disableUnits': True,  # disable unit checking
              # set sim params
              'startTime': 0,
              'stopTime': 30,
              'timestep': 0.001,
              'period': 0.1,
              # sim backend params
              'odeSolver': 'euler',  # odeSolver - euler, rk4, adaptive
              'sdeSolver': 'projem',  # sdeSolver - eulerm, projem
              'linker': 'dynamic',  # linker - dynamic, static
              'disableExecute': True,
              'disableOptimise': False,
              'disableShortCircuit': False,
              'disablePowerExpan': False,
              'mathModel': 'fast',  # math model - strict, fast
              'vecMath': False,
              'mathLib': 'gnu',  # mathLib - gnu, amd, intel
              'backend': 'aotcompiler'  # backend to use - interpreter, jitcompiler, aotcompiler, objectfile
              }
    # override params with func kwargs and return merge
    return dict(params, **alt_params)


def execute_ode(cur_dir, script, verbose=False, log_prefix="ode"):
    out_log_filename = os.path.join(cur_dir, ".{}.out.log".format(log_prefix))
    err_log_filename = os.path.join(cur_dir, ".{}.err.log".format(log_prefix))
    os.chdir(ODE_DIR)
    # run ode - sending output to log-files in the cur dir
    sh.Command("./ode3")(_in=script, _err=err_log_filename, _out=out_log_filename)
    if verbose:
        with open(out_log_filename, 'r') as f:
            logging.debug("Ode STDOUT contents - \n{}".format(f.read()))
        with open(err_log_filename, 'r') as f:
            logging.debug("Ode STDERR contents - \n{}".format(f.read()))
    # change back to start dir
    os.chdir(cur_dir)


def build(filename, module, params, log_prefix, verbose=False):
    cur_dir = os.path.abspath(os.getcwd())
    exe_name = filename + '.exe'
    # build the script
    params['backend'] = 'aotcompiler'
    params['disableExecute'] = True
    script = _gen_sim_script(filename, module, params)
    execute_ode(cur_dir, script, verbose=verbose, log_prefix=log_prefix)
    # move exe to cur dir
    sh.mv(os.path.join(ODE_DIR, exe_name), cur_dir)


def interpret(filename, module, params, verbose=False):
    cur_dir = os.path.abspath(os.getcwd())
    res_name = filename + '.bin'
    # build the script
    params['backend'] = 'interpreter'
    script = _gen_sim_script(filename, module, params)
    execute_ode(cur_dir, script, verbose=verbose)
    # move res to cur dir
    sh.mv(os.path.join(ODE_DIR, res_name), cur_dir)


def build_obj(filename, module, params, verbose=False):
    cur_dir = os.path.abspath(os.getcwd())
    exe_name = filename + '.o'
    # build the script
    params['backend'] = 'objectfile'
    params['disableExecute'] = True
    script = _gen_sim_script(filename, module, params)
    execute_ode(cur_dir, script, verbose=verbose)
    # move exe to cur dir
    sh.mv(os.path.join(ODE_DIR, exe_name), cur_dir)


def init():
    logging.basicConfig(level=logging.DEBUG)

    # arg parsing
    parser = argparse.ArgumentParser(
        description='Build an Ode model - simple Plython wrapper/build tool',  # main description for help
        epilog='Tested on Linux only'  # displayed after help
    )
    parser.add_argument("file", default='OdeModel', help="Filename to build")
    parser.add_argument("module", default='Model', help="Module name")
    # parser.add_argument("-d", "--default-params", action="store_true", default=True, help="Module name")
    parser.add_argument("-i", "--interpreter", action="store_true", default=False, help="Module name")
    parser.add_argument("-v", "--verbose", action="store_true", help="Verbose mode")
    args = parser.parse_args()

    return (args.file, args.module, args.interpreter, args.verbose)


if __name__ == '__main__':
    filename, module, interpreter, verbose = init()
    # run the build
    # ode setup
    params = get_ode_params()
    if interpreter:
        interpret(filename, module, params, verbose)
    else:
        build(filename, module, params, "ode", verbose)
