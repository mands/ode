#!/usr/bin/env python3

"""
    ParamGen takes a list of key-value pairs on the command-line and geneates an Ode module based on these constants
"""
import logging
import argparse

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)

    parser = argparse.ArgumentParser(
        description = 'ParamGen takes a list of key-value pairs as arguments and generates an Ode module from them', # main description for help
        epilog = 'Tested on Linux only' # displayed after help
    )
    parser.add_argument("-o", "--output", type=str, default="Params.od3", help="File to write output to")
    parser.add_argument("-n", "--name", type=str, default="Params", help="Module name for the params")
    parser.add_argument("-v", "--verbose", action="store_true", help="Verbose mode")
    parser.add_argument("params", nargs='+', help="List of key=value parameters")

    args = parser.parse_args()

    logging.debug("Writing params into module {} in file {}".format(args.name, args.output))
    # open the output file
    with open(args.output, 'w') as f:
        # write the module header
        f.write("// Code-generated params file, needs to be imported into Ode files\n")
        f.write("module {} {{\n".format(args.name))

        # write the params
        for param in args.params:
            try:
                paramName, paramVal = param.split('=')
            except ValueError:
                print("Param \"{}\" not in correct format, needs to be \"param=val\"".format(param))
                exit(1)
            f.write("    val {} = {}\n".format(paramName, paramVal))

        # close the module
        f.write("}\n")

    # write the file to output if verbose
    if args.verbose:
        with open(args.output, 'r') as f:
            for line in f:
                print(line, end='')

    logging.debug("Done")
