#!/usr/bin/env python3

"""
    paramGen takes a list of key-value pairs on the command-line and geneates an Ode module based on these constants
"""
import os
import logging
import argparse

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    logging.debug("In main python script")

    parser = argparse.ArgumentParser()
    parser.add_argument("-o", "--output", type=str, default="Params.od3", help="File to write output to")
    parser.add_argument("-v", "--verbose", action="store_true", help="Verbose mode")
    parser.add_argument("params", nargs='+', help="List of key=value parameters")

    args = parser.parse_args()

    for param in args.params:
      logging.debug(param)
      

