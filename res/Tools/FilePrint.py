#!/usr/bin/env python3

"""
    compare program that takes two sets of results files, and calculates the maximum error value
    NOTE - this should only work on amd64 systems as uses hardcoded element sizes and (little-)endineness
"""
import os
import logging
import argparse
import scipy as sp

from Common import openFile, getCols

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)

    parser = argparse.ArgumentParser(
        description = 'FilePrint prints an Ode output file to the screen', # main description for help
        epilog = 'Tested on Linux only' # displayed after help
    )
    parser.add_argument("file", help="First file to print")
    parser.add_argument("-c", "--cols", type=str, help="Print this subset of cols=x,y,z")
    #parser.add_argument("-v", "--verbose", action="store_true", help="Verbose mode")
    args = parser.parse_args()

    filename = os.path.abspath(args.file)

    ## get the files
    (data, num_cols) = openFile(filename)
    # set print ops
    sp.set_printoptions(edgeitems=3, linewidth=400, precision=16,suppress=False, threshold=10000)
    # get and print the specified columns
    cols = getCols(num_cols, args.cols, True)
    print (data[:,cols])
    logging.debug("Done")
