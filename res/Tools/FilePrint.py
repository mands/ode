#!/usr/bin/env python3

"""
    compare program that takes two sets of results files, and calculates the maximum error value
    NOTE - this should only work on amd64 systems as uses hardcoded element sizes and (little-)endineness
"""
import os
import logging
import argparse
import scipy as sp

from OdeSupport.FileReader import openFile

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    logging.debug("In main python script")

    parser = argparse.ArgumentParser()
    parser.add_argument("file", help="First file to print")
    parser.add_argument("-v", "--verbose", action="store_true", help="Verbose mode")
    args = parser.parse_args()

    filename = os.path.abspath(args.file)

    ## get the files
    (a, colsA) = openFile(filename)
    sp.set_printoptions(edgeitems=3, linewidth=400, precision=16,suppress=False, threshold=10000)
    print (a)
    logging.debug("Done")
