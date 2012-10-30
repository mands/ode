#!/usr/bin/env python

"""
    compare program that takes two sets of results files, and calculates the maximum error value
    NOTE - this should only work on amd64 systems as uses hardcoded element sizes and (little-)endineness
"""
import os
import logging
import argparse
import scipy as sp
import sys

from OdeSupport.FileReader import openFile

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    logging.debug("In main python script")

    parser = argparse.ArgumentParser()
    parser.add_argument("file1", help="First file to compare")
    parser.add_argument("file2", help="Second file to compare")
    parser.add_argument("--col1", type=int, help="Column of File1 to use")
    parser.add_argument("--col2", type=int, help="Column of File2 to use")
    parser.add_argument("-v", "--verbose", action="store_true", help="Verbose mode")
    args = parser.parse_args()

    filename1 = os.path.abspath(args.file1)
    filename2 = os.path.abspath(args.file2)

    ## get the files
    (a, colsA) = openFile(filename1)
    (b, colsB) = openFile(filename2)

    # get columns (if needed)
    if (args.col1 != None) and (args.col2 != None):
        logging.debug("In column mode")
        if args.col1 < 0 or args.col1 >= colsA:
            raise Exception("Col1 index not contained within File1")
        if args.col2 < 0 or args.col2 >= colsB:
            raise Exception("Col2 index not contained within File2")
        logging.debug("Got col args {0} and {1}".format(args.col1, args.col2))
        # destructively update a and b
        a = a[:,args.col1]
        b = b[:,args.col2]
    else:
        # we are performing a whole-matrix diff
        if colsA != colsB:
            raise Exception("Files are not of the same size")

    diffAbs = (sp.absolute(sp.subtract(a, b)))
    diffMax = sp.nanmax(diffAbs)
    diffEps = diffMax / sys.float_info.epsilon

    print("Max difference is {:+.16g} ({:+.16g} machine epsilons)".format(diffMax, diffEps))

    if args.verbose:
        sp.set_printoptions(edgeitems=3, linewidth=400, precision=16,suppress=False, threshold=10000)
        print (diffAbs)
    logging.debug("Done")

