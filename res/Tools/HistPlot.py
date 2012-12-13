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
    anaylse program that takes a results files of binary numbers, and plots a histogram from a single column
    NOTE - this should only work on amd64 systems as uses hardcoded element sizes and (little-)endineness
"""

import os
import logging
import pylab
import argparse
import scipy as sp

from Common import openFile, getCols

if __name__ == '__main__':

    logging.basicConfig(level=logging.DEBUG)


    # arg parsing
    parser = argparse.ArgumentParser(
        description = 'HistPlot takes an Ode output file and plots a histogram from a single column', # main description for help
        epilog = 'Tested on Linux only' # displayed after help
    )
    parser.add_argument("file", help="File to plot")
    parser.add_argument("-v", "--verbose", action="store_true", help="Verbose mode")
    parser.add_argument("--title", type=str, help="Title to use for graph")
    parser.add_argument("-s", "--save", type=str, help="Write the output to file")
    parser.add_argument("-c", "--col", type=int, help="Plot this col")
    parser.add_argument("-x", "--xlabel", type=str, default="Value", help="Label for the x-axis")
    parser.add_argument("-y", "--ylabel", type=str, default="Frequency", help="Label for the y-axis")
    args = parser.parse_args()

    # open the file
    filename = os.path.abspath(args.file)
    (data, num_cols) = openFile(filename)

    # print some data stats
    print("Min : {:+.16g}, Max : {:+.16g}, Mean : {:+.16g}".format(sp.amin(data), sp.amax(data), sp.mean(data)))

    # plot the data
    logging.debug("Setting up a plot")
    # get and plot the specified columns
    plot_col = 0
    if num_cols > 1 and args.col is not None and args.col <= num_cols:
        plot_col = args.col - 1

    pylab.hist(data[:,plot_col], bins=100)

    # add the labels
    pylab.xlabel(args.xlabel)
    pylab.ylabel(args.ylabel)
    if args.title:
        pylab.title(args.title)

    # save or display the graph
    if args.save:
        pylab.savefig(args.save, format='pdf')
    else:
        pylab.grid(True)
        pylab.show()

    logging.debug("Done")
