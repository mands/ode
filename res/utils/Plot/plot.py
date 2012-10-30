#!/usr/bin/env python
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
import pylab
import argparse
import scipy as sp

from OdeSupport.FileReader import openFile

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    logging.debug("In main python script")

    # arg parsing
    parser = argparse.ArgumentParser()
    parser.add_argument("file", help="File to plot")
    parser.add_argument("-v", "--verbose", action="store_true", help="Verbose mode")
    parser.add_argument("--title", type=str, help="Title to use for graph")
    parser.add_argument("-s", "--save", type=str, help="Write the output to file")
    parser.add_argument("-c", "--cols", type=str, help="Plot this subset of cols=x,y,z")
    parser.add_argument("-x", "--xlabel", type=str, default="Time (s)", help="Label for the x-axis")
    parser.add_argument("-y", "--ylabel", type=str, default="Value", help="Label for the y-axis")
    args = parser.parse_args()

    # open the file
    filename = os.path.abspath(args.file)
    (data, num_cols) = openFile(filename)

    # setup the cols
    cols = range(1, num_cols)
    if args.cols:
      cols = [int(c) for c in args.cols.split(',') if int(c) > 0 and int(c) < num_cols]
    
    # plot the data
    logging.debug("Setting up a plot")
    # add each specified column to the plot
    for col in cols:
        pylab.plot(data[:,0], data[:,col])

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

