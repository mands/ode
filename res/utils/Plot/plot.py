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
    NOTE - this should only work on amd64 systems as uses hardcoded element sizes and endineness
"""

import os
import struct
import logging
from optparse import OptionParser
import scipy as sp
import pylab

def plot(data, cols, title = 'graph', save = False):
    logging.debug("Setting up a plot")
    for col in range(1, cols):
        pylab.plot(data[:,0], data[:,col])

    pylab.xlabel("Time (s)")
    pylab.ylabel("Populations")
    pylab.title(title)
    pylab.grid(True)
    if save:
        pylab.savefig(title + ".eps", format='eps')
    pylab.show()

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    logging.debug("In main python script")

    #parse cmd line args
    usage = "usage: %prog [options] FILE"
    version="%prog 0.1"
    parser = OptionParser(usage=usage, version=version)
    # do we have any options yet...
    (options, args) = parser.parse_args()

    if len(args) < 1:
        parser.error("incorrect number of arguments")
    else:
        filename = os.path.abspath(args[0])
        (path, filen) = os.path.split(filename)
        (title, ext) = os.path.splitext(filen)

    # parse the file
    with open(filename, 'rb') as f:
        # parse the header
        # get the num columns
        (cols, ) = struct.unpack('i', f.read(4))
        logging.debug("Number of cols %d" % cols)

        # skip to the data
        a = sp.fromfile(f, dtype='f8', count=-1)
        a = sp.reshape(a, (-1, cols))

    plot(a, cols, title, save=False)
    logging.debug("Done")