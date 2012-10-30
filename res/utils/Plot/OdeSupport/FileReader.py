import os
import struct
import logging
from optparse import OptionParser
import scipy as sp
import pylab

def openFile(filename):
    # parse the file
    with open(filename, 'rb') as f:
        # parse the header
        # get the num columns
        (cols, ) = struct.unpack('i', f.read(4))
        logging.debug("Number of cols %d" % cols)

        # skip to the data
        a = sp.fromfile(f, dtype='f8', count=-1)
        a = sp.reshape(a, (-1, cols))

    return (a, cols)
