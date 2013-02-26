import struct
import logging
import scipy as sp


def openFile(filename):
    # parse the file
    with open(filename, 'rb') as f:
        # parse the header
        # get the num columns
        (cols, ) = struct.unpack('=Q', f.read(8))
        logging.debug("Number of cols = %d" % cols)

        # skip to the data
        a = sp.fromfile(f, dtype='f8', count=-1)
        a = sp.reshape(a, (-1, cols))

    return a, cols


def saveFile(data, filename):
    with open(filename, 'wb') as f:
        # write the header
        (r, c) = data.shape
        cols = struct.pack('=Q', c)
        f.write(cols)
        # write the data
        data.tofile(f)


# setup the cols
def getCols(num_cols, col_list=None, add_zero=False):
    if col_list:
        cols = [int(c) for c in col_list.split(',') if 0 < int(c) < num_cols]
    else:
        cols = list(range(1, num_cols))
    if add_zero:
        cols.insert(0,0)
        return cols
    else:
        return cols


def setupPrint():
    sp.set_printoptions(edgeitems=3, linewidth=400, precision=16,suppress=False, threshold=10000)
