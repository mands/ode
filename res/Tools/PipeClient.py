#!/usr/bin/env python3
"""
    Test client that opens a pipe and continuously writes command-line input into it
"""

import os
import argparse
import logging

if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG)

    # cmd line parser
    parser = argparse.ArgumentParser(
        description = 'Echos input into named pipe', # main description for help
        epilog = 'Tested on Linux only' # displayed after help
    )
    parser.add_argument('name', type=str, nargs='?', default='./pypipe', help="Pipe name")
    args = parser.parse_args()

    pipename = args.name

    logging.debug("Simple pipe console")
    print("Using pipe {} - waiting for reader...".format(pipename))

    # create and open the pipe
    if not os.path.exists(pipename):
        os.mkfifo(pipename)

    with open(pipename, 'wt', buffering=1, errors='strict', newline='\n') as pipe:
        print("Connected")

        # main loop
        while True:
            try:
                cmd = input("> ")
                pipe.write(cmd + '\n') # have to readd the newline
            except (EOFError, KeyboardInterrupt):
                print("Connection closed by this side")
                break
            except IOError:
                print("Connection closed by other side")
                break

    logging.debug("Done")
