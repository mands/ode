#!/usr/bin/python3
# test client that opens a pipe - on command line, and continously writes line input into it
import os
import readline
import argparse

def main(pipename):
    print("Simple pipe console")
    print("Using pipe", pipename, "- waiting for reader...")

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
          except (IOError):
              print("Connection closed by other side")
              break

    print("Exiting")

if __name__ == "__main__":
    # cmd line parser
    parser = argparse.ArgumentParser(
                description = 'Echos input into named pipe', # main description for help
                epilog = 'Tested on Linux only') # displayed after help
    parser.add_argument('name', type=str, nargs='?', default='./pypipe', help="The name of the pipe to write to")
    args = parser.parse_args()

    main(args.name)

