## Top level makefile
## Uses recusrive makes - i know not recoemmned but is easiest solution for now
## everything depends on src/Stdlib, but otherwise all sub-modules are independent

include Makefile.inc

SRC_DIRS := src/StdLib src/Solvers src/Utils src/VecMath

all:
	-for d in $(SRC_DIRS); do (cd $$d; $(MAKE) ); done

install:
	-for d in $(SRC_DIRS); do (cd $$d; $(MAKE) install); done

clean:
	-for d in $(SRC_DIRS); do (cd $$d; $(MAKE) clean ); done
#	rm -f *.o *~ *.bin

force_look:
	true

