ghc := ghc
ghcflags := -Wall -optl -w
# -DHAS_GETPOINTERTOGLOBAL=1
examples := HelloJIT Fibonacci BrainF Vector Array DotProd Arith Align Struct Varargs

all: $(examples:%=%.exe)

Vector:	Convert.hs

%.exe: %.hs
	$(ghc) $(ghcflags) --make -o $(basename $<).exe -main-is $(basename $<).main $<

Struct.exe:	Struct.hs structCheck.c
	$(ghc) $(ghcflags) --make -o Struct.exe -main-is Struct.main Struct.hs structCheck.c

%.run: %.exe
	./$<

run:	$(examples:%=%.run)

N=40
fastfib:	Fibonacci.exe
	@rm -f Fib.bc Fib.s
	time ./Fibonacci.exe $(N)
	opt-2.7 -std-compile-opts Fibonacci.bc -o Fib.bc
	llc-2.7 Fib.bc
	$(CC) mainfib.c Fib.s -o Fib
	time ./Fib $(N)
	@echo Have a look at Fib.s if you like to see clever code.

clean:
	rm -f $(examples) *.o *.hi *.s *.bc Fib *.exe *.exe.manifest *~
