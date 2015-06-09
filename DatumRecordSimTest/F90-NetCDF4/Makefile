all: test

test: test.f90
	ifort -O2 test.f90 -o test

clean:
	rm -f test *.mod
