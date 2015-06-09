all: test.x

test.x: test.f90
	ifort -O2 test.f90 -o test.x

clean:
	rm -f test.x
