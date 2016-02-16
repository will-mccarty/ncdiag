#F90FLAGS = -g -traceback
F90FLAGS = -O2

all: test_index.x test_indvarr.x

test_index.x: test_index.f90
	ifort $(F90FLAGS) test_index.f90 -o test_index.x

test_indvarr.x: test_indvarr.f90
	ifort $(F90FLAGS) test_indvarr.f90 -o test_indvarr.x

clean:
	rm -f *.x
