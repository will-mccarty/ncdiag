all: hashingtest.x arrayhashtest.x

F90FLAGS=-fpp -g -pg -check bounds -fstack-protector -warn all -traceback
#F90FLAGS=-O2
NETCDF_FLAGS=-I $(BASEDIR)/Linux/include/netcdf -L$(BASEDIR)/Linux/lib \
	-lnetcdff -lnetcdf -lhdf5 -lcurl -lz -lrt -lmpi -lhdf5_hl -lhdf5 \
	-lz -lsz -lgpfs -lmfhdf -ldf -ljpeg

COMPILE_FLAGS=-D_REAL8_

kinds.mod: kinds.F90
	ifort $(F90FLAGS) -c kinds.F90 $(COMPILE_FLAGS)

netcdf_layer.mod: kinds.mod fnv32mod.mod $(NETCDF_LAYER_DEPS) netcdf_layer.F90
	ifort $(F90FLAGS) -c netcdf_layer.F90 $(NETCDF_FLAGS) $(COMPILE_FLAGS)

fnv32mod.mod: kinds.mod fnv32mod.f90
	ifort $(F90FLAGS) -c fnv32mod.f90

minidict.mod: kinds.mod fnv32mod.mod minidict.f90
	ifort $(F90FLAGS) -c minidict.f90

arrayhashtest.x: fnv32mod.mod minidict.mod arrayhashtest.f90
	ifort $(F90FLAGS) -o arrayhashtest.x arrayhashtest.f90 fnv32mod.o

hashingtest.x: fnv32mod.mod hashingtest.f90
	ifort $(F90FLAGS) -o hashingtest.x hashingtest.f90 fnv32mod.o

clean:
	rm -f *.mod *.x *.o
