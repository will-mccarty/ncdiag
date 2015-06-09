all: test_netcdf_layer.x generic_data.x

F90FLAGS=-g -check bounds -fstack-protector -warn all -traceback
NETCDF_FLAGS=-I $(BASEDIR)/Linux/include/netcdf -L$(BASEDIR)/Linux/lib -lnetcdff -lnetcdf -lhdf5 -lcurl -lz -lrt -lmpi -lhdf5_hl -lhdf5 -lz -lsz -lgpfs -lmfhdf -ldf -ljpeg

NETCDF_LAYER_DEPS=netcdf_realloc_int_header.f90 \
	netcdf_layer_int_header.f90 netcdf_realloc_sub_header.f90 \
	netcdf_hresize_sub_header.f90 netcdf_layer_sub_header.f90

kinds.mod: kinds.F90
	ifort $(F90FLAGS) -c kinds.F90 -D_REAL8_

netcdf_layer.mod: kinds.mod fnv32mod.mod $(NETCDF_LAYER_DEPS) netcdf_layer.f90
	ifort $(F90FLAGS) -c netcdf_layer.f90 $(NETCDF_FLAGS) -D_REAL8_

fnv32mod.mod: fnv32mod.f90
	ifort $(F90FLAGS) -c fnv32mod.f90

test_netcdf_layer.x: netcdf_layer.mod test_netcdf_layer.f90
	ifort $(F90FLAGS) -o test_netcdf_layer.x test_netcdf_layer.f90 netcdf_layer.o $(NETCDF_FLAGS) -D_REAL8_

generic_data.x: netcdf_layer.mod generic_data.f90
	ifort $(F90FLAGS) -o generic_data.x generic_data.f90

clean:
	rm -f *.mod *.x *.o
