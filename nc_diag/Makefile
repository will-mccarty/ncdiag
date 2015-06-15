all: test_netcdf_layer.x generic_data.x test_utils.x

F90FLAGS=-fpp -g -pg -check bounds -fstack-protector -warn all -traceback
#F90FLAGS=-O2
NETCDF_FLAGS=-I $(BASEDIR)/Linux/include/netcdf -L$(BASEDIR)/Linux/lib \
	-lnetcdff -lnetcdf -lhdf5 -lcurl -lz -lrt -lmpi -lhdf5_hl -lhdf5 \
	-lz -lsz -lgpfs -lmfhdf -ldf -ljpeg

COMPILE_FLAGS=-D_REAL8_ -DANSI_TERM_COLORS

NETCDF_LAYER_DEPS=netcdf_realloc_decl.f90 \
	netcdf_lheader_decl.f90 netcdf_chaninfo_decl.F90 \
	netcdf_realloc_imp.f90 netcdf_hresize.F90 \
	netcdf_ciresize.F90 netcdf_lheader_imp.F90 \
	netcdf_chaninfo_imp.F90

kinds.mod: kinds.F90
	ifort $(F90FLAGS) -c kinds.F90 $(COMPILE_FLAGS)

netcdf_layer.mod: kinds.mod utils.mod fnv32mod.mod $(NETCDF_LAYER_DEPS) netcdf_layer.F90
	ifort $(F90FLAGS) -c netcdf_layer.F90 $(NETCDF_FLAGS) $(COMPILE_FLAGS)

fnv32mod.mod: fnv32mod.f90
	ifort $(F90FLAGS) -c fnv32mod.f90

test_netcdf_layer.x: netcdf_layer.mod test_netcdf_layer.f90
	ifort $(F90FLAGS) -o test_netcdf_layer.x test_netcdf_layer.f90 \
		netcdf_layer.o utils.o $(NETCDF_FLAGS) $(COMPILE_FLAGS)

generic_data.x: netcdf_layer.mod generic_data.f90
	ifort $(F90FLAGS) -o generic_data.x generic_data.f90

test_utils.x: utils.mod test_utils.f90
	ifort $(F90FLAGS) -o test_utils.x utils.o test_utils.f90

utils.mod: utils.f90
	ifort $(F90FLAGS) -c utils.f90

clean:
	rm -f *.mod *.x *.o
