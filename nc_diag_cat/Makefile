all: test_nc_unlimdims.x nc_diag_cat.x

FORT90C=ifort

# DISABLE_NETCDF - if set to 1, remove NetCDF dependency and replace 
#                  with blank fillers to assist with debugging. (No, 
#                  this is not NotCDF! Nor is it politically
#                  motivated... just a way to help debug!) If you use
#                  this, -static can be used in F90FLAGS. When using
#                  static, NETCDF_FLAGS should be set to empty, and
#                  -fstack-protector should be removed from F90FLAGS.
#                  
#                  All of the above is automatically done for you. This
#                  only needs to be set manually if you set -DNO_NETCDF
#                  below in COMPILE_FLAGS.
ifndef DISABLE_NETCDF
	DISABLE_NETCDF=0
endif

ifeq ($(DISABLE_NETCDF), 1)
	NETCDF_FLAGS=
	COMPILE_FLAGS+=-DNO_NETCDF
	F90FLAGS+=-O0 -static -fpp -g -check all -warn all -traceback \
		-ftrapuv -debug all -fpe0 -warn all -traceback -debug extended
	NETCDF_LAYER_DEPS+=netcdf_nonetcdf_decl.F90 \
		netcdf_nonetcdf_imp.F90 \
		netcdf_nonetcdf_putvar.F90
else
	F90FLAGS+=-O0 -fpp -g -pg -fstack-protector -check all \
		-ftrapuv -debug all -fpe0 -warn all -traceback -debug extended
	NETCDF_FLAGS+=-I $(BASEDIR)/Linux/include/netcdf -L$(BASEDIR)/Linux/lib \
		-lnetcdff -lnetcdf -lhdf5 -lcurl -lz -lrt -lmpi -lhdf5_hl -lhdf5 \
		-lz -lsz -lgpfs -lmfhdf -ldf -ljpeg
endif

# Available compile flags:
#   -D_REAL8_          - define the default "real" number
#                        (other kinds defines available!)
#   -D_DEBUG_MEM_      - enable verbose memory debugging messages.
#                        WARNING - this is a LOT of messages!
#   -DANSI_TERM_COLORS - enable terminal colors for warning and error
#                        messages.
#   -DERROR_TRACEBACK  - enable a traceback on error. This is useful for
#                        debugging any issues with code or NCLayer.
#                        This works by triggering a divide-by-zero
#                        runtime error, which generates a traceback.
#   -DIGNORE_VERSION   - ignore the version of NetCDF4. This disables
#                        any bug checks related to NetCDF4 versions.
#                        WARNING - do not define this (aka disable
#                        version checks) unless you know what you're
#                        doing! (Hint: if you don't want NetCDF4 to
#                        crash with an error message, don't define
#                        this!)
#   -DNO_NETCDF        - see DISABLE_NETCDF above for details.
#                        
#                        NOTE - this is set automatically if
#                        DISABLE_NETCDF is set.
COMPILE_FLAGS+=-D_REAL8_ -DANSI_TERM_COLORS -DIGNORE_VERSION -DERROR_TRACEBACK -DENABLE_ACTION_MSGS

NC_DIAG_CAT_DEPS+=Makefile \
	ncdc_util.F90 \
	ncdc_cli_decl.F90 ncdc_cli_imp.F90 \
	ncdc_metadata_decl.F90 ncdc_metadata_imp.F90 \
	ncdc_data_decl.F90 ncdc_data_imp.F90

kinds.mod: Makefile kinds.F90
	$(FORT90C) $(F90FLAGS) -c kinds.F90 $(COMPILE_FLAGS)

nc_diag_cat.x: $(NC_DIAG_CAT_DEPS) ncdc_realloc.mod netcdf_unlimdims.mod nc_diag_cat.f90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) nc_diag_cat.f90 ncdc_realloc.o netcdf_unlimdims.o $(NETCDF_FLAGS) -o nc_diag_cat.x

test_nc_unlimdims.x: netcdf_unlimdims.mod ncdc_util.F90 test_nc_unlimdims.f90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) test_nc_unlimdims.f90 netcdf_unlimdims.o $(NETCDF_FLAGS) -o test_nc_unlimdims.x

gen_diff_size_netcdf.x: gen_diff_size_netcdf.f90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) gen_diff_size_netcdf.f90 \
		../nc_diag/netcdf_layer.o ../nc_diag/utils.o -I../nc_diag -L../nc_diag \
		$(NETCDF_FLAGS) -o gen_diff_size_netcdf.x

ncdc_realloc.mod: Makefile kinds.mod ncdc_realloc.f90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) -c ncdc_realloc.f90

netcdf_unlimdims.mod: Makefile netcdf_unlimdims.F90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) -c netcdf_unlimdims.F90 $(NETCDF_FLAGS)

utils.mod: Makefile utils.f90
	$(FORT90C) $(F90FLAGS) -c utils.f90

clean:
	rm -f *.mod *.x *.o
