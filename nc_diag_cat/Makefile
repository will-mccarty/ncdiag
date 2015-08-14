ifndef USE_MPI
	USE_MPI=1
endif

ifeq ($(USE_MPI), 1)
	FORT90C=mpif90
	COMPILE_FLAGS+=-DUSE_MPI
	NC_DIAG_CAT_DEPS+=ncdc_data_MPI.mod
	NC_DIAG_CAT_OBJS+=ncdc_data_MPI.o
else
	NC_DIAG_CAT_DEPS+=ncdc_data.mod
	NC_DIAG_CAT_OBJS+=ncdc_data.o
	FORT90C=ifort
endif

# Available compile flags:
#   -D_REAL8_          - define the default "real" number
#                        (other kinds defines available!)
#   -D_DEBUG_MEM_      - enable verbose memory debugging messages.
#                        WARNING - this is a LOT of messages!
#   -DQUIET            - silence most (if not all) verbose output from
#                        nc_diag_cat. This include info messages. Note
#                        that this does NOT silence error or warning
#                        messages.
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
COMPILE_FLAGS+=-D_REAL8_ -DANSI_TERM_COLORS -DIGNORE_VERSION -DERROR_TRACEBACK -DENABLE_ACTION_MSGS -DQUIET

NC_DIAG_CAT_DEPS+=Makefile \
	ncdc_realloc.mod ncdc_state.mod ncdc_climsg.mod ncdc_types.mod \
	ncdc_cli_process.mod \
	ncdc_dims.mod ncdc_vars.mod \
	ncdc_metadata.mod \
	netcdf_unlimdims.mod

NC_DIAG_CAT_OBJS+=\
	ncdc_realloc.o ncdc_state.o ncdc_climsg.o ncdc_types.o \
	ncdc_cli_process.o \
	ncdc_dims.o ncdc_vars.o \
	ncdc_metadata.o \
	netcdf_unlimdims.o

all: test_nc_unlimdims.x nc_diag_cat.x

kinds.mod: Makefile kinds.F90
	$(FORT90C) $(F90FLAGS) -c kinds.F90 $(COMPILE_FLAGS)

ncdc_types.mod: ncdc_types.f90 kinds.mod
	$(FORT90C) $(F90FLAGS) -c ncdc_types.f90 $(COMPILE_FLAGS)

ncdc_climsg.mod: ncdc_climsg.F90 ncdc_state.mod kinds.mod
	$(FORT90C) $(F90FLAGS) -c ncdc_climsg.F90 $(COMPILE_FLAGS) $(NETCDF_FLAGS)

ncdc_realloc.mod: ncdc_realloc.F90 ncdc_state.mod kinds.mod
	$(FORT90C) $(F90FLAGS) -c ncdc_realloc.F90 $(COMPILE_FLAGS)

ncdc_state.mod: ncdc_state.F90 ncdc_types.mod kinds.mod
	$(FORT90C) $(F90FLAGS) -c ncdc_state.F90 $(COMPILE_FLAGS)

ncdc_cli_process.mod: ncdc_cli_process.F90 ncdc_state.mod kinds.mod
	$(FORT90C) $(F90FLAGS) -c ncdc_cli_process.F90 $(COMPILE_FLAGS) $(NETCDF_FLAGS)

ncdc_dims.mod: ncdc_dims.F90 ncdc_realloc.mod ncdc_climsg.mod ncdc_state.mod ncdc_types.mod kinds.mod
	$(FORT90C) $(F90FLAGS) -c ncdc_dims.F90 $(COMPILE_FLAGS) $(NETCDF_FLAGS)

ncdc_vars.mod: ncdc_vars.F90 ncdc_dims.mod ncdc_climsg.mod ncdc_state.mod ncdc_types.mod kinds.mod
	$(FORT90C) $(F90FLAGS) -c ncdc_vars.F90 $(COMPILE_FLAGS) $(NETCDF_FLAGS)

ncdc_metadata.mod: ncdc_metadata.F90 ncdc_cli_process.mod netcdf_unlimdims.mod ncdc_dims.mod ncdc_vars.mod ncdc_climsg.mod ncdc_state.mod ncdc_types.mod kinds.mod
	$(FORT90C) $(F90FLAGS) -c ncdc_metadata.F90 $(COMPILE_FLAGS) $(NETCDF_FLAGS)

ncdc_data.mod: ncdc_data.F90 ncdc_cli_process.mod ncdc_dims.mod ncdc_vars.mod ncdc_climsg.mod ncdc_state.mod ncdc_types.mod kinds.mod
	$(FORT90C) $(F90FLAGS) -c ncdc_data.F90 $(COMPILE_FLAGS) $(NETCDF_FLAGS)

ncdc_data_MPI.mod: ncdc_data_MPI.F90 ncdc_cli_process.mod ncdc_dims.mod ncdc_vars.mod ncdc_climsg.mod ncdc_state.mod ncdc_types.mod kinds.mod
	$(FORT90C) $(F90FLAGS) -c ncdc_data_MPI.F90 $(COMPILE_FLAGS) $(NETCDF_FLAGS)

nc_diag_cat.x: $(NC_DIAG_CAT_DEPS) ncdc_realloc.mod netcdf_unlimdims.mod nc_diag_cat.F90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) nc_diag_cat.F90 $(NC_DIAG_CAT_OBJS) $(NETCDF_FLAGS) -o nc_diag_cat.x

test_nc_unlimdims.x: netcdf_unlimdims.mod ncdc_climsg.mod test_nc_unlimdims.F90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) test_nc_unlimdims.F90 ncdc_climsg.o ncdc_state.o netcdf_unlimdims.o $(NETCDF_FLAGS) -o test_nc_unlimdims.x

netcdf_unlimdims.mod: Makefile netcdf_unlimdims.F90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) -c netcdf_unlimdims.F90 $(NETCDF_FLAGS)

clean:
	rm -f *.mod *.x *.o
