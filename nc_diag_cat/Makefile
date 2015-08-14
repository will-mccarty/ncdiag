# Include build configuration
include Makefile.config

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

kinds.mod: kinds.F90 Makefile Makefile.config
	$(NCDC_MAKE_FANCY_BEGIN)
	$(NCDC_MAKE_QUIET)$(NCDC_FORT90C) $(NCDC_F90FLAGS) $(NCDC_CONFIG_FLAGS) $(NCDC_CONFIG_FLAGS) -c kinds.F90
	$(NCDC_MAKE_FANCY_END)

ncdc_types.mod: ncdc_types.f90 kinds.mod
	$(NCDC_MAKE_FANCY_BEGIN)
	$(NCDC_MAKE_QUIET)$(NCDC_FORT90C) $(NCDC_F90FLAGS) $(NCDC_CONFIG_FLAGS) -c ncdc_types.f90
	$(NCDC_MAKE_FANCY_END)

ncdc_climsg.mod: ncdc_climsg.F90 ncdc_state.mod kinds.mod
	$(NCDC_MAKE_FANCY_BEGIN)
	$(NCDC_MAKE_QUIET)$(NCDC_FORT90C) $(NCDC_F90FLAGS) $(NCDC_CONFIG_FLAGS) -c ncdc_climsg.F90 $(NCDC_NETCDF_FLAGS)
	$(NCDC_MAKE_FANCY_END)

ncdc_realloc.mod: ncdc_realloc.F90 ncdc_state.mod kinds.mod
	$(NCDC_MAKE_FANCY_BEGIN)
	$(NCDC_MAKE_QUIET)$(NCDC_FORT90C) $(NCDC_F90FLAGS) $(NCDC_CONFIG_FLAGS) -c ncdc_realloc.F90
	$(NCDC_MAKE_FANCY_END)

ncdc_state.mod: ncdc_state.F90 ncdc_types.mod kinds.mod
	$(NCDC_MAKE_FANCY_BEGIN)
	$(NCDC_MAKE_QUIET)$(NCDC_FORT90C) $(NCDC_F90FLAGS) $(NCDC_CONFIG_FLAGS) -c ncdc_state.F90
	$(NCDC_MAKE_FANCY_END)

ncdc_cli_process.mod: ncdc_cli_process.F90 ncdc_state.mod kinds.mod
	$(NCDC_MAKE_FANCY_BEGIN)
	$(NCDC_MAKE_QUIET)$(NCDC_FORT90C) $(NCDC_F90FLAGS) $(NCDC_CONFIG_FLAGS) -c ncdc_cli_process.F90 $(NCDC_NETCDF_FLAGS)
	$(NCDC_MAKE_FANCY_END)

ncdc_dims.mod: ncdc_dims.F90 ncdc_realloc.mod ncdc_climsg.mod ncdc_state.mod ncdc_types.mod kinds.mod
	$(NCDC_MAKE_FANCY_BEGIN)
	$(NCDC_MAKE_QUIET)$(NCDC_FORT90C) $(NCDC_F90FLAGS) $(NCDC_CONFIG_FLAGS) -c ncdc_dims.F90 $(NCDC_NETCDF_FLAGS)
	$(NCDC_MAKE_FANCY_END)

ncdc_vars.mod: ncdc_vars.F90 ncdc_dims.mod ncdc_climsg.mod ncdc_state.mod ncdc_types.mod kinds.mod
	$(NCDC_MAKE_FANCY_BEGIN)
	$(NCDC_MAKE_QUIET)$(NCDC_FORT90C) $(NCDC_F90FLAGS) $(NCDC_CONFIG_FLAGS) -c ncdc_vars.F90 $(NCDC_NETCDF_FLAGS)
	$(NCDC_MAKE_FANCY_END)

ncdc_metadata.mod: ncdc_metadata.F90 ncdc_cli_process.mod netcdf_unlimdims.mod ncdc_dims.mod ncdc_vars.mod ncdc_climsg.mod ncdc_state.mod ncdc_types.mod kinds.mod
	$(NCDC_MAKE_FANCY_BEGIN)
	$(NCDC_MAKE_QUIET)$(NCDC_FORT90C) $(NCDC_F90FLAGS) $(NCDC_CONFIG_FLAGS) -c ncdc_metadata.F90 $(NCDC_NETCDF_FLAGS)
	$(NCDC_MAKE_FANCY_END)

ncdc_data.mod: ncdc_data.F90 ncdc_cli_process.mod ncdc_dims.mod ncdc_vars.mod ncdc_climsg.mod ncdc_state.mod ncdc_types.mod kinds.mod
	$(NCDC_MAKE_FANCY_BEGIN)
	$(NCDC_MAKE_QUIET)$(NCDC_FORT90C) $(NCDC_F90FLAGS) $(NCDC_CONFIG_FLAGS) -c ncdc_data.F90 $(NCDC_NETCDF_FLAGS)
	$(NCDC_MAKE_FANCY_END)

ncdc_data_mpi.mod: ncdc_data_MPI.F90 ncdc_cli_process.mod ncdc_dims.mod ncdc_vars.mod ncdc_climsg.mod ncdc_state.mod ncdc_types.mod kinds.mod
	$(NCDC_MAKE_FANCY_BEGIN)
	$(NCDC_MAKE_QUIET)$(NCDC_FORT90C) $(NCDC_F90FLAGS) $(NCDC_CONFIG_FLAGS) -c ncdc_data_MPI.F90 $(NCDC_NETCDF_FLAGS)
	$(NCDC_MAKE_FANCY_END)

nc_diag_cat.x: $(NC_DIAG_CAT_DEPS) ncdc_realloc.mod netcdf_unlimdims.mod nc_diag_cat.F90
	$(NCDC_MAKE_FANCY_BIGLIB_BEGIN)
	$(NCDC_MAKE_QUIET)$(NCDC_FORT90C) $(NCDC_F90FLAGS) $(NCDC_CONFIG_FLAGS) nc_diag_cat.F90 $(NC_DIAG_CAT_OBJS) $(NCDC_NETCDF_FLAGS) -o nc_diag_cat.x
	$(NCDC_MAKE_FANCY_BIGLIB_END)

test_nc_unlimdims.x: netcdf_unlimdims.mod ncdc_climsg.mod test_nc_unlimdims.F90
	$(NCDC_MAKE_FANCY_BIGLIB_BEGIN)
	$(NCDC_MAKE_QUIET)$(NCDC_FORT90C) $(NCDC_F90FLAGS) $(NCDC_CONFIG_FLAGS) test_nc_unlimdims.F90 ncdc_climsg.o ncdc_state.o netcdf_unlimdims.o $(NCDC_NETCDF_FLAGS) -o test_nc_unlimdims.x
	$(NCDC_MAKE_FANCY_BIGLIB_END)

netcdf_unlimdims.mod: netcdf_unlimdims.F90 Makefile
	$(NCDC_MAKE_FANCY_BEGIN)
	$(NCDC_MAKE_QUIET)$(NCDC_FORT90C) $(NCDC_F90FLAGS) $(NCDC_CONFIG_FLAGS) -c netcdf_unlimdims.F90 $(NCDC_NETCDF_FLAGS)
	$(NCDC_MAKE_FANCY_END)

clean:
	rm -f *.mod *.x *.o
