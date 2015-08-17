# Include build configuration:
include Makefile.config

NC_DIAG_READ_DEPS+=Makefile \
    ncdr_climsg.o \
    ncdr_state.o ncdr_types.o \
    netcdf_unlimdims.o \
    ncdr_realloc_mod.o \
    ncdr_vars.o ncdr_dims.o ncdr_attrs.o ncdr_global_attrs.o

NC_DIAG_READ_OBJS+= \
    kinds.o \
    ncdr_climsg.o \
    ncdr_state.o ncdr_types.o \
    netcdf_unlimdims.o \
    ncdr_realloc_mod.o \
    ncdr_dims.o ncdr_check.o \
    ncdr_vars.o \
    ncdr_attrs.o \
    ncdr_global_attrs.o \
    ncdr_alloc_assert.o \
    ncdr_vars_fetch.o \
    ncdr_attrs_fetch.o \
    ncdr_global_attrs_fetch.o

.PHONY: all clean tests check

all: libnc_diag_read.a nc_diag_read.o tests

tests:
	make -C tests

check:
	make -C tests check

libnc_diag_read.a: nc_diag_read.o $(NC_DIAG_READ_DEPS)
	ar rcs libnc_diag_read.a $(NC_DIAG_READ_OBJS)

kinds.o: Makefile Makefile.config Makefile.libconfig kinds.F90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) -c kinds.F90 $(NCDR_CONFIG_FLAGS)

nc_diag_read.o: kinds.o $(NC_DIAG_READ_DEPS) nc_diag_read.F90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c nc_diag_read.F90 \
		$(NCDR_NETCDF_FLAGS)

ncdr_climsg.o: ncdr_climsg.F90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c ncdr_climsg.F90

ncdr_types.o: ncdr_types.f90 kinds.o
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c ncdr_types.f90

ncdr_state.o: ncdr_state.f90 kinds.o ncdr_types.o
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c ncdr_state.f90

ncdr_vars.o: ncdr_vars.f90 kinds.o ncdr_state.o ncdr_climsg.o ncdr_check.o ncdr_alloc_assert.o
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c ncdr_vars.f90 \
		$(NCDR_NETCDF_FLAGS)

ncdr_vars_fetch.o: ncdr_vars_fetch.f90 kinds.o ncdr_state.o ncdr_check.o ncdr_alloc_assert.o
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c ncdr_vars_fetch.f90 \
		$(NCDR_NETCDF_FLAGS)

ncdr_attrs.o: ncdr_attrs.f90 kinds.o ncdr_state.o ncdr_climsg.o ncdr_check.o ncdr_alloc_assert.o
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c ncdr_attrs.f90 \
		$(NCDR_NETCDF_FLAGS)

ncdr_attrs_fetch.o: ncdr_attrs_fetch.f90 kinds.o ncdr_state.o ncdr_check.o ncdr_alloc_assert.o
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c ncdr_attrs_fetch.f90 \
		$(NCDR_NETCDF_FLAGS)

ncdr_global_attrs.o: ncdr_global_attrs.f90 kinds.o ncdr_state.o ncdr_climsg.o ncdr_check.o
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c ncdr_global_attrs.f90 \
		$(NCDR_NETCDF_FLAGS)

ncdr_global_attrs_fetch.o: ncdr_global_attrs_fetch.f90 kinds.o ncdr_state.o ncdr_check.o ncdr_alloc_assert.o
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c ncdr_global_attrs_fetch.f90 \
		$(NCDR_NETCDF_FLAGS)

ncdr_alloc_assert.o: ncdr_alloc_assert.f90 kinds.o ncdr_state.o ncdr_climsg.o ncdr_check.o
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c ncdr_alloc_assert.f90 \
		$(NCDR_NETCDF_FLAGS)

ncdr_dims.o: ncdr_dims.f90 kinds.o ncdr_state.o ncdr_climsg.o ncdr_check.o netcdf_unlimdims.o
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c ncdr_dims.f90 \
		$(NCDR_NETCDF_FLAGS)

ncdr_check.o: ncdr_check.f90 kinds.o ncdr_climsg.o ncdr_state.o
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c ncdr_check.f90 \
		$(NCDR_NETCDF_FLAGS)

ncdr_realloc_mod.o: ncdr_realloc_mod.F90 kinds.o ncdr_types.o
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c ncdr_realloc_mod.F90

netcdf_unlimdims.o: netcdf_unlimdims.F90 Makefile
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c netcdf_unlimdims.F90 \
		$(NCDR_NETCDF_FLAGS)

clean:
	rm -f *.mod *.x *.o *.a
	make -C tests clean
