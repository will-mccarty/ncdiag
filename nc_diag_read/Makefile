# Include build configuration:
include Makefile.config

# Debug setup
ifndef NCDR_DEBUG
	NCDR_DEBUG=0
endif

# Debug flags
ifeq ($(NCDR_DEBUG), 1)
	NCDR_NCDR_F90FLAGS+=-O0 -fpp -g -pg -fstack-protector -check all \
		-ftrapuv -debug all -fpe0 -warn all -traceback -debug extended
endif

NC_DIAG_READ_DEPS+=Makefile \
	ncdr_climsg.mod \
	ncdr_state.mod ncdr_types.mod \
	netcdf_unlimdims.mod \
	ncdr_realloc_mod.mod \
	ncdr_vars.mod ncdr_dims.mod

NC_DIAG_READ_OBJS+= \
	nc_diag_read.o \
	ncdr_climsg.o \
	ncdr_state.o ncdr_types.o \
	netcdf_unlimdims.o \
	ncdr_realloc_mod.o \
	ncdr_dims.o ncdr_check.o \
	ncdr_vars.o \
	ncdr_alloc_assert.o \
	ncdr_vars_fetch.o

.PHONY: all clean tests check

all: libnc_diag_read.a nc_diag_read.mod tests

kinds.mod: Makefile kinds.F90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) -c kinds.F90 $(NCDR_CONFIG_FLAGS)

tests:
	make -C tests

check:
	make -C tests check

libnc_diag_read.a: nc_diag_read.mod
	ar rcs libnc_diag_read.a $(NC_DIAG_READ_OBJS)

nc_diag_read.mod: kinds.mod $(NC_DIAG_READ_DEPS) nc_diag_read.F90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c nc_diag_read.F90 \
		$(NCDR_NETCDF_FLAGS)

ncdr_climsg.mod: kinds.mod ncdr_climsg.F90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c ncdr_climsg.F90

ncdr_types.mod: kinds.mod ncdr_types.f90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c ncdr_types.f90

ncdr_state.mod: kinds.mod ncdr_types.mod ncdr_state.f90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c ncdr_state.f90

ncdr_vars.mod: kinds.mod ncdr_state.mod ncdr_types.mod ncdr_check.mod ncdr_vars_fetch.mod ncdr_vars.f90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c ncdr_vars.f90 \
		$(NCDR_NETCDF_FLAGS)

ncdr_vars_fetch.mod: kinds.mod ncdr_state.mod ncdr_types.mod ncdr_check.mod ncdr_alloc_assert.mod ncdr_vars_fetch.f90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c ncdr_vars_fetch.f90 \
		$(NCDR_NETCDF_FLAGS)

ncdr_alloc_assert.mod: kinds.mod ncdr_check.mod ncdr_alloc_assert.f90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c ncdr_alloc_assert.f90 \
		$(NCDR_NETCDF_FLAGS)

ncdr_dims.mod: kinds.mod ncdr_state.mod ncdr_types.mod ncdr_check.mod netcdf_unlimdims.mod ncdr_dims.f90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c ncdr_dims.f90 \
		$(NCDR_NETCDF_FLAGS)

ncdr_check.mod: kinds.mod ncdr_types.mod ncdr_state.mod ncdr_check.f90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c ncdr_check.f90 \
		$(NCDR_NETCDF_FLAGS)

ncdr_realloc_mod.mod: kinds.mod ncdr_types.mod ncdr_realloc_mod.F90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c ncdr_realloc_mod.F90

netcdf_unlimdims.mod: Makefile netcdf_unlimdims.F90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) $(NCDR_CONFIG_FLAGS) \
		-c netcdf_unlimdims.F90 \
		$(NCDR_NETCDF_FLAGS)

utils.mod: Makefile utils.f90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) \
		-c utils.f90

clean:
	rm -f *.mod *.x *.o
	make -C tests clean
