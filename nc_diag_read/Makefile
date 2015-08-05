FORT90C=ifort

F90FLAGS+=-O0 -fpp -g -pg -fstack-protector -check all \
	-ftrapuv -debug all -fpe0 -warn all -traceback -debug extended
NETCDF_FLAGS+=-I $(BASEDIR)/Linux/include/netcdf -L$(BASEDIR)/Linux/lib \
	-lnetcdff -lnetcdf -lhdf5 -lcurl -lz -lrt -lmpi -lhdf5_hl -lhdf5 \
	-lz -lsz -lgpfs -lmfhdf -ldf -ljpeg


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

all: nc_diag_read.mod tests

kinds.mod: Makefile kinds.F90
	$(FORT90C) $(F90FLAGS) -c kinds.F90 $(COMPILE_FLAGS)

tests:
	make -C tests

check:
	make -C tests check

nc_diag_read.mod: kinds.mod $(NC_DIAG_READ_DEPS) nc_diag_read.F90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) \
		-c nc_diag_read.F90 \
		$(NETCDF_FLAGS)

ncdr_climsg.mod: kinds.mod ncdr_climsg.F90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) \
		-c ncdr_climsg.F90

ncdr_types.mod: kinds.mod ncdr_types.f90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) \
		-c ncdr_types.f90

ncdr_state.mod: kinds.mod ncdr_types.mod ncdr_state.f90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) \
		-c ncdr_state.f90

ncdr_vars.mod: kinds.mod ncdr_state.mod ncdr_types.mod ncdr_check.mod ncdr_vars_fetch.mod ncdr_vars.f90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) \
		-c ncdr_vars.f90 \
		$(NETCDF_FLAGS)

ncdr_vars_fetch.mod: kinds.mod ncdr_state.mod ncdr_types.mod ncdr_check.mod ncdr_alloc_assert.mod ncdr_vars_fetch.f90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) \
		-c ncdr_vars_fetch.f90 \
		$(NETCDF_FLAGS)

ncdr_alloc_assert.mod: kinds.mod ncdr_check.mod ncdr_alloc_assert.f90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) \
		-c ncdr_alloc_assert.f90 \
		$(NETCDF_FLAGS)

ncdr_dims.mod: kinds.mod ncdr_state.mod ncdr_types.mod ncdr_check.mod netcdf_unlimdims.mod ncdr_dims.f90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) \
		-c ncdr_dims.f90 \
		$(NETCDF_FLAGS)

ncdr_check.mod: kinds.mod ncdr_types.mod ncdr_state.mod ncdr_check.f90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) \
		-c ncdr_check.f90 \
		$(NETCDF_FLAGS)

ncdr_realloc_mod.mod: kinds.mod ncdr_types.mod ncdr_realloc_mod.F90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) \
		-c ncdr_realloc_mod.F90

netcdf_unlimdims.mod: Makefile netcdf_unlimdims.F90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) \
		-c netcdf_unlimdims.F90 \
		$(NETCDF_FLAGS)

utils.mod: Makefile utils.f90
	$(FORT90C) $(F90FLAGS) \
		-c utils.f90

clean:
	rm -f *.mod *.x *.o
	make -C tests clean
