FORT90C=ifort

F90FLAGS+=-O0 -fpp -g -pg -fstack-protector -check all \
	-ftrapuv -debug all -fpe0 -warn all -traceback -debug extended \
	-I.. -L..
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
	../ncdr_climsg.mod \
	../ncdr_state.mod ../ncdr_types.mod \
	../netcdf_unlimdims.mod \
	../ncdr_realloc_mod.mod \
	../ncdr_vars.mod ../ncdr_dims.mod

NC_DIAG_READ_OBJS+= \
	../nc_diag_read.o \
	../ncdr_climsg.o \
	../ncdr_state.o ../ncdr_types.o \
	../netcdf_unlimdims.o \
	../ncdr_realloc_mod.o \
	../ncdr_dims.o ../ncdr_check.o \
	../ncdr_vars.o \
	../ncdr_alloc_assert.o \
	../ncdr_vars_fetch.o

all: test_ncdr_get.x test_ncdr_get_arda.x \
	test_ncdr_get_ardg.x test_ncdr_get_ardag.x \
	test_ncdr_get_id.x test_ncdr_get_id_arda.x \
	test_ncdr_get_id_ardg.x test_ncdr_get_id_ardag.x \
	test_ncdr_get_pushpop.x

test_ncdr_get.x: ../nc_diag_read.mod test_ncdr_get.f90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) \
		test_ncdr_get.f90 $(NC_DIAG_READ_OBJS) -o test_ncdr_get.x \
		$(NETCDF_FLAGS)

test_ncdr_get_pushpop.x: ../nc_diag_read.mod test_ncdr_get_pushpop.f90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) \
		test_ncdr_get_pushpop.f90 $(NC_DIAG_READ_OBJS) -o test_ncdr_get_pushpop.x \
		$(NETCDF_FLAGS)

test_ncdr_get_arda.x: ../nc_diag_read.mod test_ncdr_get_arda.f90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) \
		test_ncdr_get_arda.f90 $(NC_DIAG_READ_OBJS) -o test_ncdr_get_arda.x \
		$(NETCDF_FLAGS)

test_ncdr_get_ardg.x: ../nc_diag_read.mod test_ncdr_get_ardg.f90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) \
		test_ncdr_get_ardg.f90 $(NC_DIAG_READ_OBJS) -o test_ncdr_get_ardg.x \
		$(NETCDF_FLAGS)

test_ncdr_get_ardag.x: ../nc_diag_read.mod test_ncdr_get_ardag.f90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) \
		test_ncdr_get_ardag.f90 $(NC_DIAG_READ_OBJS) -o test_ncdr_get_ardag.x \
		$(NETCDF_FLAGS)

test_ncdr_get_id.x: ../nc_diag_read.mod test_ncdr_get_id.f90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) \
		test_ncdr_get_id.f90 $(NC_DIAG_READ_OBJS) -o test_ncdr_get_id.x \
		$(NETCDF_FLAGS)

test_ncdr_get_id_arda.x: ../nc_diag_read.mod test_ncdr_get_id_arda.f90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) \
		test_ncdr_get_id_arda.f90 $(NC_DIAG_READ_OBJS) -o test_ncdr_get_id_arda.x \
		$(NETCDF_FLAGS)

test_ncdr_get_id_ardg.x: ../nc_diag_read.mod test_ncdr_get_id_ardg.f90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) \
		test_ncdr_get_id_ardg.f90 $(NC_DIAG_READ_OBJS) -o test_ncdr_get_id_ardg.x \
		$(NETCDF_FLAGS)

test_ncdr_get_id_ardag.x: ../nc_diag_read.mod test_ncdr_get_id_ardag.f90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) \
		test_ncdr_get_id_ardag.f90 $(NC_DIAG_READ_OBJS) -o test_ncdr_get_id_ardag.x \
		$(NETCDF_FLAGS)

../nc_diag_read.mod: ../nc_diag_read.F90 ../ncdr_*.F90 ../ncdr_*.f90
	@echo " *** nc_diag_read library not built yet, building it now..."
	make -C .. nc_diag_read.mod

clean:
	rm -f *.mod *.x *.o
