# Include build configuration
include Makefile.config

NC_DIAG_WRITE_OBJS+= \
    ncdw_climsg.o \
    ncdw_chaninfo.o ncdw_ciresize.o \
    ncdw_data2d.o ncdw_dresize.o \
    ncdw_lheader.o \
    ncdw_metadata.o ncdw_mresize.o \
    ncdw_realloc.o \
    ncdw_types.o ncdw_state.o \
    ncdw_strarrutils.o \
    ncdw_varattr.o

NC_DIAG_WRITE_DEPS+=Makefile \
    $(NC_DIAG_WRITE_OBJS)

NCDW_VAR_DEPS+= kinds.o \
    ncdw_climsg.o \
    ncdw_types.o ncdw_state.o \
    ncdw_realloc.o \
    ncdw_strarrutils.o \
    ncdw_varattr.o

NCDW_VAR_RESIZE_DEPS+= kinds.o \
    ncdw_state.o \
    ncdw_types.o \
    ncdw_climsg.o \
    ncdw_realloc.o

.PHONY: all tests check clean

all: libnc_diag_write.a tests

tests:
	make -C tests

check:
	make -C tests check

libnc_diag_write.a: nc_diag_write_mod.o $(NC_DIAG_WRITE_DEPS)
	ar rcs libnc_diag_write.a nc_diag_write_mod.o $(NC_DIAG_WRITE_OBJS)

kinds.o: kinds.F90 Makefile Makefile.config Makefile.libconfig
	$(NCDW_FORT90C) $(NCDW_F90FLAGS) -c kinds.F90 $(NCDW_CONFIG_FLAGS)

nc_diag_write_mod.o: kinds.o $(NC_DIAG_WRITE_DEPS) nc_diag_write_mod.F90
	$(NCDW_FORT90C) $(NCDW_F90FLAGS) $(NCDW_CONFIG_FLAGS) -c nc_diag_write_mod.F90 $(NCDW_NETCDF_FLAGS)

ncdw_chaninfo.o: ncdw_chaninfo.F90 $(NCDW_VAR_DEPS) ncdw_ciresize.o
	$(NCDW_FORT90C) $(NCDW_F90FLAGS) $(NCDW_CONFIG_FLAGS) -c ncdw_chaninfo.F90 $(NCDW_NETCDF_FLAGS)

ncdw_ciresize.o: ncdw_ciresize.F90 $(NCDW_VAR_RESIZE_DEPS)
	$(NCDW_FORT90C) $(NCDW_F90FLAGS) $(NCDW_CONFIG_FLAGS) -c ncdw_ciresize.F90 $(NCDW_NETCDF_FLAGS)

ncdw_climsg.o: ncdw_climsg.F90 kinds.o
	$(NCDW_FORT90C) $(NCDW_F90FLAGS) $(NCDW_CONFIG_FLAGS) -c ncdw_climsg.F90 $(NCDW_NETCDF_FLAGS)

ncdw_state.o: ncdw_state.f90 ncdw_types.o kinds.o
	$(NCDW_FORT90C) $(NCDW_F90FLAGS) $(NCDW_CONFIG_FLAGS) -c ncdw_state.f90 $(NCDW_NETCDF_FLAGS)

ncdw_types.o: ncdw_types.F90 kinds.o
	$(NCDW_FORT90C) $(NCDW_F90FLAGS) $(NCDW_CONFIG_FLAGS) -c ncdw_types.F90 $(NCDW_NETCDF_FLAGS)

ncdw_strarrutils.o: ncdw_strarrutils.F90
	$(NCDW_FORT90C) $(NCDW_F90FLAGS) -c ncdw_strarrutils.F90

ncdw_varattr.o: ncdw_varattr.F90 $(NCDW_VAR_RESIZE_DEPS)
	$(NCDW_FORT90C) $(NCDW_F90FLAGS) $(NCDW_CONFIG_FLAGS) -c ncdw_varattr.F90 $(NCDW_NETCDF_FLAGS)

ncdw_metadata.o: ncdw_metadata.F90 $(NCDW_VAR_DEPS) ncdw_mresize.o
	$(NCDW_FORT90C) $(NCDW_F90FLAGS) $(NCDW_CONFIG_FLAGS) -c ncdw_metadata.F90 $(NCDW_NETCDF_FLAGS)

ncdw_mresize.o: ncdw_mresize.F90 $(NCDW_VAR_RESIZE_DEPS)
	$(NCDW_FORT90C) $(NCDW_F90FLAGS) $(NCDW_CONFIG_FLAGS) -c ncdw_mresize.F90 $(NCDW_NETCDF_FLAGS)

ncdw_data2d.o: ncdw_data2d.F90 ncdw_dresize.o $(NCDW_VAR_DEPS)
	$(NCDW_FORT90C) $(NCDW_F90FLAGS) $(NCDW_CONFIG_FLAGS) -c ncdw_data2d.F90 $(NCDW_NETCDF_FLAGS)

ncdw_dresize.o: ncdw_dresize.F90 $(NCDW_VAR_RESIZE_DEPS)
	$(NCDW_FORT90C) $(NCDW_F90FLAGS) $(NCDW_CONFIG_FLAGS) -c ncdw_dresize.F90 $(NCDW_NETCDF_FLAGS)

ncdw_realloc.o: ncdw_realloc.F90 ncdw_climsg.o kinds.o
	$(NCDW_FORT90C) $(NCDW_F90FLAGS) $(NCDW_CONFIG_FLAGS) -c ncdw_realloc.F90 $(NCDW_NETCDF_FLAGS)

ncdw_lheader.o: ncdw_lheader.F90 ncdw_state.o ncdw_climsg.o kinds.o
	$(NCDW_FORT90C) $(NCDW_F90FLAGS) $(NCDW_CONFIG_FLAGS) -c ncdw_lheader.F90 $(NCDW_NETCDF_FLAGS)

clean:
	rm -f *.mod *.x *.o *.a gmon.out
	make -C tests clean
