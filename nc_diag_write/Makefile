# Include build configuration
include Makefile.config

NETCDF_LAYER_OBJS+= \
    nclayer_climsg.o \
    nclayer_chaninfo.o nclayer_ciresize.o \
    nclayer_data2d.o nclayer_dresize.o \
    nclayer_lheader.o \
    nclayer_metadata.o nclayer_mresize.o \
    nclayer_realloc.o \
    nclayer_types.o nclayer_state.o \
    nclayer_strarrutils.o \
    nclayer_varattr.o

NETCDF_LAYER_DEPS+=Makefile \
    $(NETCDF_LAYER_OBJS)

NETCDF_LAYER_VAR_DEPS+= kinds.o \
    nclayer_climsg.o \
    nclayer_types.o nclayer_state.o \
    nclayer_realloc.o \
    nclayer_strarrutils.o \
    nclayer_varattr.o

NETCDF_LAYER_VAR_RESIZE_DEPS+= kinds.o \
    nclayer_state.o \
    nclayer_types.o \
    nclayer_climsg.o \
    nclayer_realloc.o

.PHONY: all tests check clean

all: libnetcdf_layer.a tests

tests:
	make -C tests

check:
	make -C tests check

libnetcdf_layer.a: netcdf_layer.o
	ar rcs libnetcdf_layer.a netcdf_layer.o $(NETCDF_LAYER_OBJS)

kinds.o: kinds.F90 Makefile Makefile.config Makefile.libconfig
	$(NCLAYER_FORT90C) $(NCLAYER_F90FLAGS) -c kinds.F90 $(NCLAYER_CONFIG_FLAGS)

netcdf_layer.o: kinds.o $(NETCDF_LAYER_DEPS) netcdf_layer.F90
	$(NCLAYER_FORT90C) $(NCLAYER_F90FLAGS) $(NCLAYER_CONFIG_FLAGS) -c netcdf_layer.F90 $(NCLAYER_NETCDF_FLAGS)

nclayer_chaninfo.o: nclayer_chaninfo.F90 $(NETCDF_LAYER_VAR_DEPS) nclayer_ciresize.o
	$(NCLAYER_FORT90C) $(NCLAYER_F90FLAGS) $(NCLAYER_CONFIG_FLAGS) -c nclayer_chaninfo.F90 $(NCLAYER_NETCDF_FLAGS)

nclayer_ciresize.o: nclayer_ciresize.F90 $(NETCDF_LAYER_VAR_RESIZE_DEPS)
	$(NCLAYER_FORT90C) $(NCLAYER_F90FLAGS) $(NCLAYER_CONFIG_FLAGS) -c nclayer_ciresize.F90 $(NCLAYER_NETCDF_FLAGS)

nclayer_climsg.o: nclayer_climsg.F90 kinds.o
	$(NCLAYER_FORT90C) $(NCLAYER_F90FLAGS) $(NCLAYER_CONFIG_FLAGS) -c nclayer_climsg.F90 $(NCLAYER_NETCDF_FLAGS)

nclayer_state.o: nclayer_state.f90 nclayer_types.o kinds.o
	$(NCLAYER_FORT90C) $(NCLAYER_F90FLAGS) $(NCLAYER_CONFIG_FLAGS) -c nclayer_state.f90 $(NCLAYER_NETCDF_FLAGS)

nclayer_types.o: nclayer_types.F90 kinds.o
	$(NCLAYER_FORT90C) $(NCLAYER_F90FLAGS) $(NCLAYER_CONFIG_FLAGS) -c nclayer_types.F90 $(NCLAYER_NETCDF_FLAGS)

nclayer_strarrutils.o: nclayer_strarrutils.F90
	$(NCLAYER_FORT90C) $(NCLAYER_F90FLAGS) -c nclayer_strarrutils.F90

nclayer_varattr.o: nclayer_varattr.F90 $(NETCDF_LAYER_VAR_RESIZE_DEPS)
	$(NCLAYER_FORT90C) $(NCLAYER_F90FLAGS) $(NCLAYER_CONFIG_FLAGS) -c nclayer_varattr.F90 $(NCLAYER_NETCDF_FLAGS)

nclayer_metadata.o: nclayer_metadata.F90 $(NETCDF_LAYER_VAR_DEPS) nclayer_mresize.o
	$(NCLAYER_FORT90C) $(NCLAYER_F90FLAGS) $(NCLAYER_CONFIG_FLAGS) -c nclayer_metadata.F90 $(NCLAYER_NETCDF_FLAGS)

nclayer_mresize.o: nclayer_mresize.F90 $(NETCDF_LAYER_VAR_RESIZE_DEPS)
	$(NCLAYER_FORT90C) $(NCLAYER_F90FLAGS) $(NCLAYER_CONFIG_FLAGS) -c nclayer_mresize.F90 $(NCLAYER_NETCDF_FLAGS)

nclayer_data2d.o: nclayer_data2d.F90 nclayer_dresize.o $(NETCDF_LAYER_VAR_DEPS)
	$(NCLAYER_FORT90C) $(NCLAYER_F90FLAGS) $(NCLAYER_CONFIG_FLAGS) -c nclayer_data2d.F90 $(NCLAYER_NETCDF_FLAGS)

nclayer_dresize.o: nclayer_dresize.F90 $(NETCDF_LAYER_VAR_RESIZE_DEPS)
	$(NCLAYER_FORT90C) $(NCLAYER_F90FLAGS) $(NCLAYER_CONFIG_FLAGS) -c nclayer_dresize.F90 $(NCLAYER_NETCDF_FLAGS)

nclayer_realloc.o: nclayer_realloc.F90 nclayer_climsg.o kinds.o
	$(NCLAYER_FORT90C) $(NCLAYER_F90FLAGS) $(NCLAYER_CONFIG_FLAGS) -c nclayer_realloc.F90 $(NCLAYER_NETCDF_FLAGS)

nclayer_lheader.o: nclayer_lheader.F90 nclayer_state.o nclayer_climsg.o kinds.o
	$(NCLAYER_FORT90C) $(NCLAYER_F90FLAGS) $(NCLAYER_CONFIG_FLAGS) -c nclayer_lheader.F90 $(NCLAYER_NETCDF_FLAGS)

clean:
	rm -f *.mod *.x *.o *.a gmon.out
	make -C tests clean
