all: test_netcdf_layer.x test_nclayer_allcov.x test_nclayer_stress.x test_utils.x

FORT90C=ifort

# Coverage build flags
ifndef COVERAGE
    COVERAGE=0
endif

ifeq ($(COVERAGE), 1)
	F90FLAGS+=-prof-gen=srcpos
endif

F90FLAGS+=-O0 -fpp -g -fstack-protector -check all \
	-ftrapuv -debug all -fpe0 -warn all -traceback -debug extended
NETCDF_FLAGS+=-I $(BASEDIR)/Linux/include/netcdf -L$(BASEDIR)/Linux/lib \
	-lnetcdff -lnetcdf -lhdf5 -lcurl -lz -lrt -lmpi -lhdf5_hl -lhdf5 \
	-lz -lsz -lgpfs -lmfhdf -ldf -ljpeg

# Available compile flags:
#   -D_REAL8_            - define the default "real" number
#                          (other kinds defines available!)
#   -D_DEBUG_MEM_        - enable verbose memory debugging messages.
#                          WARNING - this is a LOT of messages!
#   -DANSI_TERM_COLORS   - enable terminal colors for warning and error
#                          messages.
#   -DERROR_TRACEBACK    - enable a traceback on error. This is useful
#                          for debugging any issues with code or
#                          NCLayer. This works by triggering a
#                          divide-by-zero runtime error, which in turn
#                          generates a traceback.
#   -DENABLE_ACTION_MSGS - enable messages that show the nc_diag API
#                          calls during execution.
COMPILE_FLAGS+=-D_REAL8_ -DANSI_TERM_COLORS -DERROR_TRACEBACK -DENABLE_ACTION_MSGS

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

kinds.o: Makefile kinds.F90
	$(FORT90C) $(F90FLAGS) -c kinds.F90 $(COMPILE_FLAGS)

netcdf_layer.o: kinds.o $(NETCDF_LAYER_DEPS) netcdf_layer.F90
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) -c netcdf_layer.F90 $(NETCDF_FLAGS)

libnetcdf_layer.a: netcdf_layer.o
	ar rcs libnetcdf_layer.a netcdf_layer.o $(NETCDF_LAYER_OBJS)

nclayer_chaninfo.o: nclayer_chaninfo.F90 $(NETCDF_LAYER_VAR_DEPS) nclayer_ciresize.o
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) -c nclayer_chaninfo.F90 $(NETCDF_FLAGS)

nclayer_ciresize.o: nclayer_ciresize.F90 $(NETCDF_LAYER_VAR_RESIZE_DEPS)
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) -c nclayer_ciresize.F90 $(NETCDF_FLAGS)

nclayer_climsg.o: nclayer_climsg.F90 kinds.o
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) -c nclayer_climsg.F90 $(NETCDF_FLAGS)

nclayer_state.o: nclayer_state.f90 nclayer_types.o kinds.o
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) -c nclayer_state.f90 $(NETCDF_FLAGS)

nclayer_types.o: nclayer_types.F90 kinds.o
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) -c nclayer_types.F90 $(NETCDF_FLAGS)

nclayer_strarrutils.o: nclayer_strarrutils.F90
	$(FORT90C) $(F90FLAGS) -c nclayer_strarrutils.F90

nclayer_varattr.o: nclayer_varattr.F90 $(NETCDF_LAYER_VAR_RESIZE_DEPS)
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) -c nclayer_varattr.F90 $(NETCDF_FLAGS)

nclayer_metadata.o: nclayer_metadata.F90 $(NETCDF_LAYER_VAR_DEPS) nclayer_mresize.o
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) -c nclayer_metadata.F90 $(NETCDF_FLAGS)

nclayer_mresize.o: nclayer_mresize.F90 $(NETCDF_LAYER_VAR_RESIZE_DEPS)
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) -c nclayer_mresize.F90 $(NETCDF_FLAGS)

nclayer_data2d.o: nclayer_data2d.F90 nclayer_dresize.o $(NETCDF_LAYER_VAR_DEPS)
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) -c nclayer_data2d.F90 $(NETCDF_FLAGS)

nclayer_dresize.o: nclayer_dresize.F90 $(NETCDF_LAYER_VAR_RESIZE_DEPS)
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) -c nclayer_dresize.F90 $(NETCDF_FLAGS)

nclayer_realloc.o: nclayer_realloc.F90 nclayer_climsg.o kinds.o
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) -c nclayer_realloc.F90 $(NETCDF_FLAGS)

nclayer_lheader.o: nclayer_lheader.F90 nclayer_state.o nclayer_climsg.o kinds.o
	$(FORT90C) $(F90FLAGS) $(COMPILE_FLAGS) -c nclayer_lheader.F90 $(NETCDF_FLAGS)

test_netcdf_layer.x: libnetcdf_layer.a test_netcdf_layer.f90
	$(FORT90C) $(F90FLAGS) -o test_netcdf_layer.x test_netcdf_layer.f90 \
		-L. -lnetcdf_layer $(NETCDF_FLAGS) $(COMPILE_FLAGS)

test_nclayer_allcov.x: libnetcdf_layer.a test_nclayer_allcov.f90
	$(FORT90C) $(F90FLAGS) -o test_nclayer_allcov.x test_nclayer_allcov.f90 \
		-L. -lnetcdf_layer $(NETCDF_FLAGS) $(COMPILE_FLAGS)

test_nclayer_stress.x: libnetcdf_layer.a test_nclayer_stress.f90
	$(FORT90C) $(F90FLAGS) -o test_nclayer_stress.x test_nclayer_stress.f90 \
		-L. -lnetcdf_layer $(NETCDF_FLAGS) $(COMPILE_FLAGS)

generic_data.x: libnetcdf_layer.a generic_data.f90
	$(FORT90C) $(F90FLAGS) -o generic_data.x generic_data.f90

test_utils.x: libnetcdf_layer.a test_utils.f90
	$(FORT90C) $(F90FLAGS) -o test_utils.x test_utils.f90 -L. -lnetcdf_layer

check: test_netcdf_layer.x test_nclayer_allcov.x test_nclayer_stress.x
	./test_netcdf_layer.x
	./test_nclayer_allcov.x
	./test_nclayer_stress.x

precoverage:
	@[ ! "$$COVERAGE" = "1" ] && \
		echo "ERROR: COVERAGE must be set to 1 to set build flags and use" && \
		echo "       use coverage tools." && exit 1 || true
	@[ ! -f ~/launch_browser ] && \
		echo "ERROR: You must have a script called launch_browser in your" && \
		echo "       home directory to display code coverage results. It" && \
		echo "       should accept a file as an argument to display the" && \
		echo "       code coverage result page." && exit 1 || true

coverage: precoverage clean check
	profmerge
	codecov -srcroot ..
	~/launch_browser CODE_COVERAGE.HTML &
	rm -f *.o *.o

clean:
	rm -f *.mod *.x *.o
	rm -rf pgopti.* CodeCoverage CODE_COVERAGE.HTML *.dyn
