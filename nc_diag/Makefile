all: test_netcdf_layer.x test_nclayer_allcov.x test_nclayer_stress.x test_utils.x

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

# Coverage build flags
ifndef COVERAGE
    COVERAGE=0
endif

ifeq ($(COVERAGE), 1)
	F90FLAGS+=-prof-gen=srcpos
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
	F90FLAGS+=-O0 -fpp -g -fstack-protector -check all \
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
#   -DNO_NETCDF        - see DISABLE_NETCDF above for details.
#                        
#                        NOTE - this is set automatically if
#                        DISABLE_NETCDF is set.
COMPILE_FLAGS+=-D_REAL8_ -DANSI_TERM_COLORS -DERROR_TRACEBACK -DENABLE_ACTION_MSGS

NETCDF_LAYER_DEPS+=Makefile \
    nclayer_climsg.mod \
    nclayer_chaninfo.mod nclayer_ciresize.mod \
    nclayer_data2d.mod nclayer_dresize.mod \
    nclayer_lheader.mod \
    nclayer_metadata.mod nclayer_mresize.mod \
    nclayer_realloc.mod \
    nclayer_types.mod nclayer_state.mod \
    nclayer_strarrutils.mod \
    nclayer_varattr.mod

NETCDF_LAYER_OBJS+=netcdf_layer.o \
    nclayer_climsg.o \
    nclayer_chaninfo.o nclayer_ciresize.o \
    nclayer_data2d.o nclayer_dresize.o \
    nclayer_lheader.o \
    nclayer_metadata.o nclayer_mresize.o \
    nclayer_realloc.o \
    nclayer_types.o nclayer_state.o \
    nclayer_strarrutils.o \
    nclayer_varattr.o

NETCDF_LAYER_VAR_DEPS+= kinds.mod \
    nclayer_climsg.mod \
    nclayer_types.mod nclayer_state.mod \
    nclayer_realloc.mod \
    nclayer_strarrutils.mod \
    nclayer_varattr.mod \

NETCDF_LAYER_VAR_RESIZE_DEPS+= kinds.mod \
    nclayer_state.mod \
    nclayer_climsg.mod \
    nclayer_realloc.mod

kinds.mod: Makefile kinds.F90
	$(FORT90C) $(F90FLAGS) -c kinds.F90 $(COMPILE_FLAGS)

netcdf_layer.mod: kinds.mod $(NETCDF_LAYER_DEPS) netcdf_layer.F90
	$(FORT90C) $(F90FLAGS) -c netcdf_layer.F90 $(NETCDF_FLAGS) $(COMPILE_FLAGS)

libnetcdf_layer.a: netcdf_layer.mod
	ar rcs libnetcdf_layer.a $(NETCDF_LAYER_OBJS)

nclayer_chaninfo.mod: nclayer_chaninfo.F90 $(NETCDF_LAYER_VAR_DEPS) nclayer_ciresize.mod
	$(FORT90C) $(F90FLAGS) -c nclayer_chaninfo.F90 $(NETCDF_FLAGS) $(COMPILE_FLAGS)

nclayer_ciresize.mod: nclayer_ciresize.F90 $(NETCDF_LAYER_VAR_RESIZE_DEPS)
	$(FORT90C) $(F90FLAGS) -c nclayer_ciresize.F90 $(NETCDF_FLAGS) $(COMPILE_FLAGS)

nclayer_climsg.mod: nclayer_climsg.F90 kinds.mod
	$(FORT90C) $(F90FLAGS) -c nclayer_climsg.F90 $(NETCDF_FLAGS) $(COMPILE_FLAGS)

nclayer_state.mod: nclayer_state.f90 nclayer_types.mod kinds.mod
	$(FORT90C) $(F90FLAGS) -c nclayer_state.f90 $(NETCDF_FLAGS) $(COMPILE_FLAGS)

nclayer_types.mod: nclayer_types.F90 kinds.mod
	$(FORT90C) $(F90FLAGS) -c nclayer_types.F90 $(NETCDF_FLAGS) $(COMPILE_FLAGS)

nclayer_strarrutils.mod: nclayer_strarrutils.F90
	$(FORT90C) $(F90FLAGS) -c nclayer_strarrutils.F90

nclayer_varattr.mod: nclayer_varattr.F90 nclayer_types.mod nclayer_state.mod nclayer_climsg.mod kinds.mod
	$(FORT90C) $(F90FLAGS) -c nclayer_varattr.F90 $(NETCDF_FLAGS) $(COMPILE_FLAGS)

nclayer_metadata.mod: nclayer_metadata.F90 $(NETCDF_LAYER_VAR_DEPS) nclayer_mresize.mod
	$(FORT90C) $(F90FLAGS) -c nclayer_metadata.F90 $(NETCDF_FLAGS) $(COMPILE_FLAGS)

nclayer_mresize.mod: nclayer_mresize.F90 $(NETCDF_LAYER_VAR_RESIZE_DEPS)
	$(FORT90C) $(F90FLAGS) -c nclayer_mresize.F90 $(NETCDF_FLAGS) $(COMPILE_FLAGS)

nclayer_data2d.mod: nclayer_data2d.F90 nclayer_dresize.mod $(NETCDF_LAYER_VAR_DEPS)
	$(FORT90C) $(F90FLAGS) -c nclayer_data2d.F90 $(NETCDF_FLAGS) $(COMPILE_FLAGS)

nclayer_dresize.mod: nclayer_dresize.F90 $(NETCDF_LAYER_VAR_RESIZE_DEPS)
	$(FORT90C) $(F90FLAGS) -c nclayer_dresize.F90 $(NETCDF_FLAGS) $(COMPILE_FLAGS)

nclayer_realloc.mod: nclayer_realloc.F90 nclayer_climsg.mod kinds.mod
	$(FORT90C) $(F90FLAGS) -c nclayer_realloc.F90 $(NETCDF_FLAGS) $(COMPILE_FLAGS)

nclayer_lheader.mod: nclayer_lheader.F90 nclayer_state.mod nclayer_climsg.mod kinds.mod
	$(FORT90C) $(F90FLAGS) -c nclayer_lheader.F90 $(NETCDF_FLAGS) $(COMPILE_FLAGS)

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
	rm -f *.mod *.o

clean:
	rm -f *.mod *.x *.o
	rm -rf pgopti.* CodeCoverage CODE_COVERAGE.HTML *.dyn
