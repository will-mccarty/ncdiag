all: test_netcdf_layer.x test_nclayer_allcov.x test_nclayer_stress.x generic_data.x test_utils.x

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
	netcdf_realloc_decl.f90 \
	netcdf_lheader_decl.f90 netcdf_chaninfo_decl.F90 \
	netcdf_metadata_decl.f90 \
	netcdf_realloc_imp.f90 netcdf_mresize.F90 \
	netcdf_ciresize.F90 netcdf_lheader_imp.F90 \
	netcdf_chaninfo_imp.F90 netcdf_metadata_imp.F90 \
	netcdf_data2d_decl.f90 netcdf_data2d_imp.F90 \
	netcdf_dresize.F90 \
	netcdf_varattr_decl.F90 netcdf_varattr_imp.F90

kinds.mod: Makefile kinds.F90
	$(FORT90C) $(F90FLAGS) -c kinds.F90 $(COMPILE_FLAGS)

netcdf_layer.mod: Makefile kinds.mod utils.mod $(NETCDF_LAYER_DEPS) netcdf_layer.F90
	$(FORT90C) $(F90FLAGS) -c netcdf_layer.F90 $(NETCDF_FLAGS) $(COMPILE_FLAGS)

test_netcdf_layer.x: netcdf_layer.mod test_netcdf_layer.f90
	$(FORT90C) $(F90FLAGS) -o test_netcdf_layer.x test_netcdf_layer.f90 \
		netcdf_layer.o utils.o $(NETCDF_FLAGS) $(COMPILE_FLAGS)

test_nclayer_allcov.x: netcdf_layer.mod test_nclayer_allcov.f90
	$(FORT90C) $(F90FLAGS) -o test_nclayer_allcov.x test_nclayer_allcov.f90 \
		netcdf_layer.o utils.o $(NETCDF_FLAGS) $(COMPILE_FLAGS)

test_nclayer_stress.x: netcdf_layer.mod test_nclayer_stress.f90
	$(FORT90C) $(F90FLAGS) -o test_nclayer_stress.x test_nclayer_stress.f90 \
		netcdf_layer.o utils.o $(NETCDF_FLAGS) $(COMPILE_FLAGS)

generic_data.x: netcdf_layer.mod generic_data.f90
	$(FORT90C) $(F90FLAGS) -o generic_data.x generic_data.f90

test_utils.x: utils.mod test_utils.f90
	$(FORT90C) $(F90FLAGS) -o test_utils.x utils.o test_utils.f90

utils.mod: Makefile utils.f90
	$(FORT90C) $(F90FLAGS) -c utils.f90

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
