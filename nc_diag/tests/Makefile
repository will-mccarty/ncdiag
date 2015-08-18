include ../Makefile.config
include ../Makefile.libconfig

NC_DIAG_TESTS=test_netcdf_layer.x \
    test_nclayer_allcov.x \
    test_nclayer_stress.x \
    test_utils.x

# Coverage build flags
ifndef COVERAGE
    COVERAGE=0
endif

ifeq ($(COVERAGE), 1)
    NCLAYER_F90FLAGS+=-prof-gen=srcpos
    STATIC_COMPILE=1
endif

# Static compilation flags
ifndef STATIC_COMPILE
    STATIC_COMPILE=0
endif

ifeq ($(STATIC_COMPILE), 1)
    NCLAYER_FLAGS=-I.. ../netcdf_layer.F90 ../nclayer_*.f90 ../nclayer_*.F90
endif

.PHONY: all clean check precoverage coverage

all: $(NC_DIAG_TESTS)

check: $(NC_DIAG_TESTS)
	@echo "Running regular tests..."
	@$(foreach ncdr_test,$(NC_DIAG_TESTS),echo "Running: $(ncdr_test)";./$(ncdr_test) || exit 1;echo "Pass: $(ncdr_test)";)

test_netcdf_layer.x: test_netcdf_layer.f90 $(NCLAYER_LIB_PATH)
	$(NCLAYER_MAKE_FANCY_BEGIN)
	$(NCLAYER_MAKE_QUIET)$(NCLAYER_FORT90C) $(NCLAYER_F90FLAGS)\
		test_netcdf_layer.f90 -o test_netcdf_layer.x \
		$(NCLAYER_FLAGS) \
		$(NCLAYER_NETCDF_FLAGS)
	$(NCLAYER_MAKE_FANCY_END)

test_nclayer_allcov.x: test_nclayer_allcov.f90 $(NCLAYER_LIB_PATH)
	$(NCLAYER_MAKE_FANCY_BEGIN)
	$(NCLAYER_MAKE_QUIET)$(NCLAYER_FORT90C) $(NCLAYER_F90FLAGS)\
		test_nclayer_allcov.f90 -o test_nclayer_allcov.x \
		$(NCLAYER_FLAGS) \
		$(NCLAYER_NETCDF_FLAGS)
	$(NCLAYER_MAKE_FANCY_END)

test_nclayer_stress.x: test_nclayer_stress.f90 $(NCLAYER_LIB_PATH)
	$(NCLAYER_MAKE_FANCY_BEGIN)
	$(NCLAYER_MAKE_QUIET)$(NCLAYER_FORT90C) $(NCLAYER_F90FLAGS)\
		test_nclayer_stress.f90 -o test_nclayer_stress.x \
		$(NCLAYER_FLAGS) \
		$(NCLAYER_NETCDF_FLAGS)
	$(NCLAYER_MAKE_FANCY_END)

test_utils.x: test_utils.f90 $(NCLAYER_LIB_PATH)
	$(NCLAYER_MAKE_FANCY_BEGIN)
	$(NCLAYER_MAKE_QUIET)$(NCLAYER_FORT90C) $(NCLAYER_F90FLAGS)\
		test_utils.f90 -o test_utils.x \
		$(NCLAYER_FLAGS) \
		$(NCLAYER_NETCDF_FLAGS)
	$(NCLAYER_MAKE_FANCY_END)

$(NCLAYER_LIB_PATH): $(NCLAYER_LIB_DEPS)
	@echo " *** nc_diag library not built yet (or outdated)."
	@echo " *** (Re-)building it now..."
	$(NCLAYER_BUILD_CMD)

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
	rm -f *.mod *.x *.o *.nc gmon.out
	rm -rf pgopti.* CodeCoverage CODE_COVERAGE.HTML *.dyn
