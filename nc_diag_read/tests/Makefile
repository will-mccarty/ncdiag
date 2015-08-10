include ../Makefile.config
include ../Makefile.libconfig

NC_DIAG_READ_TESTS=test_ncdr_get.x test_ncdr_get_arda.x \
	test_ncdr_get_ardg.x test_ncdr_get_ardag.x \
	test_ncdr_get_id.x test_ncdr_get_id_arda.x \
	test_ncdr_get_id_ardg.x test_ncdr_get_id_ardag.x \
	test_ncdr_get_pushpop.x \
	test_ncdr_dims.x \
	test_ncdr_vars.x

# Coverage build flags
ifndef COVERAGE
	COVERAGE=0
endif

ifeq ($(COVERAGE), 1)
	NCDR_F90FLAGS+=-prof-gen=srcpos
	STATIC_COMPILE=1
endif

# Static compilation flags
ifndef STATIC_COMPILE
	STATIC_COMPILE=0
endif

ifeq ($(STATIC_COMPILE), 1)
	NC_DIAG_READ_FLAGS=-I.. ../nc_diag_read.F90 ../ncdr_*.f90 ../ncdr_*.F90 ../netcdf_unlimdims.F90
endif

.PHONY: all clean check precoverage coverage

all: $(NC_DIAG_READ_TESTS)

check: $(NC_DIAG_READ_TESTS)
	@$(foreach ncdr_test,$(NC_DIAG_READ_TESTS),echo "Running: $(ncdr_test)";./$(ncdr_test) || exit 1;echo "Pass: $(ncdr_test)";)

test_ncdr_dims.x: $(NCDR_LIB_PATH) test_ncdr_dims.f90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS)\
		test_ncdr_dims.f90 -o test_ncdr_dims.x \
		$(NC_DIAG_READ_FLAGS) \
		$(NCDR_NETCDF_FLAGS)

test_ncdr_vars.x: $(NCDR_LIB_PATH) test_ncdr_vars.f90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) \
		test_ncdr_vars.f90 -o test_ncdr_vars.x \
		$(NC_DIAG_READ_FLAGS) \
		$(NCDR_NETCDF_FLAGS)

test_ncdr_get.x: $(NCDR_LIB_PATH) test_ncdr_get.f90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) \
		test_ncdr_get.f90 -o test_ncdr_get.x \
		$(NC_DIAG_READ_FLAGS) \
		$(NCDR_NETCDF_FLAGS)

test_ncdr_get_pushpop.x: $(NCDR_LIB_PATH) test_ncdr_get_pushpop.f90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) \
		test_ncdr_get_pushpop.f90 -o test_ncdr_get_pushpop.x \
		$(NC_DIAG_READ_FLAGS) \
		$(NCDR_NETCDF_FLAGS)

test_ncdr_get_arda.x: $(NCDR_LIB_PATH) test_ncdr_get_arda.f90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) \
		test_ncdr_get_arda.f90 -o test_ncdr_get_arda.x \
		$(NC_DIAG_READ_FLAGS) \
		$(NCDR_NETCDF_FLAGS)

test_ncdr_get_ardg.x: $(NCDR_LIB_PATH) test_ncdr_get_ardg.f90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) \
		test_ncdr_get_ardg.f90 -o test_ncdr_get_ardg.x \
		$(NC_DIAG_READ_FLAGS) \
		$(NCDR_NETCDF_FLAGS)

test_ncdr_get_ardag.x: $(NCDR_LIB_PATH) test_ncdr_get_ardag.f90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) \
		test_ncdr_get_ardag.f90 -o test_ncdr_get_ardag.x \
		$(NC_DIAG_READ_FLAGS) \
		$(NCDR_NETCDF_FLAGS)

test_ncdr_get_id.x: $(NCDR_LIB_PATH) test_ncdr_get_id.f90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) \
		test_ncdr_get_id.f90 -o test_ncdr_get_id.x \
		$(NC_DIAG_READ_FLAGS) \
		$(NCDR_NETCDF_FLAGS)

test_ncdr_get_id_arda.x: $(NCDR_LIB_PATH) test_ncdr_get_id_arda.f90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) \
		test_ncdr_get_id_arda.f90 -o test_ncdr_get_id_arda.x \
		$(NC_DIAG_READ_FLAGS) \
		$(NCDR_NETCDF_FLAGS)

test_ncdr_get_id_ardg.x: $(NCDR_LIB_PATH) test_ncdr_get_id_ardg.f90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) \
		test_ncdr_get_id_ardg.f90 -o test_ncdr_get_id_ardg.x \
		$(NC_DIAG_READ_FLAGS) \
		$(NCDR_NETCDF_FLAGS)

test_ncdr_get_id_ardag.x: $(NCDR_LIB_PATH) test_ncdr_get_id_ardag.f90
	$(NCDR_FORT90C) $(NCDR_F90FLAGS) \
		test_ncdr_get_id_ardag.f90 -o test_ncdr_get_id_ardag.x \
		$(NC_DIAG_READ_FLAGS) \
		$(NCDR_NETCDF_FLAGS)

$(NCDR_LIB_PATH): $(NCDR_LIB_DEPS)
	@echo " *** nc_diag_read library not built yet (or outdated)."
	@echo " *** (Re-)building it now..."
	$(NCDR_BUILD_CMD)

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

clean:
	rm -f *.mod *.x *.o
	rm -rf pgopti.* CodeCoverage CODE_COVERAGE.HTML *.dyn
