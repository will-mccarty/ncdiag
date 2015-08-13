# Include build configuration:
include Makefile.config

NC_DIAG_READ_DEPS+=Makefile \
    ncdres_climsg.mod

NC_DIAG_READ_OBJS+= \
    nc_diag_res.o \
    ncdres_climsg.o

.PHONY: all clean tests check

all: libnc_diag_res.a nc_diag_res.mod tests

kinds.mod: kinds.F90 Makefile Makefile.config Makefile.libconfig
	$(NCDRES_MAKE_FANCY_BEGIN)
	$(NCDRES_MAKE_QUIET)$(NCDRES_FORT90C) $(NCDRES_F90FLAGS) -c kinds.F90 $(NCDRES_CONFIG_FLAGS)
	$(NCDRES_MAKE_FANCY_END)

tests:
	make -C tests

check:
	make -C tests check

libnc_diag_res.a: $(NC_DIAG_READ_OBJS)
	$(NCDRES_MAKE_FANCY_BIGLIB_BEGIN)
	$(NCDRES_MAKE_QUIET)ar rcs libnc_diag_res.a $(NC_DIAG_READ_OBJS)
	$(NCDRES_MAKE_FANCY_BIGLIB_END)

nc_diag_res.o: nc_diag_res.f90 fson/lib/libfson.a kinds.mod $(NC_DIAG_READ_DEPS)
	$(NCDRES_MAKE_FANCY_BEGIN)
	$(NCDRES_MAKE_QUIET)$(NCDRES_FORT90C) $(NCDRES_F90FLAGS) $(NCDRES_CONFIG_FLAGS) \
		-c nc_diag_res.f90 \
		-Ifson/lib -Lfson/lib -lfson
	$(NCDRES_MAKE_FANCY_END)

ncdres_climsg.mod: ncdres_climsg.F90 kinds.mod
	$(NCDRES_MAKE_FANCY_BEGIN)
	$(NCDRES_MAKE_QUIET)$(NCDRES_FORT90C) $(NCDRES_F90FLAGS) $(NCDRES_CONFIG_FLAGS) \
		-c ncdres_climsg.F90
	$(NCDRES_MAKE_FANCY_END)

fson/lib/libfson.a: fson/Makefile fson/src/*
	$(NCDRES_MAKE_FANCY_BIGLIB_BEGIN)
	$(NCDRES_MAKE_QUIET)make -C fson lib
	$(NCDRES_MAKE_FANCY_BIGLIB_END)

clean:
	rm -f *.mod *.x *.o *.a
	make -C fson clean clobber
	make -C tests clean
