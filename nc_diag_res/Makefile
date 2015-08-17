# Include build configuration:
include Makefile.config

FSON_DEPS+=fson/Makefile \
    fson/src/fson.f90 \
    fson/src/fson_path_m.f90 \
    fson/src/fson_string_m.f90 \
    fson/src/fson_value_m.f90

NC_DIAG_READ_DEPS+=Makefile \
    ncdres_climsg.o

NC_DIAG_READ_OBJS+= \
    nc_diag_res.o \
    ncdres_climsg.o

.PHONY: all clean tests check

all: libnc_diag_res.a nc_diag_res.mod tests

tests:
	make -C tests

check:
	make -C tests check

libnc_diag_res.a: $(NC_DIAG_READ_OBJS)
	$(NCDRES_MAKE_FANCY_BIG_BEGIN)
	$(NCDRES_MAKE_QUIET)ar rcs libnc_diag_res.a $(NC_DIAG_READ_OBJS)
	$(NCDRES_MAKE_FANCY_BIG_END)

nc_diag_res.o: nc_diag_res.f90 fson/lib/libfson.a $(NC_DIAG_READ_DEPS)
	$(NCDRES_MAKE_FANCY_BEGIN)
	$(NCDRES_MAKE_QUIET)$(NCDRES_FORT90C) $(NCDRES_F90FLAGS) $(NCDRES_CONFIG_FLAGS) \
		-c nc_diag_res.f90 \
		-Ifson/lib -Lfson/lib -lfson
	$(NCDRES_MAKE_FANCY_END)

ncdres_climsg.o: ncdres_climsg.F90
	$(NCDRES_MAKE_FANCY_BEGIN)
	$(NCDRES_MAKE_QUIET)$(NCDRES_FORT90C) $(NCDRES_F90FLAGS) $(NCDRES_CONFIG_FLAGS) \
		-c ncdres_climsg.F90
	$(NCDRES_MAKE_FANCY_END)

fson/lib/libfson.a: $(FSON_DEPS)
	make -C fson lib

clean:
	rm -f *.mod *.x *.o *.a
	make -C fson clean clobber
	make -C tests clean
