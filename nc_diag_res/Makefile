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
	@printf "\033[1m\033[33m ** Building $< => $@|.o...\033[0m\n"
	@$(NCDRES_FORT90C) $(NCDRES_F90FLAGS) -c kinds.F90 $(NCDRES_CONFIG_FLAGS)
	@printf "\033[F\r\033[32m ** Building $< => $@|.o... done!\033[0m\n"

tests:
	make -C tests

check:
	make -C tests check

libnc_diag_res.a: nc_diag_res.o
	@printf "\033[1m\033[33m ** Creating static library: $@\033[0m\n"
	@ar rcs libnc_diag_res.a $(NC_DIAG_READ_OBJS)
	@printf "\033[F\r\033[32m ** Creating static library: $@ - done!\033[0m\n"

nc_diag_res.o: nc_diag_res.f90 fson/lib/libfson.a kinds.mod $(NC_DIAG_READ_DEPS)
	@printf "\033[1m\033[33m ** Building $< => $@|.o...\033[0m\n"
	@$(NCDRES_FORT90C) $(NCDRES_F90FLAGS) $(NCDRES_CONFIG_FLAGS) \
		-c nc_diag_res.f90 \
		-Ifson/lib -Lfson/lib -lfson
	@printf "\033[F\r\033[32m ** Building $< => $@|.o... done!\033[0m\n"

ncdres_climsg.mod: ncdres_climsg.F90 kinds.mod
	@printf "\033[1m\033[33m ** Building $< => $@|.o...\033[0m\n"
	@$(NCDRES_FORT90C) $(NCDRES_F90FLAGS) $(NCDRES_CONFIG_FLAGS) \
		-c ncdres_climsg.F90
	@printf "\033[F\r\033[32m ** Building $< => $@|.o... done!\033[0m\n"

fson/lib/libfson.a: fson/Makefile fson/src/*
	@printf "\033[33m ** Building FSON library...\033[0m\n"
	make -C fson lib
	@printf "\033[32m ** FSON library built successfully!\033[0m\n"

clean:
	rm -f *.mod *.x *.o *.a
	make -C fson clean clobber
	make -C tests clean
