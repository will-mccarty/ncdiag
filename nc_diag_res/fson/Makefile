# This file has been set up to build with nc_diag_res.
# Do NOT change this file!
include ../Makefile.config

SRC=src
DIST=lib
BUILD=build

F95=.f90
OBJ=.o
SO=.so
MOD=.mod
EXE=

BUILD_SHARED=0
BUILD_STATIC=1

LIBTARGET=$(DIST)/libfson$(SO)
LIBTARGET_STATIC=$(DIST)/libfson.a

ifeq ($(BUILD_SHARED), 1)
    BUILD_LIBS += $(LIBTARGET)
endif

ifeq ($(BUILD_STATIC), 1)
    BUILD_LIBS += $(LIBTARGET_STATIC)
endif

FC = ifort
FCFLAGS = $(NCDRES_F90FLAGS)
LDFLAGS=

# "make" builds all
all: lib

JSON = $(shell find src -name '*.json')
json: $(patsubst $(SRC)%, $(DIST)%, $(JSON))

FSON = fson_string_m fson_value_m fson_path_m fson
OBJECTS = $(patsubst %, $(BUILD)/%$(OBJ), $(FSON))

$(LIBTARGET) : $(OBJECTS)
	$(NCDRES_MAKE_FANCY_BIG_BEGIN)
	$(NCDRES_MAKE_QUIET)mkdir -p `dirname $@`
	$(NCDRES_MAKE_QUIET)$(FC) -shared -o $(LIBTARGET) $^
	$(NCDRES_MAKE_FANCY_BIG_END)

$(LIBTARGET_STATIC) : $(OBJECTS)
	$(NCDRES_MAKE_FANCY_BIG_BEGIN)
	$(NCDRES_MAKE_QUIET)mkdir -p `dirname $@`
	$(NCDRES_MAKE_QUIET)ar rcs $(LIBTARGET_STATIC) $^
	$(NCDRES_MAKE_FANCY_BIG_END)

lib: $(BUILD_LIBS)

$(DIST)/%.json : $(SRC)/%.json
	mkdir -p $(@D)
	cp -f $< $@

# build fson objects
$(BUILD)/%$(OBJ): $(SRC)/%$(F95)
	$(NCDRES_MAKE_FANCY_BEGIN)
	$(NCDRES_MAKE_QUIET)mkdir -p `dirname $@`
	$(NCDRES_MAKE_QUIET)$(FC) $(FCFLAGS) -fpic -module $(DIST) -c $< -o $@
	$(NCDRES_MAKE_FANCY_END)

clean:
	rm -rf $(BUILD)
	rm -rf *$(MOD)
	rm -rf 

clobber: clean
	rm -rf $(DIST)/*
