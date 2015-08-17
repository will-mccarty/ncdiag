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
	mkdir -p `dirname $@`
	$(FC) -shared -o $(LIBTARGET) $^

$(LIBTARGET_STATIC) : $(OBJECTS)
	mkdir -p `dirname $@`
	ar rcs $(LIBTARGET_STATIC) $^

lib: $(BUILD_LIBS)

$(DIST)/%.json : $(SRC)/%.json
	mkdir -p $(@D)
	cp -f $< $@

# build fson objects
$(BUILD)/%$(OBJ): $(SRC)/%$(F95)
	mkdir -p `dirname $@`
	$(FC) $(FCFLAGS) -fpic -module $(DIST) -c $< -o $@

clean:
	rm -rf $(BUILD)
	rm -rf *$(MOD)
	rm -rf 

clobber: clean
	rm -rf $(DIST)/*
