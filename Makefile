NC_DIAG_LIBS+= \
    nc_diag_write/libnc_diag_write.a \
    nc_diag_read/libnc_diag_read.a \
    nc_diag_res/libnc_diag_res.a

NC_DIAG_EXECS+= \
    nc_diag_cat/nc_diag_cat.x

all: $(NC_DIAG_LIBS) $(NC_DIAG_EXECS)

libnc_diag.a: $(NC_DIAG_LIBS)
	rm -rf tmp_nc_libs
	mkdir -p tmp_nc_libs
	cd tmp_nc_libs && $(foreach nc_lib_ar,$(NC_DIAG_LIBS),ar x ../$(nc_lib_ar);)
	ar rcs libnc_diag.a tmp_nc_libs/*.o
	rm -rf tmp_nc_libs

nc_diag_write/libnc_diag_write.a:
	make -C nc_diag_write libnc_diag_write.a

nc_diag_cat/nc_diag_cat.x:
	make -C nc_diag_cat nc_diag_cat.x

nc_diag_read/libnc_diag_read.a:
	make -C nc_diag_read libnc_diag_read.a

nc_diag_res/libnc_diag_res.a:
	make -C nc_diag_res libnc_diag_res.a

clean:
	make -C nc_diag_write clean
	make -C nc_diag_cat clean
	make -C nc_diag_read clean
	make -C nc_diag_res clean
