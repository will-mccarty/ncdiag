all: test_netcdf_write test_netcdf_read

test_netcdf_write: test_netcdf_write.f90
	ifort test_netcdf_write.f90 -o test_netcdf -I $(BASEDIR)/Linux/include/netcdf -L$(BASEDIR)/Linux/lib -lnetcdff -lnetcdf -lhdf5 -lcurl -lz -lrt -lmpi -lhdf5_hl -lhdf5 -lz -lsz -lgpfs -lmfhdf -ldf -ljpeg

test_netcdf_read: test_netcdf_read.f90
	ifort test_netcdf_read.f90 -o test_netcdf_read -I $(BASEDIR)/Linux/include/netcdf -L$(BASEDIR)/Linux/lib -lnetcdff -lnetcdf -lhdf5 -lcurl -lz -lrt -lmpi -lhdf5_hl -lhdf5 -lz -lsz -lgpfs -lmfhdf -ldf -ljpeg

clean:
	rm -f test_netcdf_read test_netcdf_write
