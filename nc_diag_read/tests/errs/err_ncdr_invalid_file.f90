program err_ncdr_invalid_file
    use nc_diag_read
    use netcdf
    
    call nc_diag_read_init("this_file_does_not_exist!.nc")
end program err_ncdr_invalid_file
