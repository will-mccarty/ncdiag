program test_netcdf_layer
    use kinds
    use netcdf
    use fnv32mod
    use netcdf_layer
    implicit none
    
    integer :: i
    
    call nc_diag_init("test.nc")
    
    do i = 1, 1000
        call nc_diag_header("test", 123)
    end do
end program test_netcdf_layer
