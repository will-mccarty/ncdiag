program test_netcdf_layer
    use kinds
    use netcdf
    use fnv32mod
    use netcdf_layer
    implicit none
    
    integer :: i
    
    call nc_diag_init("test.nc")
    
    print *, "===================="
    print *, "Single:"
    print *, "===================="
    !call nc_diag_header("test1", (/ 123, 234, 345, 456, 567, 678, 789 /))
    !call nc_diag_header("test2", (/ 123, 234, 345, 456, 567, 678, 789 /))
    ! 100,000,000
    do i = 1, 100000000
        call nc_diag_header("test", 123)
    end do
    
    print *, "===================="
    print *, "Vector:"
    print *, "===================="
    
    do i = 1, 100000000
        call nc_diag_header("test", (/ 123, 234, 345, 456, 567, 678, 789 /))
    end do
    
    print *, "==============================="
    print *, "Writing resulting NetCDF file:"
    print *, "==============================="
    
    call nc_diag_write
end program test_netcdf_layer
