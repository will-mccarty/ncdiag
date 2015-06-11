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
    do i = 1, 10000000
        call nc_diag_header("headertestsimple", 123)
    end do
    
    print *, "===================="
    print *, "Vector:"
    print *, "===================="
    
    do i = 1, 10000000
        call nc_diag_header("headertestarr1", (/ 123, 234, 345, 456, 567, 678, 789 /))
    end do
    
    call nc_diag_header("headertestarr2", (/ 222, 234, 345, 456, 567, 678, 789 /))
    call nc_diag_header("headertestarr3", (/ 333, 234, 345, 456, 567, 678, 789 /))
    call nc_diag_header("headertestarr4", (/ 444, 234, 345, 456, 567, 678, 789 /))
    call nc_diag_header("headertestarr5", (/ 111, 222, 333, 444, 555, 666, 777, 888, 999 /))
    call nc_diag_header("headertestarr6", (/ 999, 777, 555, 333, 111 /))
    call nc_diag_header("headertestsimple2", 123)
    call nc_diag_header("headertestsimple3", 321)
    call nc_diag_header("headertestarr7", (/ 111, 222, 333, 444, 555, 666, 777, 888, 999 /))
    call nc_diag_header("headertestarr7", (/ 222, 444, 666, 888 /))
    call nc_diag_header("headertestarr7", 999)
    
    print *, "==============================="
    print *, "Writing resulting NetCDF file:"
    print *, "==============================="
    
    call nc_diag_write
end program test_netcdf_layer
