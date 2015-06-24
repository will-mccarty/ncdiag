program test_netcdf_layer
    use kinds
    use netcdf
    use fnv32mod
    use netcdf_layer
    implicit none
    
    integer :: i
    real(r_single) :: f
    real(r_double) :: d
    
    character(len=100) :: str_header
    character(len=100) :: str_chaninfo
    character(len=100) :: str_metadata
    
    f = 1.234
    d = 2.34567890
    
    call nc_diag_init("test.nc")
    
#ifndef IGNORE_VERSION
    write (*, "(A, L)") "Is NetCDF layer string handling broken? (T for yes, F for no): ", NLAYER_STRING_BROKEN
#endif
    
    print *, "===================="
    print *, "Single:"
    print *, "===================="
    !call nc_diag_header("test1", (/ 123, 234, 345, 456, 567, 678, 789 /))
    !call nc_diag_header("test2", (/ 123, 234, 345, 456, 567, 678, 789 /))
    ! 100,000,000
    call nc_diag_chaninfo_dim_set(10)
    
    do i = 1, 10
        call nc_diag_chaninfo("chaninfosimple1", i)
        call nc_diag_chaninfo("chaninfosimple2", i*2)
        call nc_diag_chaninfo("chaninfosimple4_float", f + 1.00)
        call nc_diag_chaninfo("chaninfosimple5_double", d + 1.00)
        
        call nc_diag_metadata("metadatasimple1", i)
        call nc_diag_metadata("metadatasimple2", i*2)
        call nc_diag_metadata("metadatasimple4_float", f + 1.00 + i)
        call nc_diag_metadata("metadatasimple4_float2", f + 2.00 + i)
        call nc_diag_metadata("metadatasimple5_double", d + 1.00 + i)
        
        write(str_chaninfo, "(A, I0)") "ci6_", i
        call nc_diag_chaninfo("chaninfosimple6_str", str_chaninfo)
        
        write(str_metadata, "(A, I0)") "hellometa_", i
        call nc_diag_metadata("metadatasimple6_str", str_metadata)
    end do
    
    do i = 1, 9
        write(str_chaninfo, "(A, I0)") "ci_strings_", i
        call nc_diag_chaninfo("chaninfosimple7_str", str_chaninfo)
    end do
    
    !print *, "str_chaninfo:"
    !print *, str_chaninfo
    
    do i = 1, 9
        call nc_diag_chaninfo("chaninfosimple3_notcomplete", i*3)
    end do
    
    !do i = 1, 10000000
    do i = 1, 10000
        call nc_diag_header("headertestsimple", 123)
        
        call nc_diag_header("headertestsimple2_float", f)
        call nc_diag_header("headertestsimple3_double", d)
        
        write(str_header, "(A, I0)") "header_", i
        call nc_diag_header("headertestsimple4_str", str_header)
        
        call nc_diag_metadata("metadatasimple7_big", i*2)
    end do
    
    do i = 1, 100
        write(str_metadata, "(A, I0)") "morehellometa_", i
        call nc_diag_metadata("metadatasimple8_str", str_metadata)
    end do
    
    call nc_diag_header("headertestsimple5_str", "hello world")
    
    print *, "str_header:"
    print *, str_header
    
    print *, "===================="
    print *, "Vector:"
    print *, "===================="
    
    do i = 1, 1000
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
    
    call nc_diag_header("headertestarr8", (/ f, f*2, f*3, f*4 /))
    call nc_diag_header("headertestarr9", (/ d, d*2, d*3, d*4 /))
    
    ! nc_diag_header does not support arrays of strings... because
    ! NetCDF4 doesn't support it, either!
    ! (At least, that's what I remember from the docs... double check
    ! to make sure!)
    
    print *, "==============================="
    print *, "Writing resulting NetCDF file:"
    print *, "==============================="
    
    call nc_diag_write
end program test_netcdf_layer
