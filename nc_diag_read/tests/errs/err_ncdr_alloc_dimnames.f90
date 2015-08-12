program err_ncdr_alloc_dimnames
    use nc_diag_read
    use netcdf
    
    integer(i_long) :: ndims, ndims_len
    character(len=:), dimension(:), allocatable :: dim_names
    
    !------------------------------------------------------------------
    ! Subroutine allocation method testing
    !------------------------------------------------------------------
    
    call nc_diag_read_init("test.nc")
    
    allocate(character(len=10) :: dim_names(10))
    call nc_diag_read_get_dim_names(ndims, ndims_len, dim_names)
end program err_ncdr_alloc_dimnames
