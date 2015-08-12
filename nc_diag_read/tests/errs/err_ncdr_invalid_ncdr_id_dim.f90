program err_ncdr_invalid_ncdr_id_dim
    use nc_diag_read
    use netcdf
    
    integer(i_long) :: i
    
    !------------------------------------------------------------------
    ! Subroutine allocation method testing
    !------------------------------------------------------------------
    
    i = nc_diag_read_get_dim(1234567, "asdf")
end program err_ncdr_invalid_ncdr_id_dim
