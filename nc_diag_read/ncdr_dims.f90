module ncdr_dims
    use kinds
    use ncdr_climsg
    use ncdr_types
    use ncdr_state
    use ncdr_check
    use netcdf
    use netcdf_unlimdims
    implicit none
    
    interface nc_diag_read_lookup_dim
        module procedure nc_diag_read_id_lookup_dim, &
            nc_diag_read_noid_lookup_dim
    end interface nc_diag_read_lookup_dim
    
    interface nc_diag_read_assert_dim
        module procedure nc_diag_read_id_assert_dim, &
            nc_diag_read_noid_assert_dim
    end interface nc_diag_read_assert_dim
    
    interface nc_diag_read_check_dim
        module procedure nc_diag_read_id_check_dim, &
            nc_diag_read_noid_check_dim
    end interface nc_diag_read_check_dim
    
    interface nc_diag_read_get_dim
        module procedure nc_diag_read_id_get_dim, &
            nc_diag_read_noid_get_dim
    end interface nc_diag_read_get_dim
    
    interface nc_diag_read_check_dim_unlim
        module procedure nc_diag_read_id_check_dim_unlim, &
            nc_diag_read_noid_check_dim_unlim
    end interface nc_diag_read_check_dim_unlim
    
    contains
        subroutine nc_diag_read_parse_file_dims(file_ncid, file_index, num_dims)
            integer(i_long), intent(in)                :: file_ncid
            integer(i_long), intent(in)                :: file_index
            integer(i_long), intent(in)                :: num_dims
            
            integer(i_long), dimension(:), allocatable :: unlim_dims
            integer(i_long)                            :: num_unlims
            integer(i_long)                            :: i, j
            
            character(len=NF90_MAX_NAME)               :: dim_name
            
            ncdr_files(file_index)%ndims = num_dims
            allocate(ncdr_files(file_index)%dims(num_dims))
            
            ! Get unlimited dimension information
            call check(pf_nf90_inq_unlimdims(file_ncid, num_unlims))
            
            allocate(unlim_dims(num_unlims))
            
            call check(pf_nf90_inq_unlimdims(file_ncid, num_unlims, unlim_dims))
            
            do i = 1, num_dims
                ncdr_files(file_index)%dims(i)%dim_id = i
                
                call check(nf90_inquire_dimension(file_ncid, i, &
                        dim_name, &
                        ncdr_files(file_index)%dims(i)%dim_size))
                
                ncdr_files(file_index)%dims(i)%dim_name = trim(dim_name)
                ncdr_files(file_index)%dims(i)%dim_unlim = .FALSE.
                
                do j = 1, num_unlims
                    if (i == unlim_dims(j)) then
                        ncdr_files(file_index)%dims(i)%dim_unlim = .TRUE.
                        exit
                    end if
                end do
            end do
        end subroutine nc_diag_read_parse_file_dims
        
        function nc_diag_read_id_lookup_dim(file_ncid, dim_name) result(dim_index)
            integer, intent(in)            :: file_ncid
            character(len=*), intent(in)   :: dim_name
            
            integer                        :: dim_index
            
            call ncdr_check_ncid(file_ncid)
            
            do dim_index = 1, ncdr_files(file_ncid)%ndims
                if (ncdr_files(file_ncid)%dims(dim_index)%dim_name == dim_name) &
                    return
            end do
            
            ! Otherwise, return -1!
            dim_index = -1
        end function nc_diag_read_id_lookup_dim
        
        function nc_diag_read_noid_lookup_dim(dim_name) result(dim_index)
            character(len=*), intent(in)   :: dim_name
            
            integer                        :: dim_index
            
            call ncdr_check_current_ncid
            
            dim_index = nc_diag_read_id_lookup_dim(current_ncid, dim_name)
        end function nc_diag_read_noid_lookup_dim
        
        function nc_diag_read_id_assert_dim(file_ncid, dim_name) result(dim_index)
            integer, intent(in)            :: file_ncid
            character(len=*), intent(in)   :: dim_name
            
            integer                        :: dim_index
            
            call ncdr_check_ncid(file_ncid)
            
            ! Otherwise, return -1!
            dim_index = nc_diag_read_id_lookup_dim(file_ncid, dim_name)
            
            ! ...except don't, since we're asserting!
            if (dim_index == -1) &
                call error("The specified dimension '" // dim_name // "' does not exist!")
        end function nc_diag_read_id_assert_dim
        
        function nc_diag_read_noid_assert_dim(dim_name) result(dim_index)
            character(len=*), intent(in)   :: dim_name
            
            integer                        :: dim_index
            
            call ncdr_check_current_ncid
            
            dim_index = nc_diag_read_id_assert_dim(current_ncid, dim_name)
        end function nc_diag_read_noid_assert_dim
        
        function nc_diag_read_id_check_dim(file_ncid, dim_name) result(dim_exists)
            integer, intent(in)            :: file_ncid
            character(len=*), intent(in)   :: dim_name
            
            logical                        :: dim_exists
            
            call ncdr_check_ncid(file_ncid)
            
            if (nc_diag_read_id_lookup_dim(file_ncid, dim_name) == -1) then
                dim_exists = .FALSE.
                return
            end if
            
            dim_exists = .TRUE.
        end function nc_diag_read_id_check_dim
        
        function nc_diag_read_noid_check_dim(dim_name) result(dim_exists)
            character(len=*), intent(in)   :: dim_name
            
            logical                        :: dim_exists
            
            call ncdr_check_current_ncid
            
            if (nc_diag_read_lookup_dim(dim_name) == -1) then
                dim_exists = .FALSE.
                return
            end if
            
            dim_exists = .TRUE.
        end function nc_diag_read_noid_check_dim
        
        function nc_diag_read_id_get_dim(file_ncid, dim_name) result(dim_size)
            integer, intent(in)            :: file_ncid
            character(len=*), intent(in)   :: dim_name
            
            integer(i_long)                :: dim_index, dim_size
            
            call ncdr_check_ncid(file_ncid)
            
            dim_index = nc_diag_read_id_assert_dim(file_ncid, dim_name)
            
            dim_size = ncdr_files(file_ncid)%dims(dim_index)%dim_size
        end function nc_diag_read_id_get_dim
        
        function nc_diag_read_noid_get_dim(dim_name) result(dim_size)
            character(len=*), intent(in)   :: dim_name
            
            integer(i_long)                :: dim_size
            
            call ncdr_check_current_ncid
            
            dim_size = nc_diag_read_id_get_dim(current_ncid, dim_name)
        end function nc_diag_read_noid_get_dim
        
        function nc_diag_read_id_check_dim_unlim(file_ncid, dim_name) result(dim_isunlim)
            integer, intent(in)            :: file_ncid
            character(len=*), intent(in)   :: dim_name
            
            integer(i_long)                :: dim_index
            logical                        :: dim_isunlim
            
            call ncdr_check_ncid(file_ncid)
            
            dim_index = nc_diag_read_id_assert_dim(file_ncid, dim_name)
            
            dim_isunlim = ncdr_files(file_ncid)%dims(dim_index)%dim_unlim
        end function nc_diag_read_id_check_dim_unlim
        
        function nc_diag_read_noid_check_dim_unlim(dim_name) result(dim_isunlim)
            character(len=*), intent(in)   :: dim_name
            
            logical                        :: dim_isunlim
            
            call ncdr_check_current_ncid
            
            dim_isunlim = nc_diag_read_id_check_dim_unlim(current_ncid, dim_name)
        end function nc_diag_read_noid_check_dim_unlim
end module ncdr_dims
