        subroutine nc_diag_compress_metadata_pass
            character(len=1000) :: err_string
            
            call info("Scanning NetCDF input file for dimensions and variables...")
            
            call get_command_argument(3, input_file)
            if (len_trim(input_file) <= 0) then
                call usage("Invalid input file name - likely blank!")
            end if
            
            if (trim(input_file) == output_file) then
                call error("Input file can NOT be the same as the output file.")
            else
                call info(" -> Opening " // trim(input_file) // " for reading...")
                call check(nf90_open(input_file, NF90_NOWRITE, ncid_input))
                
                ! Get top level info about the file!
                call check(nf90_inquire(ncid_input, nDimensions = input_ndims, &
                    nVariables = input_nvars))
                
                ! Get unlimited dimension information
                call check(pf_nf90_inq_unlimdims(ncid_input, num_unlims))
                
                allocate(unlim_dims(num_unlims))
                
                call check(pf_nf90_inq_unlimdims(ncid_input, num_unlims, unlim_dims))
                
                ! Loop through each dimension!
                do tmp_dim_index = 1, input_ndims
                    call check(nf90_inquire_dimension(ncid_input, tmp_dim_index, &
                        tmp_dim_name, tmp_dim_size))
                    
                    is_unlim = .FALSE.
                    
                    do i = 1, num_unlims
                        if (tmp_dim_index == unlim_dims(i)) then
                            is_unlim = .TRUE.
                            exit
                        end if
                    end do
                    
                    if (is_unlim) then
                        call nc_diag_compress_metadata_add_dim(tmp_dim_name, -1)
                    else
                        call nc_diag_compress_metadata_add_dim(tmp_dim_name, tmp_dim_size)
                    end if
                end do
                
                ! Variables
                
                ! Loop through each variable!
                do var_index = 1, input_nvars
                    ! Grab number of dimensions and attributes first
                    call check(nf90_inquire_variable(ncid_input, var_index, name = tmp_var_name, ndims = tmp_var_ndims))
                    
                    ! Allocate temporary variable dimids storage!
                    allocate(tmp_var_dimids(tmp_var_ndims))
                    allocate(tmp_var_dim_names(tmp_var_ndims))
                    
                    ! Grab the actual dimension IDs and attributes
                    
                    call check(nf90_inquire_variable(ncid_input, var_index, dimids = tmp_var_dimids, &
                        xtype = tmp_var_type))
                    
                    if ((tmp_var_ndims <= 2) .OR. &
                        ((tmp_var_ndims == 3) .AND. (tmp_var_type == NF90_CHAR))) then
                        do i = 1, tmp_var_ndims
                            call check(nf90_inquire_dimension(ncid_input, tmp_var_dimids(i), tmp_var_dim_names(i)))
                        end do
                        
                        call nc_diag_compress_metadata_add_var(tmp_var_name, tmp_var_type, tmp_var_ndims, tmp_var_dim_names)
                    else
                        write (err_string, "(A, I0, A)") &
                            "Variables with >2 dimensions NOT supported." // &
                            CHAR(10) // "             " // &
                            "(Variable '" // trim(tmp_var_name) // "' has ", &
                            tmp_var_ndims, &
                            " dimensions!)"
                        call error(trim(err_string))
                    end if
                    ! Deallocate
                    deallocate(tmp_var_dimids)
                    deallocate(tmp_var_dim_names)
                end do
                
                call check(nf90_close(ncid_input))
                
                deallocate(unlim_dims)
            end if
        end subroutine nc_diag_compress_metadata_pass
        
        subroutine nc_diag_compress_metadata_define
            integer :: i, j
            
            call info("Copying new dimensions and variables to output file...")
            
            call info(" -> Defining dimensions...")
            do i = 1, dim_arr_total
                if (dim_sizes(i) == -1) then
                    call check(nf90_def_dim(ncid_output, dim_names(i), &
                        NF90_UNLIMITED, dim_output_ids(i)))
                else
                    call check(nf90_def_dim(ncid_output, dim_names(i), &
                        dim_sizes(i), dim_output_ids(i)))
                end if
            end do
            
            call info(" -> Defining variables...")
            do i = 1, var_arr_total
                do j = 1, var_dim_names(i)%num_names
                    var_dim_names(i)%output_dim_ids(j) = &
                        dim_output_ids(nc_diag_compress_lookup_dim(var_dim_names(i)%dim_names(j)))
                end do
                
                call check(nf90_def_var(ncid_output, var_names(i), var_types(i), &
                    var_dim_names(i)%output_dim_ids, &
                    var_output_ids(i)))
                
                if (var_hasunlim(i)) then
                    if (var_dim_names(i)%num_names == 1) then
                        call check(nf90_def_var_chunking(ncid_output, var_output_ids(i), &
                            NF90_CHUNKED, (/ nc_diag_compress_CHUNK_SIZE /) ))
                    else if (var_dim_names(i)%num_names == 2) then
                        call check(nf90_def_var_chunking(ncid_output, var_output_ids(i), &
                            NF90_CHUNKED, (/ dim_sizes(nc_diag_compress_lookup_dim(var_dim_names(i)%dim_names(1))), &
                            nc_diag_compress_CHUNK_SIZE /) ))
                    else if (var_dim_names(i)%num_names == 3) then
                        call check(nf90_def_var_chunking(ncid_output, var_output_ids(i), &
                            NF90_CHUNKED, &
                            (/ dim_sizes(nc_diag_compress_lookup_dim(var_dim_names(i)%dim_names(1))), &
                                dim_sizes(nc_diag_compress_lookup_dim(var_dim_names(i)%dim_names(2))), &
                                nc_diag_compress_CHUNK_SIZE /) ))
                    end if
                else
                    if (var_dim_names(i)%num_names == 1) then
                        call check(nf90_def_var_chunking(ncid_output, var_output_ids(i), &
                            NF90_CHUNKED, (/ dim_sizes(nc_diag_compress_lookup_dim(var_dim_names(i)%dim_names(1))) /) ))
                    else if (var_dim_names(i)%num_names == 2) then
                        call check(nf90_def_var_chunking(ncid_output, var_output_ids(i), &
                            NF90_CHUNKED, (/ dim_sizes(nc_diag_compress_lookup_dim(var_dim_names(i)%dim_names(1))), &
                                dim_sizes(nc_diag_compress_lookup_dim(var_dim_names(i)%dim_names(2))) /) ))
                    else if (var_dim_names(i)%num_names == 3) then
                        call check(nf90_def_var_chunking(ncid_output, var_output_ids(i), &
                            NF90_CHUNKED, &
                            (/ dim_sizes(nc_diag_compress_lookup_dim(var_dim_names(i)%dim_names(1))), &
                                dim_sizes(nc_diag_compress_lookup_dim(var_dim_names(i)%dim_names(2))), &
                                dim_sizes(nc_diag_compress_lookup_dim(var_dim_names(i)%dim_names(3))) /) ))
                    end if
                end if
                
                call check(nf90_def_var_deflate(ncid_output, var_output_ids(i), &
                    shuffle = 1, deflate = 1, deflate_level = nc_diag_compress_GZIP_COMPRESS))
            end do
        end subroutine nc_diag_compress_metadata_define
        
        function nc_diag_compress_lookup_dim(dim_name) result(ind)
            character(len=*), intent(in)    :: dim_name
            integer :: i, ind
            
            ind = -1
            
            if (allocated(dim_names)) then
                do i = 1, dim_arr_total
                    if (dim_names(i) == dim_name) then
                        ind = i
                        exit
                    end if
                end do
            end if
        end function nc_diag_compress_lookup_dim
        
        subroutine nc_diag_compress_metadata_add_dim(dim_name, dim_size)
            character(len=*), intent(in)    :: dim_name
            integer         , intent(in)    :: dim_size
            
            integer                         :: dim_index
            character(len=1000)             :: err_string
            
            dim_index = nc_diag_compress_lookup_dim(dim_name)
            
            ! If we can't find it, it's new! Make sure we have enough
            ! space for it...
            if (dim_index == -1) then
                dim_arr_total = dim_arr_total + 1
                
                if (dim_arr_total >= dim_arr_size) then
                    if (allocated(dim_names)) then
                        call nc_diag_realloc(dim_names, DIM_START_SIZE)
                        call nc_diag_realloc(dim_sizes, DIM_START_SIZE)
                        call nc_diag_realloc(dim_output_ids, DIM_START_SIZE)
                        dim_arr_size = dim_arr_size + DIM_START_SIZE
                    else
                        allocate(dim_names(DIM_START_SIZE))
                        allocate(dim_sizes(DIM_START_SIZE))
                        allocate(dim_output_ids(DIM_START_SIZE))
                        dim_arr_size = DIM_START_SIZE
                    end if
                end if
                
                dim_index = dim_arr_total
                
                ! Add name
                dim_names(dim_index) = dim_name
                dim_sizes(dim_index) = 0
                
                ! Set counter to 0
                dim_output_ids(dim_index) = -1
            end if
            
            if (dim_size /= -1) then
                ! Add/update size
                if ((dim_sizes(dim_index) /= 0) .AND. (dim_size /= dim_sizes(dim_index))) then
                    write (err_string, "(A, I0, A, I0, A)") &
                        "Fixed dimension length changed between files!" // &
                        CHAR(10) // "             " // &
                        "(Fixed dimension '" // trim(dim_name) // "' changed from length ", &
                        dim_sizes(dim_index), &
                        CHAR(10) // "             " // &
                        "to ", &
                        dim_size, &
                        "!)"
                    call error(trim(err_string))
                end if
                dim_sizes(dim_index) = dim_size
            else
                if ((dim_sizes(dim_index) /= -1) .AND. (dim_sizes(dim_index) /= 0)) then
                    write (err_string, "(A, I0, A)") &
                        "Changed from a fixed dimension length to unlimited" // &
                        CHAR(10) // "             " // &
                        "dimension length. (Fixed dimension '" // &
                        trim(dim_name) // &
                        "' had a fixed" // &
                        CHAR(10) // "             " // &
                        "length of ", &
                        dim_sizes(dim_index), &
                        "!)"
                    call error(trim(err_string))
                end if
                
                dim_sizes(dim_index) = -1
            end if
        end subroutine nc_diag_compress_metadata_add_dim
        
        function nc_diag_compress_lookup_var(var_name) result(ind)
            character(len=*), intent(in)    :: var_name
            integer :: i, ind
            
            ind = -1
            
            if (allocated(var_names)) then
                do i = 1, var_arr_total
                    if (var_names(i) == var_name) then
                        ind = i
                        exit
                    end if
                end do
            end if
        end function nc_diag_compress_lookup_var
        
        subroutine nc_diag_compress_metadata_add_var(var_name, var_type, var_ndims, var_dims)
            character(len=*), intent(in)    :: var_name
            integer(i_long) , intent(in)    :: var_type
            integer(i_long) , intent(in)    :: var_ndims
            character(len=*), intent(in)    :: var_dims(:)
            
            integer(i_long)                 :: var_index, i
            character(len=1000)             :: err_string
            
            var_index = nc_diag_compress_lookup_var(trim(var_name))
            
            ! If we can't find it, it's new! Make sure we have enough
            ! space for it...
            if (var_index == -1) then
                var_arr_total = var_arr_total + 1
                
                if (var_arr_total >= var_arr_size) then
                    if (allocated(var_names)) then
                        call nc_diag_realloc(var_names, VAR_START_SIZE)
                        call nc_diag_realloc(var_types, VAR_START_SIZE)
                        call nc_diag_realloc(var_dim_names, VAR_START_SIZE)
                        call nc_diag_realloc(var_output_ids, VAR_START_SIZE)
                        call nc_diag_realloc(var_hasunlim, VAR_START_SIZE)
                    else
                        allocate(var_names(VAR_START_SIZE))
                        allocate(var_types(VAR_START_SIZE))
                        allocate(var_dim_names(VAR_START_SIZE))
                        allocate(var_output_ids(VAR_START_SIZE))
                        allocate(var_hasunlim(VAR_START_SIZE))
                        var_arr_size = VAR_START_SIZE
                    end if
                end if
                
                var_index = var_arr_total
                
                ! Add name
                var_names(var_index) = var_name
                var_types(var_index) = var_type
            end if
            
            if (allocated(var_dim_names(var_index)%dim_names)) then
                ! Just do a sanity check!
                if (var_types(var_index) /= var_type) &
                    call error("Variable type changed!" // &
                        CHAR(10) // "             " // &
                        "(Type of variable '" // trim(var_name) // "' changed from " // &
                        trim(nc_diag_compress_metadata_type_to_str(var_types(var_index))) // &
                        CHAR(10) // "             " // &
                        "to " // &
                        trim(nc_diag_compress_metadata_type_to_str(var_type)) // &
                        "!)")
                
                if (var_dim_names(var_index)%num_names /= var_ndims) then
                    write (err_string, "(A, I0, A, I0, A)") &
                        "Variable ndims changed!" // &
                        CHAR(10) // "             " // &
                        "(Variable '" // trim(var_name) // "' changed ndims from ", &
                        var_dim_names(var_index)%num_names, &
                        CHAR(10) // "             " // &
                        "to ", &
                        var_ndims, &
                        "!)"
                    call error(trim(err_string))
                end if
                
                do i = 1, var_ndims
                    if (var_dim_names(var_index)%dim_names(i) /= var_dims(i)) &
                        call error("Variable dimensions changed!" // &
                        CHAR(10) // "             " // &
                        "(Variable '" // trim(var_name) // "' changed dimension from " // &
                        trim(var_dim_names(var_index)%dim_names(i)) // &
                        CHAR(10) // "             " // &
                        "to " // &
                        trim(var_dims(i)) // &
                        "!)")
                end do
            else
                var_dim_names(var_index)%num_names = var_ndims
                allocate(var_dim_names(var_index)%dim_names(var_ndims))
                allocate(var_dim_names(var_index)%output_dim_ids(var_ndims))
                var_dim_names(var_index)%dim_names(1:var_ndims) = var_dims(1:var_ndims)
                var_hasunlim(var_index) = .FALSE.
                
                do i = 1, var_ndims
                    if (dim_sizes(nc_diag_compress_lookup_dim(var_dim_names(var_index)%dim_names(i))) == -1) then
                        var_hasunlim(var_index) = .TRUE.
                        exit
                    end if
                end do
                
            end if
        end subroutine nc_diag_compress_metadata_add_var
        
        function nc_diag_compress_metadata_type_to_str(var_type) result(nc_str)
            integer(i_long)   :: var_type
            character(len=11) :: nc_str
            
            nc_str = "(invalid)"
            
            if (var_type == NF90_BYTE)   nc_str = "NF90_BYTE"
            if (var_type == NF90_SHORT)  nc_str = "NF90_SHORT"
            if (var_type == NF90_INT)    nc_str = "NF90_INT (LONG)"
            if (var_type == NF90_FLOAT)  nc_str = "NF90_FLOAT"
            if (var_type == NF90_DOUBLE) nc_str = "NF90_DOUBLE"
            if (var_type == NF90_CHAR)   nc_str = "NF90_CHAR"
        end function nc_diag_compress_metadata_type_to_str
