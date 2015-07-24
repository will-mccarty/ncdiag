        subroutine nc_diag_cat_metadata_pass
            character(len=1000) :: err_string
            integer             :: old_dim_arr_total = 0, old_var_arr_total = 0
            
            character(:), allocatable :: input_file_cut
            
            input_count = cli_arg_count - 2
            
            call info("Scanning NetCDF files for dimensions and variables...")
            
            do arg_index = 1, input_count
                call get_command_argument(2 + arg_index, input_file)
                
                input_file_cut = trim(input_file)
                
                if (len(input_file_cut) <= 0) then
                    call usage("Invalid input file name - likely blank!")
                end if
                
                if (input_file_cut == output_file) then
                    call warning(" -> Ignoring output file in input file list.")
                    call info(" -> Skipping " // input_file_cut // " since it is the output file...")
                else
                    call info(" -> Opening " // input_file_cut // " for reading...")
                    call check(nf90_open(input_file, NF90_NOWRITE, ncid_input))
                    
                    ! Get top level info about the file!
                    call check(nf90_inquire(ncid_input, nDimensions = input_ndims, &
                        nVariables = input_nvars, nAttributes = input_nattrs))
                    
#ifdef DEBUG
                    write (*, "(A, I0)") "Number of dimensions: ", input_ndims
#endif
                    
                    if (cached_ndims == -1) &
                        cached_ndims = input_ndims
                    
                    if (cached_ndims /= input_ndims) &
                        call warning("Number of dimensions in " // trim(input_file) // " does not match first input file.")
                    
                    allocate(tmp_input_dimids(input_ndims))
                    
                    ! Get unlimited dimension information
                    call check(pf_nf90_inq_unlimdims(ncid_input, num_unlims))
                    
#ifdef DEBUG
                    write (*, "(A, I0)") "Number of unlimited dimensions: ", num_unlims
#endif
                    
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
#ifdef DEBUG
                            write (*, "(A, I0, A, I0, A)") " => Dimension #", tmp_dim_index, ": " // &
                                trim(tmp_dim_name) // " (size: ", &
                                tmp_dim_size, &
                                " - UNLIMITED)"
#endif
                            call nc_diag_cat_metadata_add_dim(tmp_dim_name, -1, tmp_dim_size)
                        else
#ifdef DEBUG
                            write (*, "(A, I0, A, I0, A)") " => Dimension #", tmp_dim_index, ": " // &
                                trim(tmp_dim_name) // " (size: ", &
                                tmp_dim_size, &
                                ")"
#endif
                            call nc_diag_cat_metadata_add_dim(trim(tmp_dim_name), tmp_dim_size)
                        end if
                    end do
                    
                    ! Variables
#ifdef DEBUG
                    write (*, "(A, I0)") "Number of variables: ", input_nvars
#endif
                    
                    if (cached_nvars == -1) cached_nvars = input_nvars
                    if (cached_nvars /= input_nvars) &
                        call warning("Number of variables in " // trim(input_file) // " does not match first input file.")
                    
                    allocate(tmp_input_varids(input_nvars))
                    
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
                            
#ifdef DEBUG
                            write (*, "(A, I0, A, I0)") " => Variable #", var_index, ": " // &
                                trim(tmp_var_name)
                            write (*, "(A)", advance = "NO") "    => Dimension IDs: "
                            
                            do i = 1, tmp_var_ndims
                                if (i /= 1) write (*, "(A)", advance = "NO") ", "
                                write (*, "(I0)", advance = "NO") tmp_var_dimids(i)
                            end do
                            
                            write (*, "(A)") ""
                            
                            write (*, "(A)", advance = "NO") "    => Dimensions: "
#endif
                            
                            do i = 1, tmp_var_ndims
#ifdef DEBUG
                                if (i /= 1) write (*, "(A)", advance = "NO") ", "
#endif
                                call check(nf90_inquire_dimension(ncid_input, tmp_var_dimids(i), tmp_var_dim_names(i)))
#ifdef DEBUG
                                write (*, "(A)", advance = "NO") trim(tmp_var_dim_names(i))
#endif
                            end do
                            
#ifdef DEBUG
                            write (*, "(A)") ""
#endif
                            
                            call nc_diag_cat_metadata_add_var(trim(tmp_var_name), tmp_var_type, tmp_var_ndims, tmp_var_dim_names)
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
                    
#ifdef DEBUG
                    write (*, "(A)") " => For all variables, the order of dimensions are INVERTED!"
#endif
                    
                    call check(nf90_close(ncid_input))
                    
                    deallocate(unlim_dims)
                    deallocate(tmp_input_dimids)
                    deallocate(tmp_input_varids)
                    
                    if (input_ndims == 0) &
                        call warning("No dimensions found in file " // input_file_cut // "!")
                    
                    if (input_nvars == 0) &
                        call warning("No variables found in file " // input_file_cut // "!")
                    
                    old_dim_arr_total = dim_arr_total
                    old_var_arr_total = var_arr_total
                end if
            end do
        end subroutine nc_diag_cat_metadata_pass
        
        subroutine nc_diag_cat_metadata_define
            integer :: i, j
            integer(i_long), dimension(3) :: alloc_dim_sizes = 0
            
            call info("Creating new dimensions and variables for output file...")
            
            call info(" -> Defining dimensions...")
            
            if (dim_arr_total == 0) &
                call warning("No dimensions found in input files, so not defining anything.")
            
            do i = 1, dim_arr_total
                if (dim_sizes(i) == -1) then
                    call check(nf90_def_dim(ncid_output, dim_names(i), &
                        NF90_UNLIMITED, dim_output_ids(i)))
                else
                    call check(nf90_def_dim(ncid_output, dim_names(i), &
                        dim_sizes(i), dim_output_ids(i)))
                end if
#ifdef DEBUG
                write(*, "(A, I0, A, I0)") "STORED DIMID for dim " // trim(dim_names(i)) // ": ", &
                    dim_output_ids(i), " | size: ", dim_sizes(i)
#endif
            end do
            
            if (var_arr_total == 0) &
                call warning("No variables found in input files, so not defining anything.")
            
            call info(" -> Defining variables...")
            do i = 1, var_arr_total
                do j = 1, var_dim_names(i)%num_names
                    var_dim_names(i)%output_dim_ids(j) = &
                        dim_output_ids(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(j)))
#ifdef DEBUG
                    write(*, "(A, I0)") "Paired ID for dim " // trim(var_dim_names(i)%dim_names(j)) // ": ", &
                        var_dim_names(i)%output_dim_ids(j)
#endif
                end do
                
#ifdef DEBUG
                write (*, "(A, I0, A)") "Defining variable: " // trim(var_names(i)) // " (type = ", var_types(i), ")"
                
                print *, "var_dim_names(i)%output_dim_ids", var_dim_names(i)%output_dim_ids
                print *, "LEN var_dim_names(i)%output_dim_ids", size(var_dim_names(i)%output_dim_ids)
#endif
                
                call check(nf90_def_var(ncid_output, var_names(i), var_types(i), &
                    var_dim_names(i)%output_dim_ids, &
                    var_output_ids(i)))
                
#ifdef DEBUG
                if (var_dim_names(i)%num_names == 1) print *, "DIM #1", var_dim_names(i)%dim_names(1)
                if (var_dim_names(i)%num_names == 2) print *, "DIM #2", var_dim_names(i)%dim_names(2)
                if (var_dim_names(i)%num_names == 3) print *, "DIM #3", var_dim_names(i)%dim_names(3)
#endif
                
                if (var_hasunlim(i)) then
                    if (var_dim_names(i)%num_names == 1) then
                        call check(nf90_def_var_chunking(ncid_output, var_output_ids(i), &
                            NF90_CHUNKED, (/ NC_DIAG_CAT_CHUNK_SIZE /) ))
                    else if (var_dim_names(i)%num_names == 2) then
                        call check(nf90_def_var_chunking(ncid_output, var_output_ids(i), &
                            NF90_CHUNKED, (/ dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(1))), &
                            NC_DIAG_CAT_CHUNK_SIZE /) ))
                    else if (var_dim_names(i)%num_names == 3) then
                        call check(nf90_def_var_chunking(ncid_output, var_output_ids(i), &
                            NF90_CHUNKED, &
                            (/ dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(1))), &
                                dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(2))), &
                                NC_DIAG_CAT_CHUNK_SIZE /) ))
                    end if
                else
                    if (var_dim_names(i)%num_names == 1) then
                        call check(nf90_def_var_chunking(ncid_output, var_output_ids(i), &
                            NF90_CHUNKED, (/ dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(1))) /) ))
                    else if (var_dim_names(i)%num_names == 2) then
                        call check(nf90_def_var_chunking(ncid_output, var_output_ids(i), &
                            NF90_CHUNKED, (/ dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(1))), &
                                dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(2))) /) ))
                    else if (var_dim_names(i)%num_names == 3) then
                        call check(nf90_def_var_chunking(ncid_output, var_output_ids(i), &
                            NF90_CHUNKED, &
                            (/ dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(1))), &
                                dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(2))), &
                                dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(3))) /) ))
                    end if
                end if
                
                call check(nf90_def_var_deflate(ncid_output, var_output_ids(i), &
                    shuffle = 1, deflate = 1, deflate_level = NC_DIAG_CAT_GZIP_COMPRESS))
            end do
            
            ! Next portion depends on defines/vars in ncdc_data_decl.F90
            call info(" -> Allocating data storage for variables...")
            
            allocate(data_blobs(var_arr_total))
            
            do i = 1, var_arr_total
                if (var_dim_names(i)%num_names == 1) then
                    alloc_dim_sizes = (/ &
                        dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(1))), &
                        0, &
                        0 /)
                    
                    ! Check for unlimited sizes and replace them!
                    if (alloc_dim_sizes(1) == -1) &
                        alloc_dim_sizes(1) = &
                            dim_unlim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(1)))
                    
                    if (var_types(i) == NF90_BYTE)   allocate(data_blobs(i)%byte_buffer(alloc_dim_sizes(1)))
                    if (var_types(i) == NF90_SHORT)  allocate(data_blobs(i)%short_buffer(alloc_dim_sizes(1)))
                    if (var_types(i) == NF90_INT)    allocate(data_blobs(i)%long_buffer(alloc_dim_sizes(1)))
                    if (var_types(i) == NF90_FLOAT)  allocate(data_blobs(i)%rsingle_buffer(alloc_dim_sizes(1)))
                    if (var_types(i) == NF90_DOUBLE) allocate(data_blobs(i)%rdouble_buffer(alloc_dim_sizes(1)))
                    if (var_types(i) == NF90_CHAR)   call error("1D character variable type not supported!")
                    
                    if (var_types(i) == NF90_BYTE)   data_blobs(i)%byte_buffer    = NF90_FILL_BYTE
                    if (var_types(i) == NF90_SHORT)  data_blobs(i)%short_buffer   = NF90_FILL_SHORT
                    if (var_types(i) == NF90_INT)    data_blobs(i)%long_buffer    = NF90_FILL_INT
                    if (var_types(i) == NF90_FLOAT)  data_blobs(i)%rsingle_buffer = NF90_FILL_FLOAT
                    if (var_types(i) == NF90_DOUBLE) data_blobs(i)%rdouble_buffer = NF90_FILL_DOUBLE
                else if (var_dim_names(i)%num_names == 2) then
                    alloc_dim_sizes = (/ &
                        dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(1))), &
                        dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(2))), &
                        0 /)
                    
                    ! Check for unlimited sizes and replace them!
                    if (alloc_dim_sizes(2) == -1) &
                        alloc_dim_sizes(2) = &
                            dim_unlim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(2)))
                    
                    if (var_types(i) == NF90_BYTE)   allocate(data_blobs(i)%byte_2d_buffer(alloc_dim_sizes(1), alloc_dim_sizes(2)))
                    if (var_types(i) == NF90_SHORT)  allocate(data_blobs(i)%short_2d_buffer(alloc_dim_sizes(1), alloc_dim_sizes(2)))
                    if (var_types(i) == NF90_INT)    allocate(data_blobs(i)%long_2d_buffer(alloc_dim_sizes(1), alloc_dim_sizes(2)))
                    if (var_types(i) == NF90_FLOAT)  allocate(data_blobs(i)%rsingle_2d_buffer(alloc_dim_sizes(1), alloc_dim_sizes(2)))
                    if (var_types(i) == NF90_DOUBLE) allocate(data_blobs(i)%rdouble_2d_buffer(alloc_dim_sizes(1), alloc_dim_sizes(2)))
                    if (var_types(i) == NF90_CHAR)   allocate(data_blobs(i)%string_buffer(alloc_dim_sizes(1), alloc_dim_sizes(2)))
                    
                    if (var_types(i) == NF90_BYTE)   data_blobs(i)%byte_2d_buffer    = NF90_FILL_BYTE
                    if (var_types(i) == NF90_SHORT)  data_blobs(i)%short_2d_buffer   = NF90_FILL_SHORT
                    if (var_types(i) == NF90_INT)    data_blobs(i)%long_2d_buffer    = NF90_FILL_INT
                    if (var_types(i) == NF90_FLOAT)  data_blobs(i)%rsingle_2d_buffer = NF90_FILL_FLOAT
                    if (var_types(i) == NF90_DOUBLE) data_blobs(i)%rdouble_2d_buffer = NF90_FILL_DOUBLE
                    if (var_types(i) == NF90_CHAR)   data_blobs(i)%string_buffer     = NF90_FILL_CHAR
                    
                else if (var_dim_names(i)%num_names == 3) then
                    alloc_dim_sizes = (/ &
                        dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(1))), &
                        dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(2))), &
                        dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(3))) /)
                    
                    ! Check for unlimited sizes and replace them!
                    ! (Though, this should always be the case...)
                    if (alloc_dim_sizes(3) == -1) &
                        alloc_dim_sizes(3) = &
                            dim_unlim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(3)))
                    
                    if (var_types(i) == NF90_CHAR) then
                        allocate(data_blobs(i)%string_2d_buffer(alloc_dim_sizes(1), alloc_dim_sizes(2), alloc_dim_sizes(3)))
                        data_blobs(i)%string_2d_buffer = NF90_FILL_CHAR
                    else
                        call error("3D non-character variable type not supported!")
                    end if
                end if
                
                data_blobs(i)%alloc_size = alloc_dim_sizes
                !print *, trim(var_names(i)), data_blobs(i)%alloc_size
            end do
            
#ifdef DEBUG
            print *, "!! END DEFINITION PASS"
#endif
        end subroutine nc_diag_cat_metadata_define
        
        function nc_diag_cat_lookup_dim(dim_name) result(ind)
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
        end function nc_diag_cat_lookup_dim
        
        subroutine nc_diag_cat_metadata_add_dim(dim_name, dim_size, dim_ul_size)
            character(len=*), intent(in)    :: dim_name
            integer         , intent(in)    :: dim_size
            integer,optional, intent(in)    :: dim_ul_size
            
            integer                         :: dim_index
            character(len=1000)             :: err_string
            
            dim_index = nc_diag_cat_lookup_dim(dim_name)
            
            ! If we can't find it, it's new! Make sure we have enough
            ! space for it...
            if (dim_index == -1) then
#ifdef DEBUG
                print *, "NEW DIM!"
#endif
                dim_arr_total = dim_arr_total + 1
                
                if (dim_arr_total >= dim_arr_size) then
                    if (allocated(dim_names)) then
                        call nc_diag_realloc(dim_names, DIM_START_SIZE)
                        call nc_diag_realloc(dim_sizes, DIM_START_SIZE)
                        call nc_diag_realloc(dim_counters, DIM_START_SIZE)
                        call nc_diag_realloc(dim_output_ids, DIM_START_SIZE)
                        call nc_diag_realloc(dim_unlim_sizes, DIM_START_SIZE)
                        dim_arr_size = dim_arr_size + DIM_START_SIZE
                    else
                        allocate(dim_names(DIM_START_SIZE))
                        allocate(dim_sizes(DIM_START_SIZE))
                        allocate(dim_counters(DIM_START_SIZE))
                        allocate(dim_output_ids(DIM_START_SIZE))
                        allocate(dim_unlim_sizes(DIM_START_SIZE))
                        dim_arr_size = DIM_START_SIZE
                    end if
                end if
                
                dim_index = dim_arr_total
                
                ! Add name
                dim_names(dim_index) = dim_name
                dim_sizes(dim_index) = 0
                dim_unlim_sizes(dim_index) = 0
                
                ! Set counter to 0
                dim_counters(dim_index) = 0
                dim_output_ids(dim_index) = -1
            end if
            
            if (dim_size /= -1) then
                ! Add/update size
                if ((index(dim_name, "_maxstrlen") /= 0) .OR. (index(dim_name, "_str_dim") /= 0)) then
                    ! Use the maximum as the new size... and skip the check.
                    if (dim_size > dim_sizes(dim_index)) dim_sizes(dim_index) = dim_size
                else
                    if ((dim_sizes(dim_index) /= 0) .AND. (dim_size /= dim_sizes(dim_index))) then
                        write (err_string, "(A, I0, A, I0, A)") &
                            "Fixed dimension length changed between files!" // &
                            CHAR(10) // "             " // &
                            "(Fixed dimension '" // dim_name // "' changed from length ", &
                            dim_sizes(dim_index), &
                            CHAR(10) // "             " // &
                            "to ", &
                            dim_size, &
                            "!)"
                        call error(trim(err_string))
                    end if
                    dim_sizes(dim_index) = dim_size
                end if
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
                
                if (present(dim_ul_size)) then
                    dim_unlim_sizes(dim_index) = dim_unlim_sizes(dim_index) + dim_ul_size
                else
                    call warning("Call made for unlimited dimension without specifying unlimited size!")
                end if
            end if
        end subroutine nc_diag_cat_metadata_add_dim
        
        function nc_diag_cat_lookup_var(var_name) result(ind)
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
        end function nc_diag_cat_lookup_var
        
        subroutine nc_diag_cat_metadata_add_var(var_name, var_type, var_ndims, var_dims)
            character(len=*), intent(in)    :: var_name
            integer(i_long) , intent(in)    :: var_type
            integer(i_long) , intent(in)    :: var_ndims
            character(len=*), intent(in)    :: var_dims(:)
            
            integer(i_long)                 :: var_index, i
            character(len=1000)             :: err_string
            
            var_index = nc_diag_cat_lookup_var(trim(var_name))
            
            ! If we can't find it, it's new! Make sure we have enough
            ! space for it...
            if (var_index == -1) then
#ifdef DEBUG
                print *, "NEW VAR! Var = " // trim(var_name)
#endif
                
                var_arr_total = var_arr_total + 1
                
                if (var_arr_total >= var_arr_size) then
                    if (allocated(var_names)) then
                        call nc_diag_realloc(var_names, VAR_START_SIZE)
                        call nc_diag_realloc(var_types, VAR_START_SIZE)
                        call nc_diag_realloc(var_dim_names, VAR_START_SIZE)
                        call nc_diag_realloc(var_output_ids, VAR_START_SIZE)
                        call nc_diag_realloc(var_counters, VAR_START_SIZE)
                        call nc_diag_realloc(var_hasunlim, VAR_START_SIZE)
                    else
                        allocate(var_names(VAR_START_SIZE))
                        allocate(var_types(VAR_START_SIZE))
                        allocate(var_dim_names(VAR_START_SIZE))
                        allocate(var_output_ids(VAR_START_SIZE))
                        allocate(var_counters(VAR_START_SIZE))
                        allocate(var_hasunlim(VAR_START_SIZE))
                        var_arr_size = VAR_START_SIZE
                    end if
                end if
                
#ifdef DEBUG
                write (*, "(A)", advance="NO") "DEBUG DUMP:"
                
                do i = 1, var_arr_total - 1
                    if (i /= 1) write (*, "(A)", advance="NO") ", "
                    write (*, "(A)", advance="NO") var_names(i)
                end do
                
                print *, "NEW var_index: ", var_arr_total
#endif
                
                var_index = var_arr_total
                
                ! Add name
                var_names(var_index) = var_name
                var_types(var_index) = var_type
                var_counters(var_index) = 0
            end if
            
            if (allocated(var_dim_names(var_index)%dim_names)) then
                ! Just do a sanity check!
                if (var_types(var_index) /= var_type) &
                    call error("Variable type changed!" // &
                        CHAR(10) // "             " // &
                        "(Type of variable '" // var_name // "' changed from " // &
                        trim(nc_diag_cat_metadata_type_to_str(var_types(var_index))) // &
                        CHAR(10) // "             " // &
                        "to " // &
                        trim(nc_diag_cat_metadata_type_to_str(var_type)) // &
                        "!)")
                
                if (var_dim_names(var_index)%num_names /= var_ndims) then
                    write (err_string, "(A, I0, A, I0, A)") &
                        "Variable ndims changed!" // &
                        CHAR(10) // "             " // &
                        "(Variable '" // var_name // "' changed ndims from ", &
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
                        "(Variable '" // var_name // "' changed dimension from " // &
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
                    if (dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(var_index)%dim_names(i))) == -1) then
                        var_hasunlim(var_index) = .TRUE.
                        exit
                    end if
                end do
                
            end if
        end subroutine nc_diag_cat_metadata_add_var
        
        function nc_diag_cat_metadata_type_to_str(var_type) result(nc_str)
            integer(i_long)   :: var_type
            character(len=11) :: nc_str
            
            nc_str = "(invalid)"
            
            if (var_type == NF90_BYTE)   nc_str = "NF90_BYTE"
            if (var_type == NF90_SHORT)  nc_str = "NF90_SHORT"
            if (var_type == NF90_INT)    nc_str = "NF90_INT (LONG)"
            if (var_type == NF90_FLOAT)  nc_str = "NF90_FLOAT"
            if (var_type == NF90_DOUBLE) nc_str = "NF90_DOUBLE"
            if (var_type == NF90_CHAR)   nc_str = "NF90_CHAR"
        end function nc_diag_cat_metadata_type_to_str
