        subroutine nc_diag_cat_data_pass
            integer :: cur_dim_id, cur_dim_len
            integer :: cur_out_var_id, cur_out_var_ndims, cur_out_var_counter
            integer :: cur_out_dim_ind, cur_out_var_ind
            integer, dimension(:), allocatable :: cur_out_dim_ids, cur_dim_ids
            integer, dimension(:), allocatable :: cur_out_dim_sizes
            integer, dimension(:), allocatable :: cur_dim_sizes
            
            character(1) ,dimension(:,:), allocatable :: tmp_string_buffer
            character(1),dimension(:,:,:),allocatable :: string_2d_buffer
            
            integer :: i, j
            
            character(len=NF90_MAX_NAME) , allocatable :: tmp_in_dim_names(:)
            
            character(len=1000)                :: err_string
            
            character(:), allocatable :: input_file_cut
            
            if (.NOT. allocated(var_names)) then
                call warning("No variables found to concatenate.")
                return
            end if
            
            call info("Reading in data from all files...")
            
#ifdef DEBUG
            print *, " !!! BEGINNING DATA PASS!!"
#endif
            
            input_count = cli_arg_count - 2
            
            do arg_index = 1, input_count
#ifdef DEBUG
                print *, " !!! INPUT FILE STAGE"
#endif
                call get_command_argument(2 + arg_index, input_file)
                
                input_file_cut = trim(input_file)
                
                if (len(input_file_cut) <= 0) then
                    call usage("Invalid input file name - likely blank!")
                end if
                
                if (input_file_cut == output_file) then
                    ! No warning here - we've already shown it in metadata.
                    call info(" -> Skipping " // input_file_cut // " since it is the output file...")
                else
                    call info(" -> Opening " // input_file_cut // " for reading...")
                    call check(nf90_open(input_file, NF90_NOWRITE, ncid_input, &
                        cache_size = 2147483647))
                    
                    ! Get top level info about the file!
                    call check(nf90_inquire(ncid_input, nDimensions = input_ndims, &
                        nVariables = input_nvars, nAttributes = input_nattrs))
                    
                    ! Dimensions
                    allocate(tmp_in_dim_names(input_ndims))
                    do tmp_dim_index = 1, input_ndims
                        call check(nf90_inquire_dimension(ncid_input, tmp_dim_index, &
                            tmp_in_dim_names(tmp_dim_index)))
                    end do
                    
                    ! Variables
#ifdef DEBUG
                    write (*, "(A, I0)") "Number of variables: ", input_nvars
#endif
                    
                    allocate(tmp_input_varids(input_nvars))
                    
                    ! Loop through each variable!
                    do var_index = 1, input_nvars
                        ! Grab number of dimensions and attributes first
                        call check(nf90_inquire_variable(ncid_input, var_index, name = tmp_var_name, ndims = tmp_var_ndims))
                        
#ifdef DEBUG
                        print *, "** PROCESSING VARIABLE: " // trim(tmp_var_name)
#endif
                        
                        ! Allocate temporary variable dimids storage!
                        allocate(tmp_var_dimids(tmp_var_ndims))
                        allocate(tmp_var_dim_names(tmp_var_ndims))
                        allocate(cur_dim_ids(tmp_var_ndims))
                        allocate(cur_dim_sizes(tmp_var_ndims))
                        allocate(cur_out_dim_ids(tmp_var_ndims))
                        allocate(cur_out_dim_sizes(tmp_var_ndims))
                        
#ifdef DEBUG
                        print *, "** (ALLOC DONE)"
#endif
                        
                        ! Grab the actual dimension IDs and attributes
                        call check(nf90_inquire_variable(ncid_input, var_index, dimids = tmp_var_dimids, &
                            xtype = tmp_var_type))
                        
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
                            call check(nf90_inquire_dimension(ncid_input, tmp_var_dimids(i), tmp_var_dim_names(i), cur_dim_sizes(i)))
#ifdef DEBUG
                            write (*, "(A)", advance = "NO") trim(tmp_var_dim_names(i))
#endif
                            cur_out_dim_ind = nc_diag_cat_lookup_dim(tmp_var_dim_names(i))
                            cur_out_dim_ids(i)   = dim_output_ids(cur_out_dim_ind)
                            cur_out_dim_sizes(i) = dim_sizes(cur_out_dim_ind)
                        end do
                        
#ifdef DEBUG
                        write (*, "(A)") ""
#endif
                        
                        ! Now, let's lookup everything and translate the result to our file.
                        cur_out_var_ind = nc_diag_cat_lookup_var(tmp_var_name)
                        cur_out_var_id = var_output_ids(cur_out_var_ind)
                        cur_out_var_ndims = var_dim_names(cur_out_var_ind)%num_names
                        cur_out_var_counter = var_counters(cur_out_var_ind)
                        
#ifdef DEBUG
                        print *, " (starting var write)"
#endif
                        
                        !print *, "VARIABLE: " // trim(var_names(cur_out_var_ind))
                        !print *, "ALLOC SIZES:", data_blobs(cur_out_var_ind)%alloc_size
                        !print *, "CUR POS:", data_blobs(cur_out_var_ind)%cur_pos
                        !print *, "DIM SIZES:", cur_out_dim_sizes
                        !print *, "VAR COUNTER:", cur_out_var_counter
                        
                        ! Check for one-time only vars...
                        if (((.NOT. any(cur_out_dim_sizes == -1)) .AND. (cur_out_var_counter == 0)) &
                            .OR. (any(cur_out_dim_sizes == -1))) then
                            
                            if ((cur_out_var_ndims == 1) .OR. &
                                ((cur_out_var_ndims == 2) .AND. (tmp_var_type == NF90_CHAR))) then
                                !! TODO: 
                                !! implement max str len for string stuff
                                !! fix string implementation below (need 2D spec + max str len!)
                                !! check here to make sure things work, then do 2D stuff!
                                
                                !! NOTE THAT THIS IS ON LOCAL MACHINE, AFTER DONE TRANSFER TO DALI AND
                                !! CONTINUE DEV/TESTING THERE!!!
                                
                                if (tmp_var_type == NF90_BYTE) then
                                    call check(nf90_get_var(ncid_input, var_index, &
                                        data_blobs(cur_out_var_ind)%byte_buffer &
                                            (data_blobs(cur_out_var_ind)%cur_pos : &
                                                data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(1) - 1), &
                                        start = (/ 1 /), &
                                        count = (/ cur_dim_sizes(1) /) ))
                                else if (tmp_var_type == NF90_SHORT) then
                                    call check(nf90_get_var(ncid_input, var_index, &
                                        data_blobs(cur_out_var_ind)%short_buffer &
                                            (data_blobs(cur_out_var_ind)%cur_pos : &
                                                data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(1) - 1), &
                                        start = (/ 1 /), &
                                        count = (/ cur_dim_sizes(1) /) ))
                                else if (tmp_var_type == NF90_INT) then
                                    call check(nf90_get_var(ncid_input, var_index, &
                                        data_blobs(cur_out_var_ind)%long_buffer &
                                            (data_blobs(cur_out_var_ind)%cur_pos : &
                                                data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(1) - 1), &
                                        start = (/ 1 /), &
                                        count = (/ cur_dim_sizes(1) /) ))
                                else if (tmp_var_type == NF90_FLOAT) then
                                    call check(nf90_get_var(ncid_input, var_index, &
                                        data_blobs(cur_out_var_ind)%rsingle_buffer &
                                            (data_blobs(cur_out_var_ind)%cur_pos : &
                                                data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(1) - 1), &
                                        start = (/ 1 /), &
                                        count = (/ cur_dim_sizes(1) /) ))
                                else if (tmp_var_type == NF90_DOUBLE) then
                                    call check(nf90_get_var(ncid_input, var_index, &
                                        data_blobs(cur_out_var_ind)%rdouble_buffer &
                                            (data_blobs(cur_out_var_ind)%cur_pos : &
                                                data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(1) - 1), &
                                        start = (/ 1 /), &
                                        count = (/ cur_dim_sizes(1) /) ))
                                else if (tmp_var_type == NF90_CHAR) then
                                    !write (*, "(A)") "VARIABLE: " // trim(var_names(cur_out_var_ind))
                                    !print *, "ALLOC SIZES: ", data_blobs(cur_out_var_ind)%alloc_size
                                    !write (*, "(A, I0, A, I0)") "start = ", data_blobs(cur_out_var_ind)%cur_pos, &
                                    !    " | end = ", data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1
                                    !write (*, "(A, I0, A, I0)") "var dim 1 = ", cur_dim_sizes(1), &
                                    !    " | var dim 2 = ", cur_dim_sizes(2)
                                    !data_blobs(cur_out_var_ind)%string_buffer &
                                    !    (1 : cur_dim_sizes(1), &
                                    !        data_blobs(cur_out_var_ind)%cur_pos : &
                                    !        data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1) &
                                    !            = NF90_FILL_CHAR
                                    !call check(nf90_get_var(ncid_input, var_index, &
                                    !    data_blobs(cur_out_var_ind)%string_buffer &
                                    !        (1 : data_blobs(cur_out_var_ind)%alloc_size(1), &
                                    !            data_blobs(cur_out_var_ind)%cur_pos : &
                                    !            data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1), &
                                    !    start = (/ 1, 1 /), &
                                    !    count = (/ cur_dim_sizes(1), cur_dim_sizes(2) /) ))
                                    
                                    ! ALTERNATIVE USING INDEXING
                                    ! Note - this is VERY slow and inefficient!
                                    
                                    !do i = data_blobs(cur_out_var_ind)%cur_pos, &
                                    !    data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1
                                    !    call check(nf90_get_var(ncid_input, var_index, &
                                    !        data_blobs(cur_out_var_ind)%string_buffer &
                                    !            (1 : cur_dim_sizes(1), &
                                    !                i), &
                                    !        start = (/ 1, 1 /), &
                                    !        count = (/ cur_dim_sizes(1), 1 /) ))
                                    !end do
                                    
                                    ! Strangely enough, NetCDF doesn't support storing strings to
                                    ! an array splice. Even with defined bounds, the strings is not
                                    ! stored properly, especially when the variable's dimensions
                                    ! are smaller than the actual target's dimensions. The smaller
                                    ! strings are stored contiguously within the array, going outside
                                    ! the given bounds.
                                    ! 
                                    ! For example, given [ '1234', '5678' ], placing it into a 5x2 array
                                    ! yields [ '12345', '678**' ] instead of [ '1234 ', '5678 ' ].
                                    
                                    allocate(tmp_string_buffer (cur_dim_sizes(1), cur_dim_sizes(2)))
                                    tmp_string_buffer = NF90_FILL_CHAR
                                    
                                    call check(nf90_get_var(ncid_input, var_index, tmp_string_buffer, &
                                        start = (/ 1, 1 /), &
                                        count = (/ cur_dim_sizes(1), cur_dim_sizes(2) /) ))
                                    
                                    data_blobs(cur_out_var_ind)%string_buffer &
                                        (1 : cur_dim_sizes(1), &
                                            data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1) = &
                                        tmp_string_buffer
                                    
                                    deallocate(tmp_string_buffer)
                                    
                                    !do i = data_blobs(cur_out_var_ind)%cur_pos, &
                                    !    data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1
                                    !    
                                    !    write (*, "(I0, A)", advance = "no") i, "    ["
                                    !    
                                    !    do j = 1, data_blobs(cur_out_var_ind)%alloc_size(1)
                                    !        write (*, "(A)", advance = "no") &
                                    !            data_blobs(cur_out_var_ind)%string_buffer(j, i)
                                    !    end do
                                    !    write (*, "(A)") "]"
                                    !end do
                                else
                                    write (err_string, "(A, I0, A)") &
                                        "Invalid type detected during write." // &
                                        CHAR(10) // "             " // &
                                        "(Variable '" // trim(tmp_var_name) // "' has an type of ", &
                                        tmp_var_type, "," // &
                                        CHAR(10) // "             " // &
                                        "which is invalid!)"
                                    call error(trim(err_string))
                                end if
                            else if ((cur_out_var_ndims == 2) .OR. &
                                ((cur_out_var_ndims == 3) .AND. (tmp_var_type == NF90_CHAR))) then
                                
                                if (tmp_var_type == NF90_BYTE) then
                                    call check(nf90_get_var(ncid_input, var_index, &
                                        data_blobs(cur_out_var_ind)%byte_2d_buffer &
                                            (1 : cur_dim_sizes(1), &
                                                data_blobs(cur_out_var_ind)%cur_pos : &
                                                data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1)))
                                else if (tmp_var_type == NF90_SHORT) then
                                    call check(nf90_get_var(ncid_input, var_index, &
                                        data_blobs(cur_out_var_ind)%short_2d_buffer &
                                            (1 : cur_dim_sizes(1), &
                                                data_blobs(cur_out_var_ind)%cur_pos : &
                                                data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1)))
                                else if (tmp_var_type == NF90_INT) then
                                    call check(nf90_get_var(ncid_input, var_index, &
                                        data_blobs(cur_out_var_ind)%long_2d_buffer &
                                            (1 : cur_dim_sizes(1), &
                                                data_blobs(cur_out_var_ind)%cur_pos : &
                                                data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1)))
#ifdef DEBUG
                                    print *, "Storage place: ", dim_counters(nc_diag_cat_lookup_dim(tmp_var_dim_names(2)))
#endif
                                else if (tmp_var_type == NF90_FLOAT) then
                                    call check(nf90_get_var(ncid_input, var_index, &
                                        data_blobs(cur_out_var_ind)%rsingle_2d_buffer &
                                            (1 : cur_dim_sizes(1), &
                                                data_blobs(cur_out_var_ind)%cur_pos : &
                                                data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1), &
                                        start = (/ 1, 1 /), &
                                        count = (/ cur_dim_sizes(1), cur_dim_sizes(2) /) ))
                                else if (tmp_var_type == NF90_DOUBLE) then
                                    call check(nf90_get_var(ncid_input, var_index, &
                                        data_blobs(cur_out_var_ind)%rdouble_2d_buffer &
                                        (1 : cur_dim_sizes(1), &
                                            data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1), &
                                        start = (/ 1, 1 /), &
                                        count = (/ cur_dim_sizes(1), cur_dim_sizes(2) /) ))
                                else if (tmp_var_type == NF90_CHAR) then
                                    ! Use string buffer variable - same issue as before with 1D strings!
                                    !call check(nf90_get_var(ncid_input, var_index, &
                                    !    data_blobs(cur_out_var_ind)%string_2d_buffer &
                                    !        (1 : cur_dim_sizes(1), 1 : cur_dim_sizes(2), &
                                    !            data_blobs(cur_out_var_ind)%cur_pos : &
                                    !            data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(3) - 1), &
                                    !    start = (/ 1, 1, 1 /), &
                                    !    count = (/ cur_dim_sizes(1), cur_dim_sizes(2), cur_dim_sizes(3) /) ))
                                    allocate(string_2d_buffer (cur_dim_sizes(1), cur_dim_sizes(2), cur_dim_sizes(3)))
                                    string_2d_buffer = NF90_FILL_CHAR
                                    call check(nf90_get_var(ncid_input, var_index, string_2d_buffer, &
                                        start = (/ 1, 1, 1 /), &
                                        count = (/ cur_dim_sizes(1), cur_dim_sizes(2), cur_dim_sizes(3) /) ))
                                    data_blobs(cur_out_var_ind)%string_2d_buffer &
                                        (1 : cur_dim_sizes(1), 1 : cur_dim_sizes(2), &
                                            data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(3) - 1) & &
                                        = string_2d_buffer(:,:,:)
                                    deallocate(string_2d_buffer)
                                else
                                    write (err_string, "(A, I0, A)") &
                                        "Invalid type detected during write." // &
                                        CHAR(10) // "             " // &
                                        "(Variable '" // trim(tmp_var_name) // "' has an type of ", &
                                        tmp_var_type, "," // &
                                        CHAR(10) // "             " // &
                                        "which is invalid!)"
                                    call error(trim(err_string))
                                end if
                            end if
                            
                            !print *, "cur_out_var_ndims", cur_out_var_ndims
                            !print *, "cur_dim_sizes(cur_out_var_ndims)", cur_dim_sizes(cur_out_var_ndims)
                            !print *, "any(cur_out_dim_sizes == -1)", any(cur_out_dim_sizes == -1)
                            
                            if (any(cur_out_dim_sizes == -1)) &
                                data_blobs(cur_out_var_ind)%cur_pos = &
                                    data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(cur_out_var_ndims)
                            
                            var_counters(cur_out_var_ind) = &
                                    var_counters(cur_out_var_ind) + 1
                        end if
                        
#ifdef DEBUG
                        print *, " (end var write / start dealloc)"
#endif
                        
                        ! Deallocate
                        deallocate(tmp_var_dimids)
                        deallocate(tmp_var_dim_names)
                        deallocate(cur_dim_ids)
                        deallocate(cur_dim_sizes)
                        deallocate(cur_out_dim_ids)
                        deallocate(cur_out_dim_sizes)
                        
#ifdef DEBUG
                        print *, " (end dealloc)"
#endif
                    end do
                    
                    ! Update any unlimited counters
                    if (any(dim_sizes == -1)) then
                        do i = 1, dim_arr_total
                            ! Check for -1 - unlimited indicator
                            if ((dim_sizes(i) == -1) .AND. (any(tmp_in_dim_names == dim_names(i)))) then
                                ! We got one! But... we need to find this dimension in the file.
                                ! First, lookup dimension name to get dimension ID.
#ifdef DEBUG
                                print *, "Unlimited dimension name: ", trim(dim_names(i))
#endif
                                call check(nf90_inq_dimid(ncid_input, dim_names(i), cur_dim_id))
                                
                                ! Then, grab the current unlimited dimension length!
                                call check(nf90_inquire_dimension(ncid_input, cur_dim_id, len = cur_dim_len))
                                
                                ! Add the length to the counter!
                                dim_counters(i) = dim_counters(i) + cur_dim_len
                            end if
                        end do
                    end if
                    
                    call check(nf90_close(ncid_input))
                    
                    !deallocate(unlim_dims)
                    !deallocate(tmp_input_dimids)
                    deallocate(tmp_input_varids)
                    deallocate(tmp_in_dim_names)
                end if
            end do
            
            call info("Doing final data write...")
            
            do var_index = 1, var_arr_total
                call info(" => Writing variable " // trim(var_names(var_index)) // "...")
                if ((var_dim_names(var_index)%num_names == 1) .OR. &
                    ((var_dim_names(var_index)%num_names == 2) .AND. (var_types(var_index) == NF90_CHAR)) ) then
                    if (var_types(var_index) == NF90_BYTE) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%byte_buffer, &
                            start = (/ 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1) /) ))
                    if (var_types(var_index) == NF90_SHORT) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%short_buffer, &
                            start = (/ 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1) /) ))
                    if (var_types(var_index) == NF90_INT) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%long_buffer, &
                            start = (/ 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1) /) ))
                    if (var_types(var_index) == NF90_FLOAT) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%rsingle_buffer, &
                            start = (/ 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1) /) ))
                    
                    if (var_types(var_index) == NF90_DOUBLE) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%rdouble_buffer, &
                            start = (/ 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1) /) ))
                    if (var_types(var_index) == NF90_CHAR) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%string_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1), &
                                data_blobs(var_index)%alloc_size(2) /) ))
                else if ((var_dim_names(var_index)%num_names == 2) .OR. &
                    ((var_dim_names(var_index)%num_names == 3) .AND. (var_types(var_index) == NF90_CHAR)) ) then
                    if (var_types(var_index) == NF90_BYTE) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%byte_2d_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1), &
                                data_blobs(var_index)%alloc_size(2) /) ))
                    if (var_types(var_index) == NF90_SHORT) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%short_2d_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1), &
                                data_blobs(var_index)%alloc_size(2) /) ))
                    if (var_types(var_index) == NF90_INT) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%long_2d_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1), &
                                data_blobs(var_index)%alloc_size(2) /) ))
                    if (var_types(var_index) == NF90_FLOAT) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%rsingle_2d_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1), &
                                data_blobs(var_index)%alloc_size(2) /) ))
                    if (var_types(var_index) == NF90_DOUBLE) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%rdouble_2d_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1), &
                                data_blobs(var_index)%alloc_size(2) /) ))
                    if (var_types(var_index) == NF90_CHAR) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%string_2d_buffer, &
                            start = (/ 1, 1, 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1), &
                                data_blobs(var_index)%alloc_size(2), &
                                data_blobs(var_index)%alloc_size(3) /) ))
                end if
            end do
        end subroutine nc_diag_cat_data_pass


