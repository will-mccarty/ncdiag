        subroutine nc_diag_compress_data_pass
            integer :: cur_out_var_id, cur_out_var_ndims
            integer :: cur_out_var_ind
            integer, dimension(:), allocatable :: cur_dim_sizes
            
            character(len=1000)                :: err_string
            
            call info("Concatenating data to output file...")
            
            call info(" -> Opening " // trim(input_file) // " for reading...")
            call check(nf90_open(input_file, NF90_NOWRITE, ncid_input))
            
            ! Get top level info about the file!
            call check(nf90_inquire(ncid_input, nVariables = input_nvars))
            
            ! Variables
            
            ! Loop through each variable!
            do var_index = 1, input_nvars
                ! Grab number of dimensions and attributes first
                call check(nf90_inquire_variable(ncid_input, var_index, name = tmp_var_name, ndims = tmp_var_ndims))
                
                ! Allocate temporary variable dimids storage!
                allocate(tmp_var_dimids(tmp_var_ndims))
                allocate(cur_dim_sizes(tmp_var_ndims))
                
                ! Grab the actual dimension IDs and attributes
                call check(nf90_inquire_variable(ncid_input, var_index, dimids = tmp_var_dimids, &
                    xtype = tmp_var_type))
                
                do i = 1, tmp_var_ndims
                    call check(nf90_inquire_dimension(ncid_input, tmp_var_dimids(i), len = cur_dim_sizes(i)))
                end do
                
                ! Now, let's lookup everything and translate the result to our file.
                cur_out_var_ind = nc_diag_compress_lookup_var(tmp_var_name)
                cur_out_var_id = var_output_ids(cur_out_var_ind)
                cur_out_var_ndims = var_dim_names(cur_out_var_ind)%num_names
                
                if ((cur_out_var_ndims == 1) .OR. &
                    ((cur_out_var_ndims == 2) .AND. (tmp_var_type == NF90_CHAR))) then
                    
                    if (tmp_var_type == NF90_BYTE) then
                        allocate(byte_buffer   (cur_dim_sizes(1)))
                        byte_buffer = NF90_FILL_BYTE
                        call check(nf90_get_var(ncid_input, var_index, byte_buffer))
                        call check(nf90_put_var(ncid_output, cur_out_var_id, byte_buffer, &
                            start = (/ 1 /), &
                            count = (/ cur_dim_sizes(1) /) ))
                        deallocate(byte_buffer)
                    else if (tmp_var_type == NF90_SHORT) then
                        allocate(short_buffer  (cur_dim_sizes(1)))
                        short_buffer = NF90_FILL_SHORT
                        call check(nf90_get_var(ncid_input, var_index, short_buffer))
                        call check(nf90_put_var(ncid_output, cur_out_var_id, short_buffer, &
                            start = (/ 1 /), &
                            count = (/ cur_dim_sizes(1) /) ))
                        deallocate(short_buffer)
                    else if (tmp_var_type == NF90_INT) then
                        allocate(long_buffer   (cur_dim_sizes(1)))
                        long_buffer = NF90_FILL_INT
                        call check(nf90_get_var(ncid_input, var_index, long_buffer))
                        call check(nf90_put_var(ncid_output, cur_out_var_id, long_buffer, &
                            start = (/ 1 /), &
                            count = (/ cur_dim_sizes(1) /) ))
                        deallocate(long_buffer)
                    else if (tmp_var_type == NF90_FLOAT) then
                        allocate(rsingle_buffer(cur_dim_sizes(1)))
                        rsingle_buffer = NF90_FILL_FLOAT
                        call check(nf90_get_var(ncid_input, var_index, rsingle_buffer))
                        call check(nf90_put_var(ncid_output, cur_out_var_id, rsingle_buffer, &
                            start = (/ 1 /), &
                            count = (/ cur_dim_sizes(1) /) ))
                        deallocate(rsingle_buffer)
                    else if (tmp_var_type == NF90_DOUBLE) then
                        allocate(rdouble_buffer(cur_dim_sizes(1)))
                        rdouble_buffer = NF90_FILL_DOUBLE
                        call check(nf90_get_var(ncid_input, var_index, rdouble_buffer))
                        call check(nf90_put_var(ncid_output, cur_out_var_id, rdouble_buffer, &
                            start = (/ 1 /), &
                            count = (/ cur_dim_sizes(1) /) ))
                        deallocate(rdouble_buffer)
                    else if (tmp_var_type == NF90_CHAR) then
                        allocate(string_buffer (cur_dim_sizes(1), cur_dim_sizes(2)))
                        string_buffer = NF90_FILL_CHAR
                        
                        call check(nf90_get_var(ncid_input, var_index, string_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ cur_dim_sizes(1), cur_dim_sizes(2) /) ))
                        
                        call check(nf90_put_var(ncid_output, cur_out_var_id, string_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ cur_dim_sizes(1), cur_dim_sizes(2) /) ))
                        deallocate(string_buffer)
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
                        allocate(byte_2d_buffer   (cur_dim_sizes(1), cur_dim_sizes(2)))
                        byte_2d_buffer = NF90_FILL_BYTE
                        call check(nf90_get_var(ncid_input, var_index, byte_2d_buffer))
                        call check(nf90_put_var(ncid_output, cur_out_var_id, byte_2d_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ cur_dim_sizes(1), cur_dim_sizes(2) /) ))
                        deallocate(byte_2d_buffer)
                    else if (tmp_var_type == NF90_SHORT) then
                        allocate(short_2d_buffer  (cur_dim_sizes(1), cur_dim_sizes(2)))
                        short_2d_buffer = NF90_FILL_SHORT
                        call check(nf90_get_var(ncid_input, var_index, short_2d_buffer))
                        call check(nf90_put_var(ncid_output, cur_out_var_id, short_2d_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ cur_dim_sizes(2), cur_dim_sizes(1) /) ))
                        deallocate(short_2d_buffer)
                    else if (tmp_var_type == NF90_INT) then
                        allocate(long_2d_buffer   (cur_dim_sizes(1), cur_dim_sizes(2)))
                        long_2d_buffer = NF90_FILL_INT
                        call check(nf90_get_var(ncid_input, var_index, long_2d_buffer))
                        call check(nf90_put_var(ncid_output, cur_out_var_id, long_2d_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ cur_dim_sizes(1), cur_dim_sizes(2) /) ))
                        deallocate(long_2d_buffer)
                    else if (tmp_var_type == NF90_FLOAT) then
                        allocate(rsingle_2d_buffer(cur_dim_sizes(1), cur_dim_sizes(2)))
                        rsingle_2d_buffer = NF90_FILL_FLOAT
                        call check(nf90_get_var(ncid_input, var_index, rsingle_2d_buffer))
                        call check(nf90_put_var(ncid_output, cur_out_var_id, rsingle_2d_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ cur_dim_sizes(1), cur_dim_sizes(2) /) ))
                        deallocate(rsingle_2d_buffer)
                    else if (tmp_var_type == NF90_DOUBLE) then
                        allocate(rdouble_2d_buffer(cur_dim_sizes(1), cur_dim_sizes(2)))
                        rdouble_2d_buffer = NF90_FILL_DOUBLE
                        call check(nf90_get_var(ncid_input, var_index, rdouble_2d_buffer))
                        call check(nf90_put_var(ncid_output, cur_out_var_id, rdouble_2d_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ cur_dim_sizes(1), cur_dim_sizes(2) /) ))
                        deallocate(rdouble_2d_buffer)
                    else if (tmp_var_type == NF90_CHAR) then
                        allocate(string_2d_buffer (cur_dim_sizes(1), cur_dim_sizes(2), cur_dim_sizes(3)))
                        string_2d_buffer = NF90_FILL_CHAR
                        call check(nf90_get_var(ncid_input, var_index, string_2d_buffer, &
                            start = (/ 1, 1, 1 /), &
                            count = (/ cur_dim_sizes(1), cur_dim_sizes(2), cur_dim_sizes(3) /) ))
                        call check(nf90_put_var(ncid_output, cur_out_var_id, string_2d_buffer, &
                            start = (/ 1, 1, 1 /), &
                            count = (/ cur_dim_sizes(1), cur_dim_sizes(2), cur_dim_sizes(3) /) ))
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
                
                ! Deallocate
                deallocate(tmp_var_dimids)
                deallocate(cur_dim_sizes)
            end do
            
            call check(nf90_close(ncid_input))
        end subroutine nc_diag_compress_data_pass


