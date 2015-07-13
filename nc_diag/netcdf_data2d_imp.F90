        !===============================================================
        ! nc_diag_data2d - data2d handling (implementation)
        !===============================================================
        ! DO NOT COMPILE THIS DIRECTLY! THIS IS MEANT TO BE INCLUDED
        ! INSIDE A LARGER F90 SOURCE!
        ! If you compile this directly, you WILL face the WRATH of your
        ! compiler!
        !---------------------------------------------------------------
        ! Depends on:
        !   netcdf_mresize.F90, netcdf_realloc_data2d.f90
        ! 
        ! Technically, order shouldn't matter... but just in case,
        ! include netcdf_realloc_imp.f90 FIRST!
        !---------------------------------------------------------------
        ! nc_diag_data2d stores data2d data as NetCDF4 global
        ! attributes. The nc_diag_data2d subroutines temporarily cache
        ! any data2d data until write, where it will be set by
        ! NF90_PUT_ATT().
        !---------------------------------------------------------------
        ! This file provides the actual data2d data adding subroutines,
        ! referred to by the interface. It also provides support
        ! subroutines for data2d writing and allocation setup.
        
        subroutine nc_diag_data2d_allocmulti(multiplier)
            integer(i_long), intent(in)    :: multiplier
            if (init_done) then
                ! # of times we needed to realloc simple data2d
                ! also the multiplier factor for allocation (2^x)
                diag_data2d_store%alloc_s_multi = multiplier
                
                ! # of times we needed to realloc data2d data storage
                ! also the multiplier factor for allocation (2^x)
                diag_data2d_store%alloc_m_multi = multiplier
                
                ! # of times we needed to realloc data2d INDEX data storage
                ! also the multiplier factor for allocation (2^x)
                diag_data2d_store%alloc_mi_multi = multiplier
            end if
        end subroutine nc_diag_data2d_allocmulti
        
        function nc_diag_data2d_max_len_var(var_index) result(max_len)
            integer(i_llong), intent(in)    :: var_index
            
            integer :: i, max_len
            
            max_len = -1
            
            do i = 1, diag_data2d_store%stor_i_arr(var_index)%icount
                if (diag_data2d_store%stor_i_arr(var_index)%length_arr(i) > max_len) &
                    max_len = diag_data2d_store%stor_i_arr(var_index)%length_arr(i)
            end do
        end function nc_diag_data2d_max_len_var
        
        subroutine nc_diag_data2d_write_def(internal)
            logical, intent(in), optional         :: internal
            
            integer(i_byte)                       :: data_type
            character(len=100)                    :: data2d_name
            
            integer(i_llong)                      :: curdatindex, j
            integer(i_kind)                       :: nc_data_type
            integer(i_long)                       :: tmp_dim_id
            character(len=120)                    :: data_dim_name
            character(len=120)                    :: data_dim_str_name
            integer(i_long)                       :: max_len
            integer(i_long)                       :: max_str_len, msl_tmp
            
            character(len=:),         allocatable :: string_arr(:)
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (enable_action) then
                if (present(internal)) then
                    write(action_str, "(A, L, A)") "nc_diag_data2d_write_def(internal = ", internal, ")"
                else
                    write(action_str, "(A)") "nc_diag_data2d_write_def(internal = (not specified))"
                end if
                call actionm(trim(action_str))
            end if
#endif
            
            if (init_done) then
                if (.NOT. diag_data2d_store%def_lock) then
                    call check(nf90_def_dim(ncid, "nobs_data2d", NF90_UNLIMITED, diag_data2d_store%nobs_dim_id))
                    
                    do curdatindex = 1, diag_data2d_store%total
                        data2d_name = diag_data2d_store%names(curdatindex)
                        data_type = diag_data2d_store%types(curdatindex)
                        
                        call info("data2d: defining " // trim(data2d_name))
                        
                        if (data_type == NLAYER_BYTE)   nc_data_type = NF90_BYTE
                        if (data_type == NLAYER_SHORT)  nc_data_type = NF90_SHORT
                        if (data_type == NLAYER_LONG)   nc_data_type = NF90_INT
                        if (data_type == NLAYER_FLOAT)  nc_data_type = NF90_FLOAT
                        if (data_type == NLAYER_DOUBLE) nc_data_type = NF90_DOUBLE
                        if (data_type == NLAYER_STRING) nc_data_type = NF90_CHAR
                        
#ifdef _DEBUG_MEM_
                        print *, "data2d part 1"
#endif
                        
                        ! We need to create a new dimension...
                        write (data_dim_name, "(A, A)") trim(data2d_name), "_arr_dim"
                        
                        ! Find the maximum array length of this variable!
                        max_len = nc_diag_data2d_max_len_var(curdatindex)
                        
                        ! Create this maximum array length dimension for this variable
                        call check(nf90_def_dim(ncid, data_dim_name, max_len, diag_data2d_store%var_dim_ids(curdatindex)))
                        
                        ! Store maximum length
                        diag_data2d_store%max_lens(curdatindex) = max_len;
                        
                        if (data_type == NLAYER_STRING) then
                            max_str_len = 0
                            write (data_dim_name, "(A, A)") trim(data2d_name), "_maxstrlen"
                            
                            ! Dimension is # of chars by # of obs (unlimited)
                            do j = 1, diag_data2d_store%stor_i_arr(curdatindex)%icount
                                allocate(character(10000) :: string_arr(diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j)))
                                string_arr = &
                                    diag_data2d_store%m_string(diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) &
                                        : diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) + &
                                            diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j))
                                
                                write(*, "(A, I0)") "DEBUG DATA2D: tmp array size is: ", diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j)
                                
                                ! Now we can calculate the length!
                                msl_tmp = max_len_string_array(string_arr, &
                                    diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j))
                                
                                if (msl_tmp > max_str_len) max_str_len = msl_tmp
                                
#ifdef _DEBUG_MEM_
                                write (*, "(A, A, A, I0, A, I0)") "DEBUG DATA2D DEF WRITE: at data2d_name ", trim(data2d_name), ", msl_tmp computes to ", msl_tmp, ", max_str_len computes to ", max_str_len
                                print *, "DEBUG DATA2D DEF WRITE: string array dump follows:"
                                call string_array_dump(string_arr)
#endif
                                
                                ! Deallocate right after we're done!
                                deallocate(string_arr)
                            end do
                            
#ifdef _DEBUG_MEM_
                            write (*, "(A, A, A, I0, A, I0)") "DEBUG DATA2D DEF WRITE: ** at data2d_name ", trim(data2d_name), ", FINAL max_str_len computes to ", max_str_len, ", max_len computes to ", max_len
#endif
                            
                            ! Save the max string len
                            diag_data2d_store%max_str_lens(curdatindex) = max_str_len
                            
                            ! Create dimension needed!
                            write (data_dim_str_name, "(A, A)") trim(data2d_name), "_str_dim"
                            call check(nf90_def_dim(ncid, data_dim_str_name, max_str_len, tmp_dim_id))
                            
#ifdef _DEBUG_MEM_
                            print *, "Defining char var type..."
#endif
                            
                            call check(nf90_def_var(ncid, data2d_name, nc_data_type, &
                                (/ tmp_dim_id, diag_data2d_store%var_dim_ids(curdatindex), diag_data2d_store%nobs_dim_id /), &
                                diag_data2d_store%var_ids(curdatindex)))
                            
#ifdef _DEBUG_MEM_
                            write (*, "(A, A, A, I0, A, I0)") "DEBUG DATA2D DEF WRITE: ** at data2d_name ", trim(data2d_name), ", result VID is ", diag_data2d_store%var_ids(curdatindex)
                            write (*, "(A, I0, A, I0)") "DEBUG DATA2D DEF WRITE: ** result dim is unlim x max_len = ", max_len, " x max_str_len = ", max_str_len
                            print *, "data2d part 2"
#endif
                            
#ifdef _DEBUG_MEM_
                            print *, "Done defining char var type..."
#endif
                        else
#ifdef _DEBUG_MEM_
                            print *, "Definition for variable " // trim(data2d_name) // ":"
                            print *, diag_data2d_store%max_lens(curdatindex), "x unlimited (NetCDF order)"
#endif
                            call check(nf90_def_var(ncid, data2d_name, nc_data_type, &
                                (/ diag_data2d_store%var_dim_ids(curdatindex), diag_data2d_store%nobs_dim_id /), &
                                diag_data2d_store%var_ids(curdatindex)))
                        end if
                        
                        call nc_diag_varattr_add_var(diag_data2d_store%names(curdatindex), &
                                    diag_data2d_store%types(curdatindex), &
                                    diag_data2d_store%var_ids(curdatindex))
                        
                        ! Enable compression
                        ! Args: ncid, varid, enable_shuffle (yes), enable_deflate (yes), deflate_level
#ifdef _DEBUG_MEM_
                        print *, "Defining compression 1 (chunking)..."
#endif
                        
                        if (data_type == NLAYER_STRING) then
                            !call check(nf90_def_var_chunking(ncid, diag_data2d_store%var_ids(curdatindex), &
                            !    NF90_CHUNKED, (/ 1, 1024, 1024 /)))
                        else
                            !call check(nf90_def_var_chunking(ncid, diag_data2d_store%var_ids(curdatindex), &
                            !    NF90_CHUNKED, (/ 1024, 1024 /)))
                        end if
                        
#ifdef _DEBUG_MEM_
                        print *, "Defining compression 2 (gzip)..."
#endif
                        call check(nf90_def_var_deflate(ncid, diag_data2d_store%var_ids(curdatindex), &
                            1, 1, NLAYER_COMPRESSION))
                        
#ifdef _DEBUG_MEM_
                        print *, "Done defining compression..."
#endif
                        
                        ! Lock the definitions!
                        diag_data2d_store%def_lock = .TRUE.
                    end do
                else
                    if(.NOT. present(internal)) &
                        call error("Can't write definitions - definitions have already been written and locked!")
                end if
            end if
        end subroutine nc_diag_data2d_write_def
        
        subroutine nc_diag_data2d_write_data(flush_data_only)
            ! Optional internal flag to only flush data - if this is
            ! true, data flushing will be performed, and the data will
            ! NOT be locked.
            logical, intent(in), optional         :: flush_data_only
            
            integer(i_byte)                       :: data_type
            character(len=100)                    :: data2d_name
            
            ! For some strange reason, curdatindex needs to be
            ! initialized here to 1, otherwise a runtime error of using
            ! an undefined variable occurs... even though it's set
            ! by the DO loop...
            integer(i_long)                       :: curdatindex = 1, j
            
            integer(i_byte), dimension(:, :), allocatable :: byte_arr
            integer(i_short),dimension(:, :), allocatable :: short_arr
            integer(i_long), dimension(:, :), allocatable :: long_arr
            real(r_single),  dimension(:, :), allocatable :: rsingle_arr
            real(r_double),  dimension(:, :), allocatable :: rdouble_arr
            character(len=:),dimension(:, :), allocatable :: string_arr
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (enable_action) then
                if (present(flush_data_only)) then
                    write(action_str, "(A, L, A)") "nc_diag_data2d_write_data(flush_data_only = ", flush_data_only, ")"
                else
                    write(action_str, "(A)") "nc_diag_data2d_write_data(flush_data_only = (not specified))"
                end if
                call actionm(trim(action_str))
            end if
#endif
            if (init_done .AND. allocated(diag_data2d_store)) then
                if (.NOT. diag_data2d_store%data_lock) then
                    do curdatindex = 1, diag_data2d_store%total
#ifdef _DEBUG_MEM_
                        print *, curdatindex
#endif
                        data2d_name = diag_data2d_store%names(curdatindex)
                        data_type = diag_data2d_store%types(curdatindex)
                        
                        call info("data2d: writing " // trim(data2d_name))
                        
                        ! Make sure we have data to write in the first place!
                        if (diag_data2d_store%stor_i_arr(curdatindex)%icount > 0) then
                            if (data_type == NLAYER_BYTE) then
                                allocate(byte_arr(diag_data2d_store%stor_i_arr(curdatindex)%icount, &
                                    diag_data2d_store%max_lens(curdatindex)))
                                
                                byte_arr = NLAYER_FILL_BYTE
                                
                                do j = 1, diag_data2d_store%stor_i_arr(curdatindex)%icount
                                    byte_arr(j, :) = diag_data2d_store%m_byte( &
                                        diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) : &
                                        diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) + &
                                            diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j))
                                end do
                                
                                call check(nf90_put_var(&
                                    ncid, diag_data2d_store%var_ids(curdatindex), &
                                    byte_arr, &
                                    (/ 1, 1 + diag_data2d_store%rel_indexes(curdatindex) /), &
                                    (/ diag_data2d_store%max_lens(curdatindex), &
                                        diag_data2d_store%stor_i_arr(curdatindex)%icount /) &
                                    ))
                                
                                deallocate(byte_arr)
                            else if (data_type == NLAYER_SHORT) then
                                allocate(short_arr(diag_data2d_store%stor_i_arr(curdatindex)%icount, &
                                    diag_data2d_store%max_lens(curdatindex)))
                                
                                short_arr = NLAYER_FILL_SHORT
                                
                                do j = 1, diag_data2d_store%stor_i_arr(curdatindex)%icount
                                    short_arr(j, :) = diag_data2d_store%m_short( &
                                        diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) : &
                                        diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) + &
                                            diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j))
                                end do
                                
                                call check(nf90_put_var(&
                                    ncid, diag_data2d_store%var_ids(curdatindex), &
                                    short_arr, &
                                    (/ 1, 1 + diag_data2d_store%rel_indexes(curdatindex) /), &
                                    (/ diag_data2d_store%max_lens(curdatindex), &
                                        diag_data2d_store%stor_i_arr(curdatindex)%icount /) &
                                    ))
                                
                                deallocate(short_arr)
                            else if (data_type == NLAYER_LONG) then
                                allocate(long_arr(diag_data2d_store%stor_i_arr(curdatindex)%icount, &
                                    diag_data2d_store%max_lens(curdatindex)))
                                
                                long_arr = NLAYER_FILL_LONG
                                
                                do j = 1, diag_data2d_store%stor_i_arr(curdatindex)%icount
                                    long_arr(j, :) = diag_data2d_store%m_long( &
                                        diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) : &
                                        diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) + &
                                            diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j))
                                end do
                                
#ifdef _DEBUG_MEM_
                                write (*, "(A, I0, A, I0, A, I0, A, I0, A)") &
                                    "Writing long with start = (", 1, ", ", &
                                    1 + diag_data2d_store%rel_indexes(curdatindex), &
                                    "), count = (", diag_data2d_store%stor_i_arr(curdatindex)%icount, &
                                    ", ", 1, ")"
#endif
                                
                                call check(nf90_put_var(&
                                    ncid, diag_data2d_store%var_ids(curdatindex), &
                                    long_arr, &
                                    (/ 1, 1 + diag_data2d_store%rel_indexes(curdatindex) /), &
                                    (/ diag_data2d_store%max_lens(curdatindex), &
                                        diag_data2d_store%stor_i_arr(curdatindex)%icount /) &
                                    ))
                                
                                deallocate(long_arr)
                            else if (data_type == NLAYER_FLOAT) then
                                allocate(rsingle_arr(diag_data2d_store%stor_i_arr(curdatindex)%icount, &
                                    diag_data2d_store%max_lens(curdatindex)))
                                
                                rsingle_arr = NLAYER_FILL_FLOAT
                                
                                do j = 1, diag_data2d_store%stor_i_arr(curdatindex)%icount
                                    rsingle_arr(j, :) = diag_data2d_store%m_rsingle( &
                                        diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) : &
                                        diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) + &
                                            diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j))
                                end do
                                
                                !print *, "end queue / start put"
                                call check(nf90_put_var(&
                                    ncid, diag_data2d_store%var_ids(curdatindex), &
                                    rsingle_arr, &
                                    (/ 1, 1 + diag_data2d_store%rel_indexes(curdatindex) /), &
                                    (/ diag_data2d_store%max_lens(curdatindex), &
                                        diag_data2d_store%stor_i_arr(curdatindex)%icount /) &
                                    ))
                                !call check(nf90_sync(ncid))
                                deallocate(rsingle_arr)
                                !print *, "end put"
                                
                            else if (data_type == NLAYER_DOUBLE) then
                                allocate(rdouble_arr(diag_data2d_store%stor_i_arr(curdatindex)%icount, &
                                    diag_data2d_store%max_lens(curdatindex)))
                                
                                rdouble_arr = NLAYER_FILL_DOUBLE
                                
                                do j = 1, diag_data2d_store%stor_i_arr(curdatindex)%icount
                                    rdouble_arr(j, :) = diag_data2d_store%m_rdouble( &
                                        diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) : &
                                        diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) + &
                                            diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j))
                                end do
                                
                                call check(nf90_put_var(&
                                    ncid, diag_data2d_store%var_ids(curdatindex), &
                                    rdouble_arr, &
                                    (/ 1, 1 + diag_data2d_store%rel_indexes(curdatindex) /), &
                                    (/ diag_data2d_store%max_lens(curdatindex), &
                                        diag_data2d_store%stor_i_arr(curdatindex)%icount /) &
                                    ))
                                deallocate(rdouble_arr)
                            else if (data_type == NLAYER_STRING) then
                                allocate(character(diag_data2d_store%max_str_lens(curdatindex)) :: &
                                    string_arr(diag_data2d_store%stor_i_arr(curdatindex)%icount, &
                                    diag_data2d_store%max_lens(curdatindex) &
                                    ))
                                
                                string_arr = NLAYER_FILL_CHAR
                                
                                do j = 1, diag_data2d_store%stor_i_arr(curdatindex)%icount
                                    string_arr(j, :) = diag_data2d_store%m_string( &
                                        diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) : &
                                        diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) + &
                                            diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j))
                                end do
                                
                                call check(nf90_put_var(&
                                    ncid, diag_data2d_store%var_ids(curdatindex), &
                                    string_arr, &
                                    (/ 1, 1, 1 + diag_data2d_store%rel_indexes(curdatindex) /), &
                                    (/ diag_data2d_store%max_str_lens(curdatindex), &
                                        diag_data2d_store%max_lens(curdatindex), &
                                        diag_data2d_store%stor_i_arr(curdatindex)%icount /) &
                                    ))
                                
                                deallocate(string_arr)
                            end if
                            
                            ! Check for data flushing, and if so, update the relative indexes
                            ! and set icount to 0.
                            if (present(flush_data_only) .AND. flush_data_only) then
                                diag_data2d_store%rel_indexes(curdatindex) = &
                                    diag_data2d_store%rel_indexes(curdatindex) + &
                                    diag_data2d_store%stor_i_arr(curdatindex)%icount
                                diag_data2d_store%stor_i_arr(curdatindex)%icount = 0
                                
#ifdef _DEBUG_MEM_
                                print *, "diag_data2d_store%rel_indexes(curdatindex) is now:"
                                print *, diag_data2d_store%rel_indexes(curdatindex)
#endif
                            end if
                            
                        end if
                    end do
                    
                    if (present(flush_data_only) .AND. flush_data_only) then
#ifdef _DEBUG_MEM_
                        print *, "In buffer flush mode!"
#endif
                        
                        ! We need to reset all array counts to zero!
                        diag_data2d_store%acount = 0
                    else
                        ! Lock data writing
                        diag_data2d_store%data_lock = .TRUE.
#ifdef _DEBUG_MEM_
                        print *, "In data lock mode!"
#endif
                    end if
                else
                    call error("Can't write data - data have already been written and locked!")
                end if
            else
                call error("Can't write data - NetCDF4 layer not initialized yet!")
            end if
            
#ifdef _DEBUG_MEM_
            print *, "All done writing data2d data"
#endif
        end subroutine nc_diag_data2d_write_data
        
        !subroutine nc_diag_data2d_write
        !    integer(i_byte)                       :: data_type
        !    logical                               :: data_vect
        !    integer(i_long), dimension(6)         :: data_type_index
        !    integer(i_long), dimension(5)         :: data_type_index_vi
        !    character(len=100)                    :: data2d_name
        !    
        !    integer(i_long)               :: curdatindex
        !    integer(i_long)               :: curdatvecsize
        !    
        !    if (init_done) then
        !        data_type_index    = (/ 1, 1, 1, 1, 1, 1 /)
        !        data_type_index_vi = (/ 1, 1, 1, 1, 1 /)
        !        do curdatindex = 1, diag_data2d_store%total
        !            data2d_name = diag_data2d_store%names(curdatindex)
        !            data_type = diag_data2d_store%types(curdatindex)
        !            data_vect = diag_data2d_store%vectored(curdatindex)
        !            
        !            if (data_type == NLAYER_BYTE) then
        !                if (data_vect) then
        !                    if (data_type_index_vi(1) <= diag_data2d_store%acount(7)) then
        !                        ! Grab the vector size, and allocate as needed.
        !                        curdatvecsize = diag_data2d_store%m_byte_vi(data_type_index_vi(1))
        !                        data_type_index_vi(1) = data_type_index_vi(1) + 1
        !                    else
        !                        call error("Critical error - byte index exceeds internal count!")
        !                    end if
        !                    
        !                    call check(nf90_put_att(ncid, NF90_GLOBAL, data2d_name, diag_data2d_store%m_byte(data_type_index(1):(data_type_index(1) + curdatvecsize - 1))))
        !                    data_type_index(1) = data_type_index(1) + curdatvecsize
        !                else
        !                    call check(nf90_put_att(ncid, NF90_GLOBAL, data2d_name, diag_data2d_store%m_byte(data_type_index(1))))
        !                    data_type_index(1) = data_type_index(1) + 1
        !                end if
        !            else if (data_type == NLAYER_SHORT) then
        !                if (data_vect) then
        !                    if (data_type_index_vi(2) <= diag_data2d_store%acount(8)) then
        !                        ! Grab the vector size, and allocate as needed.
        !                        curdatvecsize = diag_data2d_store%m_short_vi(data_type_index_vi(2))
        !                        data_type_index_vi(2) = data_type_index_vi(2) + 1
        !                    else
        !                        call error("Critical error - short index exceeds internal count!")
        !                    end if
        !                    
        !                    call check(nf90_put_att(ncid, NF90_GLOBAL, data2d_name, diag_data2d_store%m_short(data_type_index(2):(data_type_index(2) + curdatvecsize - 1))))
        !                    data_type_index(2) = data_type_index(2) + curdatvecsize
        !                else
        !                    call check(nf90_put_att(ncid, NF90_GLOBAL, data2d_name, diag_data2d_store%m_short(data_type_index(2))))
        !                    data_type_index(2) = data_type_index(2) + 1
        !                end if
        !            else if (data_type == NLAYER_LONG) then
        !                if (data_vect) then
        !                    if (data_type_index_vi(3) <= diag_data2d_store%acount(9)) then
        !                        ! Grab the vector size...
        !                        curdatvecsize = diag_data2d_store%m_long_vi(data_type_index_vi(3))
        !                        data_type_index_vi(3) = data_type_index_vi(3) + 1
        !                    else
        !                        call error("Critical error - long index exceeds internal count!")
        !                    end if
        !                    
        !                    call check(nf90_put_att(ncid, NF90_GLOBAL, data2d_name, diag_data2d_store%m_long(data_type_index(3):(data_type_index(3) + curdatvecsize - 1))))
        !                    data_type_index(3) = data_type_index(3) + curdatvecsize
        !                else
        !                    call check(nf90_put_att(ncid, NF90_GLOBAL, data2d_name, diag_data2d_store%m_long(data_type_index(3))))
        !                    data_type_index(3) = data_type_index(3) + 1
        !                end if
        !            else if (data_type == NLAYER_FLOAT) then
        !                if (data_vect) then
        !                    if (data_type_index_vi(4) <= diag_data2d_store%acount(10)) then
        !                        ! Grab the vector size, and allocate as needed.
        !                        curdatvecsize = diag_data2d_store%m_rsingle_vi(data_type_index_vi(4))
        !                        data_type_index_vi(4) = data_type_index_vi(4) + 1
        !                    else
        !                        call error("Critical error - rsingle index exceeds internal count!")
        !                    end if
        !                    
        !                    call check(nf90_put_att(ncid, NF90_GLOBAL, data2d_name, diag_data2d_store%m_rsingle(data_type_index(4):(data_type_index(4) + curdatvecsize - 1))))
        !                    data_type_index(4) = data_type_index(4) + curdatvecsize
        !                else
        !                    call check(nf90_put_att(ncid, NF90_GLOBAL, data2d_name, diag_data2d_store%m_rsingle(data_type_index(4))))
        !                    data_type_index(4) = data_type_index(4) + 1
        !                end if
        !            else if (data_type == NLAYER_DOUBLE) then
        !                if (data_vect) then
        !                    if (data_type_index_vi(5) <= diag_data2d_store%acount(11)) then
        !                        ! Grab the vector size, and allocate as needed.
        !                        curdatvecsize = diag_data2d_store%m_rdouble_vi(data_type_index_vi(5))
        !                        data_type_index_vi(5) = data_type_index_vi(5) + 1
        !                    else
        !                        call error("Critical error - rdouble index exceeds internal count!")
        !                    end if
        !                    
        !                    call check(nf90_put_att(ncid, NF90_GLOBAL, data2d_name, diag_data2d_store%m_rdouble(data_type_index(5):(data_type_index(5) + curdatvecsize - 1))))
        !                    data_type_index(5) = data_type_index(5) + curdatvecsize
        !                else
        !                    call check(nf90_put_att(ncid, NF90_GLOBAL, data2d_name, diag_data2d_store%m_rdouble(data_type_index(5))))
        !                    data_type_index(5) = data_type_index(5) + 1
        !                end if
        !            else if (data_type == NLAYER_STRING) then
        !                ! String array not available with NF90 attributes
        !                !if (data_vect) then
        !                !    if (data_type_index_vi(6) <= diag_data2d_store%acount(12)) then
        !                !        ! Grab the vector size, and allocate as needed.
        !                !        curdatvecsize = diag_data2d_store%m_string_vi(data_type_index_vi(6))
        !                !        data_type_index_vi(6) = data_type_index_vi(6) + 1
        !                !    else
        !                !        call error("Critical error - string index exceeds internal count!")
        !                !    end if
        !                !    
        !                !    data_type_index(6) = data_type_index(6) + curdatvecsize
        !                !    call check(nf90_put_att(ncid, NF90_GLOBAL, data2d_name, diag_data2d_store%m_string(data_type_index(6):(data_type_index(6) + curdatvecsize - 1))))
        !                !else
#ifdef _!DEBUG_MEM_
        !                    ! NOTE: trim() is F95
        !                    print *, "On curdatindex:"
        !                    print *, curdatindex
        !                    print *, "For variable:"
        !                    print *, trim(data2d_name)
        !                    print *, "Writing data2d string:"
        !                    print *, trim(diag_data2d_store%m_string(data_type_index(6)))
#endif  !
        !                    call check(nf90_put_att(ncid, NF90_GLOBAL, data2d_name, trim(diag_data2d_store%m_string(data_type_index(6)))))
        !                    data_type_index(6) = data_type_index(6) + 1
        !                !end if
        !            else
        !                call error("Critical error - unknown variable type!")
        !            end if
        !            
        !        end do
        !    else
        !        call error("No nc_diag initialized yet!")
        !    end if
        !    
        !end subroutine nc_diag_data2d_write
        
        ! Preallocate variable name/type/etc. storage.
        subroutine nc_diag_data2d_prealloc_vars(num_of_addl_vars)
            integer(i_llong), intent(in)          :: num_of_addl_vars
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (enable_action) then
                write(action_str, "(A, I0, A)") "nc_diag_data2d_prealloc_vars(num_of_addl_vars = ", num_of_addl_vars, ")"
                call actionm(trim(action_str))
            end if
#endif
            if (init_done .AND. allocated(diag_data2d_store)) then
                if (allocated(diag_data2d_store%names)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%names)) then
                        call nc_diag_realloc(diag_data2d_store%names, num_of_addl_vars)
                    end if
                else
                    allocate(diag_data2d_store%names(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                end if
                
                if (allocated(diag_data2d_store%types)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%types)) then
                        call nc_diag_realloc(diag_data2d_store%types, num_of_addl_vars)
                    end if
                else
                    allocate(diag_data2d_store%types(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                end if
                
                if (allocated(diag_data2d_store%stor_i_arr)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%stor_i_arr)) then
                        call nc_diag_data2d_resize_iarr_type(num_of_addl_vars)
                    end if
                else
                    allocate(diag_data2d_store%stor_i_arr(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                end if
                
                if (allocated(diag_data2d_store%var_ids)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%var_ids)) then
                        call nc_diag_realloc(diag_data2d_store%var_ids, num_of_addl_vars)
                    end if
                else
                    allocate(diag_data2d_store%var_ids(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_data2d_store%var_ids = -1
                end if
                
                if (allocated(diag_data2d_store%var_dim_ids)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%var_dim_ids)) then
                        call nc_diag_realloc(diag_data2d_store%var_dim_ids, num_of_addl_vars)
                    end if
                else
                    allocate(diag_data2d_store%var_dim_ids(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_data2d_store%var_dim_ids = -1
                end if
                
                if (allocated(diag_data2d_store%alloc_sia_multi)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%alloc_sia_multi)) then
                        call nc_diag_realloc(diag_data2d_store%alloc_sia_multi, num_of_addl_vars)
                    end if
                else
                    allocate(diag_data2d_store%alloc_sia_multi(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_data2d_store%alloc_sia_multi = 0
                end if
                
                if (allocated(diag_data2d_store%max_str_lens)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%max_str_lens)) then
                        call nc_diag_realloc(diag_data2d_store%max_str_lens, num_of_addl_vars)
                    end if
                else
                    allocate(diag_data2d_store%max_str_lens(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_data2d_store%max_str_lens = -1
                end if
                
                if (allocated(diag_data2d_store%rel_indexes)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%rel_indexes)) then
                        call nc_diag_realloc(diag_data2d_store%rel_indexes, num_of_addl_vars)
                    end if
                else
                    allocate(diag_data2d_store%rel_indexes(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_data2d_store%rel_indexes = 0
                end if
                
                if (allocated(diag_data2d_store%max_lens)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%max_lens)) then
                        call nc_diag_realloc(diag_data2d_store%max_lens, num_of_addl_vars)
                    end if
                else
                    allocate(diag_data2d_store%max_lens(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_data2d_store%max_lens = 0
                end if
                
                diag_data2d_store%prealloc_total = diag_data2d_store%prealloc_total + num_of_addl_vars
            else
                call error("NetCDF4 layer not initialized yet!")
            endif
        end subroutine nc_diag_data2d_prealloc_vars
        
        ! Preallocate actual variable data storage
        subroutine nc_diag_data2d_prealloc_vars_storage(nclayer_type, num_of_addl_slots)
            integer(i_byte), intent(in)           :: nclayer_type
            integer(i_llong), intent(in)          :: num_of_addl_slots
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (enable_action) then
                write(action_str, "(A, I0, A, I0, A)") "nc_diag_data2d_prealloc_vars_storage(nclayer_type = ", nclayer_type, ", num_of_addl_slots = ", num_of_addl_slots, ")"
                call actionm(trim(action_str))
            end if
#endif            
            
            if (nclayer_type == NLAYER_BYTE) then
                call nc_diag_data2d_resize_byte(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_SHORT) then
                call nc_diag_data2d_resize_short(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_LONG) then
                call nc_diag_data2d_resize_long(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_FLOAT) then
                call nc_diag_data2d_resize_rsingle(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_DOUBLE) then
                call nc_diag_data2d_resize_rdouble(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_STRING) then
                call nc_diag_data2d_resize_string(num_of_addl_slots, .FALSE.)
            else
                call error("Invalid type specified for variable storage preallocation!")
            end if
            
            ! resize nc_diag_data2d_resize_iarr ?
            
        end subroutine nc_diag_data2d_prealloc_vars_storage
        
        ! Preallocate index storage
        subroutine nc_diag_data2d_prealloc_vars_storage_all(num_of_addl_slots)
            integer(i_llong), intent(in)          :: num_of_addl_slots
            integer(i_long)                       :: i
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (enable_action) then
                write(action_str, "(A, I0, A)") "nc_diag_data2d_prealloc_vars_storage_all(num_of_addl_slots = ", num_of_addl_slots, ")"
                call actionm(trim(action_str))
            end if
#endif
            
            !print *, "PREALLOC IARR: "
            !print *, num_of_addl_slots
            
            do i = 1, diag_data2d_store%prealloc_total
                call nc_diag_data2d_resize_iarr(i, num_of_addl_slots, .FALSE.)
            end do
        end subroutine nc_diag_data2d_prealloc_vars_storage_all
        
        subroutine nc_diag_data2d_expand
            integer(i_llong) :: addl_fields
            
            ! Did we realloc at all?
            logical :: meta_realloc
            
            meta_realloc = .FALSE.
            
            if (init_done .AND. allocated(diag_data2d_store)) then
                addl_fields = 1 + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_data2d_store%alloc_s_multi))
                
#ifdef _DEBUG_MEM_
                call debug("INITIAL value of diag_data2d_store%alloc_s_multi:")
                print *, diag_data2d_store%alloc_s_multi
#endif
                
                if (allocated(diag_data2d_store%names)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%names)) then
#ifdef _DEBUG_MEM_
                        call debug("Reallocating diag_data2d_store%names...")
                        print *, (NLAYER_MULTI_BASE ** diag_data2d_store%alloc_s_multi)
                        print *, addl_fields
#endif
                        call nc_diag_realloc(diag_data2d_store%names, addl_fields)
#ifdef _DEBUG_MEM_
                    call debug("Reallocated diag_data2d_store%names. Size:")
                    print *, size(diag_data2d_store%names)
#endif
                        meta_realloc = .TRUE.
                    end if
                else
#ifdef _DEBUG_MEM_
                    call debug("Allocating diag_data2d_store%names for first time...")
                    print *, NLAYER_DEFAULT_ENT
#endif
                    
                    allocate(diag_data2d_store%names(NLAYER_DEFAULT_ENT))
                    
#ifdef _DEBUG_MEM_
                    call debug("Allocated diag_data2d_store%names. Size:")
                    print *, size(diag_data2d_store%names)
#endif
                end if
                
                if (allocated(diag_data2d_store%types)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%types)) then
#ifdef _DEBUG_MEM_
                        call debug("Reallocating diag_data2d_store%types...")
                        print *, (NLAYER_MULTI_BASE ** diag_data2d_store%alloc_s_multi)
                        print *, addl_fields
#endif
                        call nc_diag_realloc(diag_data2d_store%types, addl_fields)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_data2d_store%types(NLAYER_DEFAULT_ENT))
                end if
                
                if (allocated(diag_data2d_store%stor_i_arr)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%stor_i_arr)) then
#ifdef _DEBUG_MEM_
                        call debug("Reallocating diag_data2d_store%stor_i_arr...")
                        print *, (NLAYER_MULTI_BASE ** diag_data2d_store%alloc_s_multi)
                        print *, (1 + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_data2d_store%alloc_s_multi)))
#endif
                        call nc_diag_data2d_resize_iarr_type(addl_fields)
                        
                        !call nc_diag_realloc(diag_data2d_store%stor_i_arr, 1 + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_data2d_store%alloc_s_multi)))
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_data2d_store%stor_i_arr(NLAYER_DEFAULT_ENT))
                end if
                
                if (allocated(diag_data2d_store%var_ids)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%var_ids)) then
                        call nc_diag_realloc(diag_data2d_store%var_ids, addl_fields)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_data2d_store%var_ids(NLAYER_DEFAULT_ENT))
                    diag_data2d_store%var_ids = -1
                end if
                
                if (allocated(diag_data2d_store%var_dim_ids)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%var_dim_ids)) then
                        call nc_diag_realloc(diag_data2d_store%var_dim_ids, addl_fields)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_data2d_store%var_dim_ids(NLAYER_DEFAULT_ENT))
                    diag_data2d_store%var_dim_ids = -1
                end if
                
                if (allocated(diag_data2d_store%alloc_sia_multi)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%alloc_sia_multi)) then
                        call nc_diag_realloc(diag_data2d_store%alloc_sia_multi, addl_fields)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_data2d_store%alloc_sia_multi(NLAYER_DEFAULT_ENT))
                    diag_data2d_store%alloc_sia_multi = 0
                end if
                
                if (allocated(diag_data2d_store%max_str_lens)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%max_str_lens)) then
                        call nc_diag_realloc(diag_data2d_store%max_str_lens, addl_fields)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_data2d_store%max_str_lens(NLAYER_DEFAULT_ENT))
                    diag_data2d_store%max_str_lens = -1
                end if
                
                if (allocated(diag_data2d_store%rel_indexes)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%rel_indexes)) then
                        call nc_diag_realloc(diag_data2d_store%rel_indexes, addl_fields)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_data2d_store%rel_indexes(NLAYER_DEFAULT_ENT))
                    diag_data2d_store%rel_indexes = 0
                end if
                
                if (allocated(diag_data2d_store%max_lens)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%max_lens)) then
                        call nc_diag_realloc(diag_data2d_store%max_lens, addl_fields)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_data2d_store%max_lens(NLAYER_DEFAULT_ENT))
                    diag_data2d_store%max_lens = 0
                end if
                
                if (meta_realloc) then
                    !diag_data2d_store%alloc_s_count = diag_data2d_store%alloc_s_count + 1
                    diag_data2d_store%alloc_s_multi = diag_data2d_store%alloc_s_multi + 1
#ifdef _DEBUG_MEM_
                    print *, "Incrementing alloc_s_multi... new value:"
                    print *, diag_data2d_store%alloc_s_multi
#endif
                    !print *, "Size of new stuff:"
                    !print *, size(diag_data2d_store%names)
                endif
            else
                call error("NetCDF4 layer not initialized yet!")
            endif
            
        end subroutine nc_diag_data2d_expand
        
        function nc_diag_data2d_lookup_var(data2d_name) result(ind)
            character(len=*), intent(in)    :: data2d_name
            integer :: i, ind
            
            ind = -1
            
            if (init_done .AND. allocated(diag_data2d_store)) then
                do i = 1, diag_data2d_store%total
                    if (diag_data2d_store%names(i) == data2d_name) then
                        ind = i
                        exit
                    end if
                end do
            end if
        end function nc_diag_data2d_lookup_var
        
        ! nc_diag_data2d - input integer(i_byte)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_data2d_byte(data2d_name, data2d_value)
            character(len=*), intent(in)              :: data2d_name
            integer(i_byte), dimension(:), intent(in) :: data2d_value
            
            integer(i_long)                 :: var_index
            integer(i_llong)                :: input_size
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            integer(i_llong)                      :: data_value_size
            
            if (enable_action) then
                data_value_size = size(data2d_value)
                write(action_str, "(A, I0, A, I0, A, I0, A, I0, A)") &
                    "nc_diag_data2d_byte(data2d_name = " // data2d_name // &
                    ", data2d_value = array with length of ", &
                    data_value_size, &
                    " [", &
                    data2d_value(1), &
                    " ... ", &
                    data2d_value(data_value_size), &
                    "]"
                call actionm(trim(action_str))
            end if
#endif
            
            if (diag_data2d_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
            end if
            
            var_index = nc_diag_data2d_lookup_var(data2d_name)
            
            if (var_index == -1) then
                ! First, check to make sure we can still define new variables.
                if (diag_data2d_store%def_lock) then
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                call nc_diag_data2d_expand
                
                diag_data2d_store%total = diag_data2d_store%total + 1
                
                diag_data2d_store%names(diag_data2d_store%total) = data2d_name
                diag_data2d_store%types(diag_data2d_store%total) = NLAYER_BYTE
                
                var_index = diag_data2d_store%total
            end if
            
            ! Get input size and do size checks!
            input_size = size(data2d_value)
            
            if ((diag_data2d_store%def_lock) .AND. &
                (size(data2d_value) > diag_data2d_store%max_lens(var_index))) then
                call error("Cannot expand variable size after locking variable definitions!")
            end if
            
            ! We just need to add one entry...
            call nc_diag_data2d_resize_iarr(var_index, 1)
            call nc_diag_data2d_resize_byte(input_size)
            
            ! Now add the actual entry!
            diag_data2d_store%m_byte(diag_data2d_store%acount(1) - input_size + 1:diag_data2d_store%acount(1)) = &
                data2d_value
            diag_data2d_store%stor_i_arr(var_index)%index_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                diag_data2d_store%acount(1) - input_size + 1
            diag_data2d_store%stor_i_arr(var_index)%length_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                input_size
        end subroutine nc_diag_data2d_byte
        
        ! nc_diag_data2d - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_data2d_short(data2d_name, data2d_value)
            character(len=*), intent(in)               :: data2d_name
            integer(i_short), dimension(:), intent(in) :: data2d_value
            
            integer(i_long)                 :: var_index
            integer(i_llong)                :: input_size
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            integer(i_llong)                      :: data_value_size
            
            if (enable_action) then
                data_value_size = size(data2d_value)
                write(action_str, "(A, I0, A, I0, A, I0, A, I0, A)") &
                    "nc_diag_data2d_short(data2d_name = " // data2d_name // &
                    ", data2d_value = array with length of ", &
                    data_value_size, &
                    " [", &
                    data2d_value(1), &
                    " ... ", &
                    data2d_value(data_value_size), &
                    "]"
                call actionm(trim(action_str))
            end if
#endif
            
            if (diag_data2d_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
            end if
            
            var_index = nc_diag_data2d_lookup_var(data2d_name)
            
            if (var_index == -1) then
                ! First, check to make sure we can still define new variables.
                if (diag_data2d_store%def_lock) then
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                call nc_diag_data2d_expand
                
                diag_data2d_store%total = diag_data2d_store%total + 1
                
                diag_data2d_store%names(diag_data2d_store%total) = data2d_name
                diag_data2d_store%types(diag_data2d_store%total) = NLAYER_SHORT
                
                var_index = diag_data2d_store%total
            end if
            
            ! Get input size and do size checks!
            input_size = size(data2d_value)
            
            if ((diag_data2d_store%def_lock) .AND. &
                (size(data2d_value) > diag_data2d_store%max_lens(var_index))) then
                call error("Cannot expand variable size after locking variable definitions!")
            end if
            
            ! We just need to add one entry...
            call nc_diag_data2d_resize_iarr(var_index, 1)
            call nc_diag_data2d_resize_short(input_size)
            
            ! Now add the actual entry!
            diag_data2d_store%m_short(diag_data2d_store%acount(2) - input_size + 1:diag_data2d_store%acount(2)) = &
                data2d_value
            diag_data2d_store%stor_i_arr(var_index)%index_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                diag_data2d_store%acount(2) - input_size + 1
            diag_data2d_store%stor_i_arr(var_index)%length_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                input_size
        end subroutine nc_diag_data2d_short
        
        ! nc_diag_data2d - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_data2d_long(data2d_name, data2d_value)
            character(len=*), intent(in)              :: data2d_name
            integer(i_long), dimension(:), intent(in) :: data2d_value
            
            integer(i_long)                 :: var_index
            integer(i_llong)                :: input_size
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            integer(i_llong)                      :: data_value_size
            
            if (enable_action) then
                data_value_size = size(data2d_value)
                write(action_str, "(A, I0, A, I0, A, I0, A, I0, A)") &
                    "nc_diag_data2d_long(data2d_name = " // data2d_name // &
                    ", data2d_value = array with length of ", &
                    data_value_size, &
                    " [", &
                    data2d_value(1), &
                    " ... ", &
                    data2d_value(data_value_size), &
                    "]"
                call actionm(trim(action_str))
            end if
#endif
            
            if (diag_data2d_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
            end if
            
            var_index = nc_diag_data2d_lookup_var(data2d_name)
            
            if (var_index == -1) then
                ! First, check to make sure we can still define new variables.
                if (diag_data2d_store%def_lock) then
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                call nc_diag_data2d_expand
                
                diag_data2d_store%total = diag_data2d_store%total + 1
                
                diag_data2d_store%names(diag_data2d_store%total) = data2d_name
                diag_data2d_store%types(diag_data2d_store%total) = NLAYER_LONG
                
                var_index = diag_data2d_store%total
            end if
            
#ifdef _DEBUG_MEM_
            call debug("Current total:")
            print *, diag_data2d_store%total
#endif
            
            ! Get input size and do size checks!
            input_size = size(data2d_value)
            
            if ((diag_data2d_store%def_lock) .AND. &
                (size(data2d_value) > diag_data2d_store%max_lens(var_index))) then
                call error("Cannot expand variable size after locking variable definitions!")
            end if
            
            ! We just need to add one entry...
            call nc_diag_data2d_resize_iarr(var_index, 1)
            call nc_diag_data2d_resize_long(input_size)
            
            ! Now add the actual entry!
            diag_data2d_store%m_long(diag_data2d_store%acount(3) - input_size + 1:diag_data2d_store%acount(3)) = &
                data2d_value
            diag_data2d_store%stor_i_arr(var_index)%index_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                diag_data2d_store%acount(3) - input_size + 1
            diag_data2d_store%stor_i_arr(var_index)%length_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                input_size
        end subroutine nc_diag_data2d_long
        
        ! nc_diag_data2d - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_data2d_rsingle(data2d_name, data2d_value)
            character(len=*), intent(in)             :: data2d_name
            real(r_single), dimension(:), intent(in) :: data2d_value

            integer(i_long)                 :: var_index
            integer(i_llong)                :: input_size
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            integer(i_llong)                      :: data_value_size
            
            if (enable_action) then
                data_value_size = size(data2d_value)
                write(action_str, "(A, I0, A, F0.5, A, F0.5, A)") &
                    "nc_diag_data2d_rsingle(data2d_name = " // data2d_name // &
                    ", data2d_value = array with length of ", &
                    data_value_size, &
                    " [", &
                    data2d_value(1), &
                    " ... ", &
                    data2d_value(data_value_size), &
                    "]"
                call actionm(trim(action_str))
            end if
#endif
            
            if (diag_data2d_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
            end if
            
            var_index = nc_diag_data2d_lookup_var(data2d_name)
            
            if (var_index == -1) then
                ! First, check to make sure we can still define new variables.
                if (diag_data2d_store%def_lock) then
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
#ifdef _DEBUG_MEM_
                write (*, "(A, A, A, F)") "NEW data2d: ", data2d_name, " | First value: ", data2d_value
#endif
                call nc_diag_data2d_expand
                
                diag_data2d_store%total = diag_data2d_store%total + 1
                
                diag_data2d_store%names(diag_data2d_store%total) = data2d_name
                diag_data2d_store%types(diag_data2d_store%total) = NLAYER_FLOAT
                
                var_index = diag_data2d_store%total
            end if
            
            ! Get input size and do size checks!
            input_size = size(data2d_value)
            
            if ((diag_data2d_store%def_lock) .AND. &
                (size(data2d_value) > diag_data2d_store%max_lens(var_index))) then
                call error("Cannot expand variable size after locking variable definitions!")
            end if
            
            ! We just need to add one entry...
            call nc_diag_data2d_resize_iarr(var_index, 1)
            call nc_diag_data2d_resize_rsingle(input_size)
            
            ! Now add the actual entry!
            diag_data2d_store%m_rsingle(diag_data2d_store%acount(4) - input_size + 1:diag_data2d_store%acount(4)) = &
                data2d_value
            diag_data2d_store%stor_i_arr(var_index)%index_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                diag_data2d_store%acount(4) - input_size + 1
            diag_data2d_store%stor_i_arr(var_index)%length_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                input_size
        end subroutine nc_diag_data2d_rsingle
        
        ! nc_diag_data2d - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_data2d_rdouble(data2d_name, data2d_value)
            character(len=*), intent(in)             :: data2d_name
            real(r_double), dimension(:), intent(in) :: data2d_value

            integer(i_long)                 :: var_index
            integer(i_llong)                :: input_size
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            integer(i_llong)                      :: data_value_size
            
            if (enable_action) then
                data_value_size = size(data2d_value)
                write(action_str, "(A, I0, A, F0.5, A, F0.5, A)") &
                    "nc_diag_data2d_rdouble(data2d_name = " // data2d_name // &
                    ", data2d_value = array with length of ", &
                    data_value_size, &
                    " [", &
                    data2d_value(1), &
                    " ... ", &
                    data2d_value(data_value_size), &
                    "]"
                call actionm(trim(action_str))
            end if
#endif
            
            if (diag_data2d_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
            end if
            
            var_index = nc_diag_data2d_lookup_var(data2d_name)
            
            if (var_index == -1) then
                ! First, check to make sure we can still define new variables.
                if (diag_data2d_store%def_lock) then
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                call nc_diag_data2d_expand
                
                diag_data2d_store%total = diag_data2d_store%total + 1
                
                diag_data2d_store%names(diag_data2d_store%total) = data2d_name
                diag_data2d_store%types(diag_data2d_store%total) = NLAYER_DOUBLE
                
                var_index = diag_data2d_store%total
            end if
            
            ! Get input size and do size checks!
            input_size = size(data2d_value)
            
            if ((diag_data2d_store%def_lock) .AND. &
                (size(data2d_value) > diag_data2d_store%max_lens(var_index))) then
                call error("Cannot expand variable size after locking variable definitions!")
            end if
            
            ! We just need to add one entry...
            call nc_diag_data2d_resize_iarr(var_index, 1)
            call nc_diag_data2d_resize_rdouble(input_size)
            
            ! Now add the actual entry!
            diag_data2d_store%m_rdouble(diag_data2d_store%acount(5) - input_size + 1:diag_data2d_store%acount(5)) = &
                data2d_value
            diag_data2d_store%stor_i_arr(var_index)%index_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                diag_data2d_store%acount(5) - input_size + 1
            diag_data2d_store%stor_i_arr(var_index)%length_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                input_size
        end subroutine nc_diag_data2d_rdouble

        ! nc_diag_data2d - input character(len=*)
        ! Corresponding NetCDF4 type: string? char?
        subroutine nc_diag_data2d_string(data2d_name, data2d_value)
            character(len=*), intent(in)               :: data2d_name
            character(len=*), dimension(:), intent(in) :: data2d_value
            
            integer(i_long)                 :: var_index
            integer(i_long)                 :: max_str_len
            integer(i_llong)                :: input_size
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            integer(i_llong)                      :: data_value_size
            
            if (enable_action) then
                data_value_size = size(data2d_value)
                write(action_str, "(A, I0, A, A)") &
                    "nc_diag_data2d_string(data2d_name = " // data2d_name // &
                    ", data2d_value = array with length of ", &
                    data_value_size, &
                    " [" // &
                        trim(data2d_value(1)) // &
                        " ... " // &
                        trim(data2d_value(data_value_size)) // &
                        "]"
                call actionm(trim(action_str))
            end if
#endif
            
            if (diag_data2d_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
            end if
            
            var_index = nc_diag_data2d_lookup_var(data2d_name)
            
            if (var_index == -1) then
                ! First, check to make sure we can still define new variables.
                if (diag_data2d_store%def_lock) then
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                call nc_diag_data2d_expand
                
                diag_data2d_store%total = diag_data2d_store%total + 1
                
                diag_data2d_store%names(diag_data2d_store%total) = data2d_name
                diag_data2d_store%types(diag_data2d_store%total) = NLAYER_STRING
                
                var_index = diag_data2d_store%total
            else
                ! Check max string length
#ifdef _DEBUG_MEM_
                print *, "len_trim(data2d_value) = ", len_trim(data2d_value)
                print *, "diag_data2d_store%max_str_lens(var_index) = ", diag_data2d_store%max_str_lens(var_index)
#endif
            end if
            
            ! Get input size and do size checks!
            input_size = size(data2d_value)
            
            if (diag_data2d_store%def_lock) then
                if (input_size > diag_data2d_store%max_lens(var_index)) &
                    call error("Cannot expand variable size after locking variable definitions!")
                
                ! Check max string length
                max_str_len = max_len_string_array(data2d_value, &
                    int(input_size))
                
#ifdef _DEBUG_MEM_
                print *, "max_str_len: ", max_str_len
                print *, "diag_data2d_store%max_str_lens(var_index): ", diag_data2d_store%max_str_lens(var_index)
#endif
                
                if (max_str_len > diag_data2d_store%max_str_lens(var_index)) &
                    call error("Cannot expand variable string length after locking variable definitions!")
            end if
            
            ! We just need to add one entry...
            call nc_diag_data2d_resize_iarr(var_index, 1)
            call nc_diag_data2d_resize_string(input_size)
            
            ! Now add the actual entry!
            diag_data2d_store%m_string(diag_data2d_store%acount(6) - input_size + 1:diag_data2d_store%acount(6)) = &
                data2d_value
            diag_data2d_store%stor_i_arr(var_index)%index_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                diag_data2d_store%acount(6) - input_size + 1
            diag_data2d_store%stor_i_arr(var_index)%length_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                input_size
        end subroutine nc_diag_data2d_string

