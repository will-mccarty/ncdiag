        !===============================================================
        ! nc_diag_metadata - metadata handling (implementation)
        !===============================================================
        ! DO NOT COMPILE THIS DIRECTLY! THIS IS MEANT TO BE INCLUDED
        ! INSIDE A LARGER F90 SOURCE!
        ! If you compile this directly, you WILL face the WRATH of your
        ! compiler!
        !---------------------------------------------------------------
        ! Depends on:
        !   netcdf_mresize.F90, netcdf_realloc_metadata.f90
        ! 
        ! Technically, order shouldn't matter... but just in case,
        ! include netcdf_realloc_imp.f90 FIRST!
        !---------------------------------------------------------------
        ! nc_diag_metadata stores metadata data as NetCDF4 global
        ! attributes. The nc_diag_metadata subroutines temporarily cache
        ! any metadata data until write, where it will be set by
        ! NF90_PUT_ATT().
        !---------------------------------------------------------------
        ! This file provides the actual metadata data adding subroutines,
        ! referred to by the interface. It also provides support
        ! subroutines for metadata writing and allocation setup.
        
        subroutine nc_diag_metadata_allocmulti(multiplier)
            integer(i_long), intent(in)    :: multiplier
            if (init_done) then
                ! # of times we needed to realloc simple metadata
                ! also the multiplier factor for allocation (2^x)
                diag_metadata_store%alloc_s_multi = multiplier
                
                ! # of times we needed to realloc metadata data storage
                ! also the multiplier factor for allocation (2^x)
                diag_metadata_store%alloc_m_multi = multiplier
                
                ! # of times we needed to realloc metadata INDEX data storage
                ! also the multiplier factor for allocation (2^x)
                diag_metadata_store%alloc_mi_multi = multiplier
            end if
        end subroutine nc_diag_metadata_allocmulti
        
        subroutine nc_diag_metadata_write_def(internal)
            logical, intent(in), optional         :: internal
            
            integer(i_byte)                       :: data_type
            character(len=100)                    :: data_name
            
            integer(i_llong)                      :: curdatindex, j
            integer(i_kind)                       :: nc_data_type
            integer(i_long)                       :: tmp_dim_id
            character(len=120)                    :: data_dim_name
            
            character(len=:),         allocatable :: string_arr(:)
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (enable_action) then
                if (present(internal)) then
                    write(action_str, "(A, L, A)") "nc_diag_metadata_write_def(internal = ", internal, ")"
                else
                    write(action_str, "(A)") "nc_diag_metadata_write_def(internal = (not specified))"
                end if
                call actionm(trim(action_str))
            end if
#endif
            
            if (init_done) then
                if (.NOT. diag_metadata_store%def_lock) then
                    ! Use global nobs ID!
                    ! Call subroutine to ensure the nobs dim is created already...
                    call nc_diag_varattr_make_nobs_dim
                    
                    do curdatindex = 1, diag_metadata_store%total
                        data_name = diag_metadata_store%names(curdatindex)
                        data_type = diag_metadata_store%types(curdatindex)
                        
                        call info("metadata: defining " // trim(data_name))
                        
                        if (data_type == NLAYER_BYTE)   nc_data_type = NF90_BYTE
                        if (data_type == NLAYER_SHORT)  nc_data_type = NF90_SHORT
                        if (data_type == NLAYER_LONG)   nc_data_type = NF90_INT
                        if (data_type == NLAYER_FLOAT)  nc_data_type = NF90_FLOAT
                        if (data_type == NLAYER_DOUBLE) nc_data_type = NF90_DOUBLE
                        if (data_type == NLAYER_STRING) nc_data_type = NF90_CHAR
                        
#ifdef _DEBUG_MEM_
                        print *, "metadata part 1"
#endif
                        
                        if (data_type == NLAYER_STRING) then
                            write (data_dim_name, "(A, A)") trim(data_name), "_maxstrlen"
                            
                            ! If trimming is enabled, we haven't found our max_str_len yet.
                            ! Go find it!
                            if (enable_trim) then
                                ! Dimension is # of chars by # of obs (unlimited)
                                allocate(character(10000) :: string_arr(diag_metadata_store%stor_i_arr(curdatindex)%icount))
                                do j = 1, diag_metadata_store%stor_i_arr(curdatindex)%icount
                                    string_arr(j) = diag_metadata_store%m_string(diag_metadata_store%stor_i_arr(curdatindex)%index_arr(j))
                                end do
                                
                                ! Save the max string len
                                diag_metadata_store%max_str_lens(curdatindex) = max_len_string_array(string_arr, &
                                    diag_metadata_store%stor_i_arr(curdatindex)%icount)
                                
                                deallocate(string_arr)
                            end if
                            
                            call check(nf90_def_dim(ncid, data_dim_name, &
                                diag_metadata_store%max_str_lens(curdatindex), tmp_dim_id))
                            
#ifdef _DEBUG_MEM_
                            print *, "Defining char var type..."
#endif
                            
                            call check(nf90_def_var(ncid, data_name, nc_data_type, &
                                (/ tmp_dim_id, diag_varattr_store%nobs_dim_id /), &
                                diag_metadata_store%var_ids(curdatindex)))
                            
#ifdef _DEBUG_MEM_
                            print *, "Done defining char var type..."
#endif
                        else
                            call check(nf90_def_var(ncid, data_name, nc_data_type, diag_varattr_store%nobs_dim_id, &
                                diag_metadata_store%var_ids(curdatindex)))
                        end if
                        
#ifdef _DEBUG_MEM_
                        print *, "metadata part 2"
#endif
                        
                        call nc_diag_varattr_add_var(diag_metadata_store%names(curdatindex), &
                                    diag_metadata_store%types(curdatindex), &
                                    diag_metadata_store%var_ids(curdatindex))
                        
                        ! Enable compression
                        ! Args: ncid, varid, enable_shuffle (yes), enable_deflate (yes), deflate_level
#ifdef _DEBUG_MEM_
                        print *, "Defining compression 1 (chunking)..."
#endif
                        
                        if (data_type == NLAYER_STRING) then
                            call check(nf90_def_var_chunking(ncid, diag_metadata_store%var_ids(curdatindex), &
                                NF90_CHUNKED, (/ diag_metadata_store%max_str_lens(curdatindex), NLAYER_CHUNKING /)))
                        else
                            call check(nf90_def_var_chunking(ncid, diag_metadata_store%var_ids(curdatindex), &
                                NF90_CHUNKED, (/ NLAYER_CHUNKING /)))
                        end if
                        
#ifdef _DEBUG_MEM_
                        print *, "Defining compression 2 (gzip)..."
#endif
                        call check(nf90_def_var_deflate(ncid, diag_metadata_store%var_ids(curdatindex), &
                            1, 1, NLAYER_COMPRESSION))
                        
#ifdef _DEBUG_MEM_
                        print *, "Done defining compression..."
#endif
                        
                        ! Lock the definitions!
                        diag_metadata_store%def_lock = .TRUE.
                    end do
                else
                    if(.NOT. present(internal)) &
                        call error("Can't write definitions - definitions have already been written and locked!")
                end if
            end if
        end subroutine nc_diag_metadata_write_def
        
        subroutine nc_diag_metadata_write_data(flush_data_only)
            ! Optional internal flag to only flush data - if this is
            ! true, data flushing will be performed, and the data will
            ! NOT be locked.
            logical, intent(in), optional         :: flush_data_only
            
            integer(i_byte)                       :: data_type
            character(len=100)                    :: data_name
            
            integer(i_long)                       :: curdatindex, j
            
            integer(i_byte), dimension(:), allocatable :: byte_arr
            integer(i_short),dimension(:), allocatable :: short_arr
            integer(i_long), dimension(:), allocatable :: long_arr
            real(r_single),  dimension(:), allocatable :: rsingle_arr
            real(r_double),  dimension(:), allocatable :: rdouble_arr
            character(len=:),              allocatable :: string_arr(:)
            
            integer(i_llong)                           :: string_arr_maxlen
            
            integer(i_llong)                           :: data_length_counter
            character(len=100)                         :: counter_data_name
            integer(i_llong)                           :: current_length_count
            character(len=1000)                        :: data_uneven_msg
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (enable_action) then
                if (present(flush_data_only)) then
                    write(action_str, "(A, L, A)") "nc_diag_metadata_write_data(flush_data_only = ", flush_data_only, ")"
                else
                    write(action_str, "(A)") "nc_diag_metadata_write_data(flush_data_only = (not specified))"
                end if
                call actionm(trim(action_str))
            end if
#endif
            ! Initialization MUST occur here, not in decl...
            ! Otherwise, it'll initialize once, and never again...
            ! 
            ! This will cause scary issues in the future, where closing
            ! and opening a new file shows strange errors about a file
            ! opened in the past...
            data_length_counter = -1
            current_length_count = -1
            
            if (init_done .AND. allocated(diag_metadata_store)) then
                if (.NOT. diag_metadata_store%data_lock) then
                    do curdatindex = 1, diag_metadata_store%total
#ifdef _DEBUG_MEM_
                        print *, curdatindex
#endif
                        data_name = diag_metadata_store%names(curdatindex)
                        data_type = diag_metadata_store%types(curdatindex)
                        
                        call info("metadata: writing " // trim(data_name))
                        
                        ! Warn about data inconsistencies
                        if (.NOT. (present(flush_data_only) .AND. flush_data_only)) then
                            current_length_count = diag_metadata_store%stor_i_arr(curdatindex)%icount + &
                                diag_metadata_store%rel_indexes(curdatindex)
                            
                            if (data_length_counter == -1) then
                                data_length_counter = current_length_count
                                counter_data_name = data_name
                            else
                                if (data_length_counter /= current_length_count) then
                                    ! Show message!
                                    ! NOTE - I0 and TRIM are Fortran 95 specs
                                    write (data_uneven_msg, "(A, I0, A, I0, A)") "Amount of data written in " // &
                                        trim(data_name) // " (", &
                                        current_length_count, &
                                        ")" // char(10) // &
                                        "             differs from variable " // trim(counter_data_name) // &
                                        " (", data_length_counter, ")!"
                                    
                                    if (diag_metadata_store%strict_check) then
                                        call error(trim(data_uneven_msg))
                                    else
                                        call warning(trim(data_uneven_msg))
                                    end if
                                end if
                            end if
                        end if
                        
                        ! Make sure we have data to write in the first place!
                        if (diag_metadata_store%stor_i_arr(curdatindex)%icount > 0) then
                            if (data_type == NLAYER_BYTE) then
                                allocate(byte_arr(diag_metadata_store%stor_i_arr(curdatindex)%icount))
                                do j = 1, diag_metadata_store%stor_i_arr(curdatindex)%icount
                                    byte_arr(j) = diag_metadata_store%m_byte(diag_metadata_store%stor_i_arr(curdatindex)%index_arr(j))
                                end do
                                call check(nf90_put_var(&
                                    ncid, diag_metadata_store%var_ids(curdatindex), &
                                    byte_arr, &
                                    (/ 1 + diag_metadata_store%rel_indexes(curdatindex) /) &
                                    ))
                                
                                deallocate(byte_arr)
                            else if (data_type == NLAYER_SHORT) then
                                allocate(short_arr(diag_metadata_store%stor_i_arr(curdatindex)%icount))
                                do j = 1, diag_metadata_store%stor_i_arr(curdatindex)%icount
                                    short_arr(j) = diag_metadata_store%m_short(diag_metadata_store%stor_i_arr(curdatindex)%index_arr(j))
                                end do
                                call check(nf90_put_var(&
                                    ncid, diag_metadata_store%var_ids(curdatindex), &
                                    short_arr, &
                                    (/ 1 + diag_metadata_store%rel_indexes(curdatindex) /) &
                                    ))
                                
                                deallocate(short_arr)
                            else if (data_type == NLAYER_LONG) then
                                allocate(long_arr(diag_metadata_store%stor_i_arr(curdatindex)%icount))
                                do j = 1, diag_metadata_store%stor_i_arr(curdatindex)%icount
                                    long_arr(j) = diag_metadata_store%m_long(diag_metadata_store%stor_i_arr(curdatindex)%index_arr(j))
                                end do
                                
                                call check(nf90_put_var(&
                                    ncid, diag_metadata_store%var_ids(curdatindex), &
                                    long_arr, &
                                    (/ 1 + diag_metadata_store%rel_indexes(curdatindex) /) &
                                    ))
                                
                                deallocate(long_arr)
                            else if (data_type == NLAYER_FLOAT) then
                                allocate(rsingle_arr(diag_metadata_store%stor_i_arr(curdatindex)%icount))
                                !write (*, "(A, A, A, I0)") "start queue: ", trim(data_name), " - length ", diag_metadata_store%stor_i_arr(curdatindex)%icount
                                do j = 1, diag_metadata_store%stor_i_arr(curdatindex)%icount
                                    !errcode = nf90_put_var(&
                                    !    ncid, diag_metadata_store%var_ids(curdatindex), &
                                    !    diag_metadata_store%m_rsingle(diag_metadata_store%stor_i_arr(curdatindex)%index_arr(j)), &
                                    !    (/ j /) &
                                    !    )
                                    rsingle_arr(j) = diag_metadata_store%m_rsingle(diag_metadata_store%stor_i_arr(curdatindex)%index_arr(j))
                                    !print *, asdf(j)
                                end do
                                !print *, "end queue / start put"
                                call check(nf90_put_var(&
                                        ncid, diag_metadata_store%var_ids(curdatindex), &
                                        rsingle_arr, &
                                        (/ 1 + diag_metadata_store%rel_indexes(curdatindex) /) &
                                        ))
                                !call check(nf90_sync(ncid))
                                deallocate(rsingle_arr)
                                !print *, "end put"
                                
                            else if (data_type == NLAYER_DOUBLE) then
                                allocate(rdouble_arr(diag_metadata_store%stor_i_arr(curdatindex)%icount))
                                do j = 1, diag_metadata_store%stor_i_arr(curdatindex)%icount
                                    rdouble_arr(j) = diag_metadata_store%m_rdouble(diag_metadata_store%stor_i_arr(curdatindex)%index_arr(j))
                                end do
                                
                                call check(nf90_put_var(&
                                    ncid, diag_metadata_store%var_ids(curdatindex), &
                                    rdouble_arr, &
                                    (/ 1 + diag_metadata_store%rel_indexes(curdatindex) /) &
                                    ))
                                deallocate(rdouble_arr)
                            else if (data_type == NLAYER_STRING) then
                                ! Only get maximum if we haven't already done that in the define step!
                                if (diag_metadata_store%max_str_lens(curdatindex) == -1) then
                                    allocate(character(10000) :: string_arr(diag_metadata_store%stor_i_arr(curdatindex)%icount))
                                    do j = 1, diag_metadata_store%stor_i_arr(curdatindex)%icount
                                        string_arr(j) = diag_metadata_store%m_string(diag_metadata_store%stor_i_arr(curdatindex)%index_arr(j))
                                    end do
                                    
                                    string_arr_maxlen = max_len_string_array(string_arr, &
                                        diag_metadata_store%stor_i_arr(curdatindex)%icount)
                                    
                                    deallocate(string_arr)
                                else
                                    string_arr_maxlen = diag_metadata_store%max_str_lens(curdatindex)
                                end if
                                
                                allocate(character(string_arr_maxlen) :: string_arr(diag_metadata_store%stor_i_arr(curdatindex)%icount))
                                do j = 1, diag_metadata_store%stor_i_arr(curdatindex)%icount
                                    string_arr(j) = diag_metadata_store%m_string(diag_metadata_store%stor_i_arr(curdatindex)%index_arr(j))
                                end do
                                
                                call check(nf90_put_var(&
                                    ncid, diag_metadata_store%var_ids(curdatindex), &
                                    string_arr, &
                                    (/ 1, 1 + diag_metadata_store%rel_indexes(curdatindex) /) &
                                    ))
                                deallocate(string_arr)
                            end if
                            
                            ! Check for data flushing, and if so, update the relative indexes
                            ! and set icount to 0.
                            if (present(flush_data_only) .AND. flush_data_only) then
                                diag_metadata_store%rel_indexes(curdatindex) = &
                                    diag_metadata_store%rel_indexes(curdatindex) + &
                                    diag_metadata_store%stor_i_arr(curdatindex)%icount
                                diag_metadata_store%stor_i_arr(curdatindex)%icount = 0
                                
#ifdef _DEBUG_MEM_
                                print *, "diag_metadata_store%rel_indexes(curdatindex) is now:"
                                print *, diag_metadata_store%rel_indexes(curdatindex)
#endif
                            end if
                            
                        end if
                    end do
                    
                    if (present(flush_data_only) .AND. flush_data_only) then
#ifdef _DEBUG_MEM_
                        print *, "In buffer flush mode!"
#endif
                        
                        ! We need to reset all array counts to zero!
                        diag_metadata_store%acount = 0
                    else
                        ! Lock data writing
                        diag_metadata_store%data_lock = .TRUE.
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
            print *, "All done writing metadata data"
#endif
        end subroutine nc_diag_metadata_write_data
        
        subroutine nc_diag_metadata_set_strict(enable_strict)
            logical, intent(in) :: enable_strict
            
            if (init_done .AND. allocated(diag_metadata_store)) then
                diag_metadata_store%strict_check = enable_strict
            else
                call error("Can't set strictness level for metadata - NetCDF4 layer not initialized yet!")
            end if
        end subroutine nc_diag_metadata_set_strict
        
        !subroutine nc_diag_metadata_write
        !    integer(i_byte)                       :: data_type
        !    logical                               :: data_vect
        !    integer(i_long), dimension(6)         :: data_type_index
        !    integer(i_long), dimension(5)         :: data_type_index_vi
        !    character(len=100)                    :: data_name
        !    
        !    integer(i_long)               :: curdatindex
        !    integer(i_long)               :: curdatvecsize
        !    
        !    if (init_done) then
        !        data_type_index    = (/ 1, 1, 1, 1, 1, 1 /)
        !        data_type_index_vi = (/ 1, 1, 1, 1, 1 /)
        !        do curdatindex = 1, diag_metadata_store%total
        !            data_name = diag_metadata_store%names(curdatindex)
        !            data_type = diag_metadata_store%types(curdatindex)
        !            data_vect = diag_metadata_store%vectored(curdatindex)
        !            
        !            if (data_type == NLAYER_BYTE) then
        !                if (data_vect) then
        !                    if (data_type_index_vi(1) <= diag_metadata_store%acount(7)) then
        !                        ! Grab the vector size, and allocate as needed.
        !                        curdatvecsize = diag_metadata_store%m_byte_vi(data_type_index_vi(1))
        !                        data_type_index_vi(1) = data_type_index_vi(1) + 1
        !                    else
        !                        call error("Critical error - byte index exceeds internal count!")
        !                    end if
        !                    
        !                    call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_metadata_store%m_byte(data_type_index(1):(data_type_index(1) + curdatvecsize - 1))))
        !                    data_type_index(1) = data_type_index(1) + curdatvecsize
        !                else
        !                    call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_metadata_store%m_byte(data_type_index(1))))
        !                    data_type_index(1) = data_type_index(1) + 1
        !                end if
        !            else if (data_type == NLAYER_SHORT) then
        !                if (data_vect) then
        !                    if (data_type_index_vi(2) <= diag_metadata_store%acount(8)) then
        !                        ! Grab the vector size, and allocate as needed.
        !                        curdatvecsize = diag_metadata_store%m_short_vi(data_type_index_vi(2))
        !                        data_type_index_vi(2) = data_type_index_vi(2) + 1
        !                    else
        !                        call error("Critical error - short index exceeds internal count!")
        !                    end if
        !                    
        !                    call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_metadata_store%m_short(data_type_index(2):(data_type_index(2) + curdatvecsize - 1))))
        !                    data_type_index(2) = data_type_index(2) + curdatvecsize
        !                else
        !                    call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_metadata_store%m_short(data_type_index(2))))
        !                    data_type_index(2) = data_type_index(2) + 1
        !                end if
        !            else if (data_type == NLAYER_LONG) then
        !                if (data_vect) then
        !                    if (data_type_index_vi(3) <= diag_metadata_store%acount(9)) then
        !                        ! Grab the vector size...
        !                        curdatvecsize = diag_metadata_store%m_long_vi(data_type_index_vi(3))
        !                        data_type_index_vi(3) = data_type_index_vi(3) + 1
        !                    else
        !                        call error("Critical error - long index exceeds internal count!")
        !                    end if
        !                    
        !                    call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_metadata_store%m_long(data_type_index(3):(data_type_index(3) + curdatvecsize - 1))))
        !                    data_type_index(3) = data_type_index(3) + curdatvecsize
        !                else
        !                    call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_metadata_store%m_long(data_type_index(3))))
        !                    data_type_index(3) = data_type_index(3) + 1
        !                end if
        !            else if (data_type == NLAYER_FLOAT) then
        !                if (data_vect) then
        !                    if (data_type_index_vi(4) <= diag_metadata_store%acount(10)) then
        !                        ! Grab the vector size, and allocate as needed.
        !                        curdatvecsize = diag_metadata_store%m_rsingle_vi(data_type_index_vi(4))
        !                        data_type_index_vi(4) = data_type_index_vi(4) + 1
        !                    else
        !                        call error("Critical error - rsingle index exceeds internal count!")
        !                    end if
        !                    
        !                    call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_metadata_store%m_rsingle(data_type_index(4):(data_type_index(4) + curdatvecsize - 1))))
        !                    data_type_index(4) = data_type_index(4) + curdatvecsize
        !                else
        !                    call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_metadata_store%m_rsingle(data_type_index(4))))
        !                    data_type_index(4) = data_type_index(4) + 1
        !                end if
        !            else if (data_type == NLAYER_DOUBLE) then
        !                if (data_vect) then
        !                    if (data_type_index_vi(5) <= diag_metadata_store%acount(11)) then
        !                        ! Grab the vector size, and allocate as needed.
        !                        curdatvecsize = diag_metadata_store%m_rdouble_vi(data_type_index_vi(5))
        !                        data_type_index_vi(5) = data_type_index_vi(5) + 1
        !                    else
        !                        call error("Critical error - rdouble index exceeds internal count!")
        !                    end if
        !                    
        !                    call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_metadata_store%m_rdouble(data_type_index(5):(data_type_index(5) + curdatvecsize - 1))))
        !                    data_type_index(5) = data_type_index(5) + curdatvecsize
        !                else
        !                    call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_metadata_store%m_rdouble(data_type_index(5))))
        !                    data_type_index(5) = data_type_index(5) + 1
        !                end if
        !            else if (data_type == NLAYER_STRING) then
        !                ! String array not available with NF90 attributes
        !                !if (data_vect) then
        !                !    if (data_type_index_vi(6) <= diag_metadata_store%acount(12)) then
        !                !        ! Grab the vector size, and allocate as needed.
        !                !        curdatvecsize = diag_metadata_store%m_string_vi(data_type_index_vi(6))
        !                !        data_type_index_vi(6) = data_type_index_vi(6) + 1
        !                !    else
        !                !        call error("Critical error - string index exceeds internal count!")
        !                !    end if
        !                !    
        !                !    data_type_index(6) = data_type_index(6) + curdatvecsize
        !                !    call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_metadata_store%m_string(data_type_index(6):(data_type_index(6) + curdatvecsize - 1))))
        !                !else
#ifdef _!DEBUG_MEM_
        !                    ! NOTE: trim() is F95
        !                    print *, "On curdatindex:"
        !                    print *, curdatindex
        !                    print *, "For variable:"
        !                    print *, trim(data_name)
        !                    print *, "Writing metadata string:"
        !                    print *, trim(diag_metadata_store%m_string(data_type_index(6)))
#endif  !
        !                    call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, trim(diag_metadata_store%m_string(data_type_index(6)))))
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
        !end subroutine nc_diag_metadata_write
        
        ! Preallocate variable name/type/etc. storage.
        subroutine nc_diag_metadata_prealloc_vars(num_of_addl_vars)
            integer(i_llong), intent(in)          :: num_of_addl_vars
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (enable_action) then
                write(action_str, "(A, I0, A)") "nc_diag_metadata_prealloc_vars(num_of_addl_vars = ", num_of_addl_vars, ")"
                call actionm(trim(action_str))
            end if
#endif
            if (init_done .AND. allocated(diag_metadata_store)) then
                if (allocated(diag_metadata_store%names)) then
                    if (diag_metadata_store%total >= size(diag_metadata_store%names)) then
                        call nc_diag_realloc(diag_metadata_store%names, num_of_addl_vars)
                    end if
                else
                    allocate(diag_metadata_store%names(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                end if
                
                if (allocated(diag_metadata_store%types)) then
                    if (diag_metadata_store%total >= size(diag_metadata_store%types)) then
                        call nc_diag_realloc(diag_metadata_store%types, num_of_addl_vars)
                    end if
                else
                    allocate(diag_metadata_store%types(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                end if
                
                if (allocated(diag_metadata_store%stor_i_arr)) then
                    if (diag_metadata_store%total >= size(diag_metadata_store%stor_i_arr)) then
                        call nc_diag_metadata_resize_iarr_type(num_of_addl_vars)
                    end if
                else
                    allocate(diag_metadata_store%stor_i_arr(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                end if
                
                if (allocated(diag_metadata_store%var_ids)) then
                    if (diag_metadata_store%total >= size(diag_metadata_store%var_ids)) then
                        call nc_diag_realloc(diag_metadata_store%var_ids, num_of_addl_vars)
                    end if
                else
                    allocate(diag_metadata_store%var_ids(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_metadata_store%var_ids = -1
                end if
                
                if (allocated(diag_metadata_store%alloc_sia_multi)) then
                    if (diag_metadata_store%total >= size(diag_metadata_store%alloc_sia_multi)) then
                        call nc_diag_realloc(diag_metadata_store%alloc_sia_multi, num_of_addl_vars)
                    end if
                else
                    allocate(diag_metadata_store%alloc_sia_multi(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_metadata_store%alloc_sia_multi = 0
                end if
                
                if (allocated(diag_metadata_store%max_str_lens)) then
                    if (diag_metadata_store%total >= size(diag_metadata_store%max_str_lens)) then
                        call nc_diag_realloc(diag_metadata_store%max_str_lens, num_of_addl_vars)
                    end if
                else
                    allocate(diag_metadata_store%max_str_lens(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_metadata_store%max_str_lens = -1
                end if
                
                if (allocated(diag_metadata_store%rel_indexes)) then
                    if (diag_metadata_store%total >= size(diag_metadata_store%rel_indexes)) then
                        call nc_diag_realloc(diag_metadata_store%rel_indexes, num_of_addl_vars)
                    end if
                else
                    allocate(diag_metadata_store%rel_indexes(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_metadata_store%rel_indexes = 0
                end if
                
                diag_metadata_store%prealloc_total = diag_metadata_store%prealloc_total + num_of_addl_vars
            else
                call error("NetCDF4 layer not initialized yet!")
            endif
        end subroutine nc_diag_metadata_prealloc_vars
        
        ! Preallocate actual variable data storage
        subroutine nc_diag_metadata_prealloc_vars_storage(nclayer_type, num_of_addl_slots)
            integer(i_byte), intent(in)           :: nclayer_type
            integer(i_llong), intent(in)          :: num_of_addl_slots
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (enable_action) then
                write(action_str, "(A, I0, A, I0, A)") "nc_diag_metadata_prealloc_vars_storage(nclayer_type = ", nclayer_type, ", num_of_addl_slots = ", num_of_addl_slots, ")"
                call actionm(trim(action_str))
            end if
#endif            
            
            if (nclayer_type == NLAYER_BYTE) then
                call nc_diag_metadata_resize_byte(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_SHORT) then
                call nc_diag_metadata_resize_short(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_LONG) then
                call nc_diag_metadata_resize_long(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_FLOAT) then
                call nc_diag_metadata_resize_rsingle(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_DOUBLE) then
                call nc_diag_metadata_resize_rdouble(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_STRING) then
                call nc_diag_metadata_resize_string(num_of_addl_slots, .FALSE.)
            else
                call error("Invalid type specified for variable storage preallocation!")
            end if
            
            ! resize nc_diag_metadata_resize_iarr ?
            
        end subroutine nc_diag_metadata_prealloc_vars_storage
        
        ! Preallocate index storage
        subroutine nc_diag_metadata_prealloc_vars_storage_all(num_of_addl_slots)
            integer(i_llong), intent(in)          :: num_of_addl_slots
            integer(i_long)                       :: i
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (enable_action) then
                write(action_str, "(A, I0, A)") "nc_diag_metadata_prealloc_vars_storage_all(num_of_addl_slots = ", num_of_addl_slots, ")"
                call actionm(trim(action_str))
            end if
#endif
            
            !print *, "PREALLOC IARR: "
            !print *, num_of_addl_slots
            
            do i = 1, diag_metadata_store%prealloc_total
                call nc_diag_metadata_resize_iarr(i, num_of_addl_slots, .FALSE.)
            end do
        end subroutine nc_diag_metadata_prealloc_vars_storage_all
        
        subroutine nc_diag_metadata_expand
            integer(i_llong) :: addl_fields
            
            ! Did we realloc at all?
            logical :: meta_realloc
            
            meta_realloc = .FALSE.
            
            if (init_done .AND. allocated(diag_metadata_store)) then
                addl_fields = 1 + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_metadata_store%alloc_s_multi))
                
#ifdef _DEBUG_MEM_
                call debug("INITIAL value of diag_metadata_store%alloc_s_multi:")
                print *, diag_metadata_store%alloc_s_multi
#endif
                
                if (allocated(diag_metadata_store%names)) then
                    if (diag_metadata_store%total >= size(diag_metadata_store%names)) then
#ifdef _DEBUG_MEM_
                        call debug("Reallocating diag_metadata_store%names...")
                        print *, (NLAYER_MULTI_BASE ** diag_metadata_store%alloc_s_multi)
                        print *, addl_fields
#endif
                        call nc_diag_realloc(diag_metadata_store%names, addl_fields)
#ifdef _DEBUG_MEM_
                    call debug("Reallocated diag_metadata_store%names. Size:")
                    print *, size(diag_metadata_store%names)
#endif
                        meta_realloc = .TRUE.
                    end if
                else
#ifdef _DEBUG_MEM_
                    call debug("Allocating diag_metadata_store%names for first time...")
                    print *, NLAYER_DEFAULT_ENT
#endif
                    
                    allocate(diag_metadata_store%names(NLAYER_DEFAULT_ENT))
                    
#ifdef _DEBUG_MEM_
                    call debug("Allocated diag_metadata_store%names. Size:")
                    print *, size(diag_metadata_store%names)
#endif
                end if
                
                if (allocated(diag_metadata_store%types)) then
                    if (diag_metadata_store%total >= size(diag_metadata_store%types)) then
#ifdef _DEBUG_MEM_
                        call debug("Reallocating diag_metadata_store%types...")
                        print *, (NLAYER_MULTI_BASE ** diag_metadata_store%alloc_s_multi)
                        print *, addl_fields
#endif
                        call nc_diag_realloc(diag_metadata_store%types, addl_fields)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_metadata_store%types(NLAYER_DEFAULT_ENT))
                end if
                
                if (allocated(diag_metadata_store%stor_i_arr)) then
                    if (diag_metadata_store%total >= size(diag_metadata_store%stor_i_arr)) then
#ifdef _DEBUG_MEM_
                        call debug("Reallocating diag_metadata_store%stor_i_arr...")
                        print *, (NLAYER_MULTI_BASE ** diag_metadata_store%alloc_s_multi)
                        print *, (1 + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_metadata_store%alloc_s_multi)))
#endif
                        call nc_diag_metadata_resize_iarr_type(addl_fields)
                        
                        !call nc_diag_realloc(diag_metadata_store%stor_i_arr, 1 + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_metadata_store%alloc_s_multi)))
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_metadata_store%stor_i_arr(NLAYER_DEFAULT_ENT))
                end if
                
                if (allocated(diag_metadata_store%var_ids)) then
                    if (diag_metadata_store%total >= size(diag_metadata_store%var_ids)) then
                        call nc_diag_realloc(diag_metadata_store%var_ids, addl_fields)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_metadata_store%var_ids(NLAYER_DEFAULT_ENT))
                    diag_metadata_store%var_ids = -1
                end if
                
                if (allocated(diag_metadata_store%alloc_sia_multi)) then
                    if (diag_metadata_store%total >= size(diag_metadata_store%alloc_sia_multi)) then
                        call nc_diag_realloc(diag_metadata_store%alloc_sia_multi, addl_fields)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_metadata_store%alloc_sia_multi(NLAYER_DEFAULT_ENT))
                    diag_metadata_store%alloc_sia_multi = 0
                end if
                
                if (allocated(diag_metadata_store%max_str_lens)) then
                    if (diag_metadata_store%total >= size(diag_metadata_store%max_str_lens)) then
                        call nc_diag_realloc(diag_metadata_store%max_str_lens, addl_fields)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_metadata_store%max_str_lens(NLAYER_DEFAULT_ENT))
                    diag_metadata_store%max_str_lens = -1
                end if
                
                if (allocated(diag_metadata_store%rel_indexes)) then
                    if (diag_metadata_store%total >= size(diag_metadata_store%rel_indexes)) then
                        call nc_diag_realloc(diag_metadata_store%rel_indexes, addl_fields)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_metadata_store%rel_indexes(NLAYER_DEFAULT_ENT))
                    diag_metadata_store%rel_indexes = 0
                end if
                
                if (meta_realloc) then
                    !diag_metadata_store%alloc_s_count = diag_metadata_store%alloc_s_count + 1
                    diag_metadata_store%alloc_s_multi = diag_metadata_store%alloc_s_multi + 1
#ifdef _DEBUG_MEM_
                    print *, "Incrementing alloc_s_multi... new value:"
                    print *, diag_metadata_store%alloc_s_multi
#endif
                    !print *, "Size of new stuff:"
                    !print *, size(diag_metadata_store%names)
                endif
            else
                call error("NetCDF4 layer not initialized yet!")
            endif
            
        end subroutine nc_diag_metadata_expand
        
        function nc_diag_metadata_lookup_var(metadata_name) result(ind)
            character(len=*), intent(in)    :: metadata_name
            integer :: i, ind
            
            ind = -1
            
            if (init_done .AND. allocated(diag_metadata_store)) then
                do i = 1, diag_metadata_store%total
                    if (diag_metadata_store%names(i) == metadata_name) then
                        ind = i
                        exit
                    end if
                end do
            end if
        end function nc_diag_metadata_lookup_var
        
        ! nc_diag_metadata - input integer(i_byte)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_metadata_byte(metadata_name, metadata_value)
            character(len=*), intent(in)    :: metadata_name
            integer(i_byte), intent(in)     :: metadata_value
            
            integer(i_long)                 :: var_index
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (enable_action) then
                write(action_str, "(A, I0, A)") "nc_diag_metadata_byte(metadata_name = " // metadata_name // ", metadata_value = ", metadata_value, ")"
                call actionm(trim(action_str))
            end if
#endif
            
            if (diag_metadata_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
            end if
            
            var_index = nc_diag_metadata_lookup_var(metadata_name)
            
            if (var_index == -1) then
                ! First, check to make sure we can still define new variables.
                if (diag_metadata_store%def_lock) then
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                call nc_diag_metadata_expand
                
                diag_metadata_store%total = diag_metadata_store%total + 1
                
                diag_metadata_store%names(diag_metadata_store%total) = metadata_name
                diag_metadata_store%types(diag_metadata_store%total) = NLAYER_BYTE
                
                var_index = diag_metadata_store%total
            end if
            
            ! We just need to add one entry...
            call nc_diag_metadata_resize_iarr(var_index, 1)
            call nc_diag_metadata_resize_byte(1)
            
            ! Now add the actual entry!
            diag_metadata_store%m_byte(diag_metadata_store%acount(1)) = metadata_value
            diag_metadata_store%stor_i_arr(var_index)%index_arr(diag_metadata_store%stor_i_arr(var_index)%icount) = &
                diag_metadata_store%acount(1)
        end subroutine nc_diag_metadata_byte
        
        ! nc_diag_metadata - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_metadata_short(metadata_name, metadata_value)
            character(len=*), intent(in)    :: metadata_name
            integer(i_short), intent(in)    :: metadata_value
            
            integer(i_long)                 :: var_index
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (enable_action) then
                write(action_str, "(A, I0, A)") "nc_diag_metadata_short(metadata_name = " // metadata_name // ", metadata_value = ", metadata_value, ")"
                call actionm(trim(action_str))
            end if
#endif
            
            if (diag_metadata_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
            end if
            
            var_index = nc_diag_metadata_lookup_var(metadata_name)
            
            if (var_index == -1) then
                ! First, check to make sure we can still define new variables.
                if (diag_metadata_store%def_lock) then
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                call nc_diag_metadata_expand
                
                diag_metadata_store%total = diag_metadata_store%total + 1
                
                diag_metadata_store%names(diag_metadata_store%total) = metadata_name
                diag_metadata_store%types(diag_metadata_store%total) = NLAYER_SHORT
                
                var_index = diag_metadata_store%total
            end if
            
            ! We just need to add one entry...
            call nc_diag_metadata_resize_iarr(var_index, 1)
            call nc_diag_metadata_resize_short(1)
            
            ! Now add the actual entry!
            diag_metadata_store%m_short(diag_metadata_store%acount(2)) = metadata_value
            diag_metadata_store%stor_i_arr(var_index)%index_arr(diag_metadata_store%stor_i_arr(var_index)%icount) = &
                diag_metadata_store%acount(2)
        end subroutine nc_diag_metadata_short
        
        ! nc_diag_metadata - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_metadata_long(metadata_name, metadata_value)
            character(len=*), intent(in)    :: metadata_name
            integer(i_long), intent(in)     :: metadata_value
            
            integer(i_long)                 :: var_index
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (enable_action) then
                write(action_str, "(A, I0, A)") "nc_diag_metadata_long(metadata_name = " // metadata_name // ", metadata_value = ", metadata_value, ")"
                call actionm(trim(action_str))
            end if
#endif
            
            if (diag_metadata_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
            end if
            
            var_index = nc_diag_metadata_lookup_var(metadata_name)
            
            if (var_index == -1) then
                ! First, check to make sure we can still define new variables.
                if (diag_metadata_store%def_lock) then
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                call nc_diag_metadata_expand
                
                diag_metadata_store%total = diag_metadata_store%total + 1
                
                diag_metadata_store%names(diag_metadata_store%total) = metadata_name
                diag_metadata_store%types(diag_metadata_store%total) = NLAYER_LONG
                
                var_index = diag_metadata_store%total
            end if
            
#ifdef _DEBUG_MEM_
            call debug("Current total:")
            print *, diag_metadata_store%total
#endif
            
            ! We just need to add one entry...
            call nc_diag_metadata_resize_iarr(var_index, 1)
            call nc_diag_metadata_resize_long(1)
            
            ! Now add the actual entry!
            diag_metadata_store%m_long(diag_metadata_store%acount(3)) = metadata_value
            diag_metadata_store%stor_i_arr(var_index)%index_arr(diag_metadata_store%stor_i_arr(var_index)%icount) = &
                diag_metadata_store%acount(3)
        end subroutine nc_diag_metadata_long
        
        ! nc_diag_metadata - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_metadata_rsingle(metadata_name, metadata_value)
            character(len=*), intent(in)    :: metadata_name
            real(r_single), intent(in)      :: metadata_value

            integer(i_long)                 :: var_index
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (enable_action) then
                write(action_str, "(A, F0.5, A)") "nc_diag_metadata_rsingle(metadata_name = " // metadata_name // ", metadata_value = ", metadata_value, ")"
                call actionm(trim(action_str))
            end if
#endif
            
            if (diag_metadata_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
            end if
            
            var_index = nc_diag_metadata_lookup_var(metadata_name)
            
            if (var_index == -1) then
                ! First, check to make sure we can still define new variables.
                if (diag_metadata_store%def_lock) then
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
#ifdef _DEBUG_MEM_
                write (*, "(A, A, A, F)") "NEW METADATA: ", metadata_name, " | First value: ", metadata_value
#endif
                call nc_diag_metadata_expand
                
                diag_metadata_store%total = diag_metadata_store%total + 1
                
                diag_metadata_store%names(diag_metadata_store%total) = metadata_name
                diag_metadata_store%types(diag_metadata_store%total) = NLAYER_FLOAT
                
                var_index = diag_metadata_store%total
            end if
            
            ! We just need to add one entry...
            call nc_diag_metadata_resize_iarr(var_index, 1)
            call nc_diag_metadata_resize_rsingle(1)
            
            ! Now add the actual entry!
            diag_metadata_store%m_rsingle(diag_metadata_store%acount(4)) = metadata_value
            diag_metadata_store%stor_i_arr(var_index)%index_arr(diag_metadata_store%stor_i_arr(var_index)%icount) = &
                diag_metadata_store%acount(4)
        end subroutine nc_diag_metadata_rsingle
        
        ! nc_diag_metadata - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_metadata_rdouble(metadata_name, metadata_value)
            character(len=*), intent(in)    :: metadata_name
            real(r_double), intent(in)      :: metadata_value

            integer(i_long)                 :: var_index
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (enable_action) then
                write(action_str, "(A, F0.5, A)") "nc_diag_metadata_rdouble(metadata_name = " // metadata_name // ", metadata_value = ", metadata_value, ")"
                call actionm(trim(action_str))
            end if
#endif
            
            if (diag_metadata_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
            end if
            
            var_index = nc_diag_metadata_lookup_var(metadata_name)
            
            if (var_index == -1) then
                ! First, check to make sure we can still define new variables.
                if (diag_metadata_store%def_lock) then
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                call nc_diag_metadata_expand
                
                diag_metadata_store%total = diag_metadata_store%total + 1
                
                diag_metadata_store%names(diag_metadata_store%total) = metadata_name
                diag_metadata_store%types(diag_metadata_store%total) = NLAYER_DOUBLE
                
                var_index = diag_metadata_store%total
            end if
            
            ! We just need to add one entry...
            call nc_diag_metadata_resize_iarr(var_index, 1)
            call nc_diag_metadata_resize_rdouble(1)
            
            ! Now add the actual entry!
            diag_metadata_store%m_rdouble(diag_metadata_store%acount(5)) = metadata_value
            diag_metadata_store%stor_i_arr(var_index)%index_arr(diag_metadata_store%stor_i_arr(var_index)%icount) = &
                diag_metadata_store%acount(5)
        end subroutine nc_diag_metadata_rdouble

        ! nc_diag_metadata - input character(len=*)
        ! Corresponding NetCDF4 type: string? char?
        subroutine nc_diag_metadata_string(metadata_name, metadata_value)
            character(len=*), intent(in)    :: metadata_name
            character(len=*), intent(in)    :: metadata_value

            integer(i_long)                 :: var_index
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (enable_action) then
                write(action_str, "(A)") "nc_diag_metadata_string(metadata_name = " // metadata_name // ", metadata_value = " // trim(metadata_value) // ")"
                call actionm(trim(action_str))
            end if
#endif
            
            if (diag_metadata_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
            end if
            
            var_index = nc_diag_metadata_lookup_var(metadata_name)
            
            if (var_index == -1) then
                ! First, check to make sure we can still define new variables.
                if (diag_metadata_store%def_lock) then
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                call nc_diag_metadata_expand
                
                diag_metadata_store%total = diag_metadata_store%total + 1
                
                diag_metadata_store%names(diag_metadata_store%total) = metadata_name
                diag_metadata_store%types(diag_metadata_store%total) = NLAYER_STRING
                
                var_index = diag_metadata_store%total
            else
                ! Check max string length
#ifdef _DEBUG_MEM_
                print *, "len_trim(metadata_value) = ", len_trim(metadata_value)
                print *, "diag_metadata_store%max_str_lens(var_index) = ", diag_metadata_store%max_str_lens(var_index)
#endif
                if ((diag_metadata_store%def_lock) .AND. &
                    (len_trim(metadata_value) > diag_metadata_store%max_str_lens(var_index))) &
                    call error("Cannot expand variable string length after locking variable definitions!")
            end if
            
            ! We just need to add one entry...
            ! Strings can't be vectored (at least for attributes), so no 2nd argument
            ! here.
            call nc_diag_metadata_resize_iarr(var_index, 1)
            call nc_diag_metadata_resize_string(1)
            
            ! If trim isn't enabled, set our maximum string length here!
            if (.NOT. enable_trim) then
                if (diag_metadata_store%max_str_lens(var_index) == -1) then
                    diag_metadata_store%max_str_lens(var_index) = len(metadata_value)
                else
                    ! Validate that our non-first value isn't different from
                    ! the initial string length
                    if (diag_metadata_store%max_str_lens(var_index) /= len(metadata_value)) &
                        call error("Cannot change string size when trimming is disabled!")
                end if
            end if
            
            ! Now add the actual entry!
            diag_metadata_store%m_string(diag_metadata_store%acount(6)) = metadata_value
            diag_metadata_store%stor_i_arr(var_index)%index_arr(diag_metadata_store%stor_i_arr(var_index)%icount) = &
                diag_metadata_store%acount(6)
        end subroutine nc_diag_metadata_string

