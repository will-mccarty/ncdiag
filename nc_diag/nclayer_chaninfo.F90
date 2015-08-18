module nclayer_chaninfo
    use kinds, only: i_byte, i_short, i_long, i_llong, r_single, &
        r_double
    use nclayer_state, only: init_done, ncid, append_only, &
        enable_trim, &
        diag_chaninfo_store
    use nclayer_types, only: NLAYER_BYTE, NLAYER_SHORT, NLAYER_LONG, &
        NLAYER_FLOAT, NLAYER_DOUBLE, NLAYER_STRING, &
        NLAYER_FILL_BYTE, NLAYER_FILL_SHORT, NLAYER_FILL_LONG, &
        NLAYER_FILL_FLOAT, NLAYER_FILL_DOUBLE, NLAYER_FILL_CHAR, &
        NLAYER_COMPRESSION, NLAYER_DEFAULT_ENT, NLAYER_MULTI_BASE
    use nclayer_varattr, only: nc_diag_varattr_add_var
    use nclayer_strarrutils, only: max_len_string_array
    
    use nclayer_climsg, only: &
#ifdef ENABLE_ACTION_MSGS
        nclayer_enable_action, nclayer_actionm, &
#endif
        nclayer_error, nclayer_warning, nclayer_info, nclayer_check
    
    use nclayer_realloc, only: nc_diag_realloc
    use nclayer_ciresize, only: nc_diag_chaninfo_resize_byte, &
        nc_diag_chaninfo_resize_short, nc_diag_chaninfo_resize_long, &
        nc_diag_chaninfo_resize_rsingle, &
        nc_diag_chaninfo_resize_rdouble, nc_diag_chaninfo_resize_string
    
    use netcdf, only: nf90_inquire, nf90_inq_dimid, &
        nf90_inquire_dimension, nf90_inquire_variable, nf90_def_dim, &
        nf90_def_var, nf90_get_var, nf90_put_var, &
        nf90_def_var_deflate, nf90_def_var_chunking, &
        NF90_BYTE, NF90_SHORT, NF90_INT, NF90_FLOAT, NF90_DOUBLE, &
        NF90_CHAR, &
        NF90_EBADDIM, NF90_NOERR, NF90_MAX_NAME, NF90_CHUNKED
    
    implicit none
    
    !===============================================================
    ! nc_diag_chaninfo - channel info handling (declaration)
    !===============================================================
    ! DO NOT COMPILE THIS DIRECTLY! THIS IS MEANT TO BE INCLUDED
    ! INSIDE A LARGER F90 SOURCE!
    ! If you compile this directly, you WILL face the WRATH of your
    ! compiler!
    !---------------------------------------------------------------
    ! Depends on: nothing
    ! 
    ! (Note that the subroutines portion of this part of the program
    ! has dependencies - but the declaration part doesn't require
    ! anything!)
    !---------------------------------------------------------------
    ! nc_diag_chaninfo stuff stuff
    !---------------------------------------------------------------
    ! This file provides the actual subroutines, referred to by the
    ! interface.
    
    interface nc_diag_chaninfo
        module procedure nc_diag_chaninfo_byte, &
            nc_diag_chaninfo_short, nc_diag_chaninfo_long, &
            nc_diag_chaninfo_rsingle, nc_diag_chaninfo_rdouble, &
            nc_diag_chaninfo_string!, nc_diag_chaninfo_byte_v, &
            !nc_diag_chaninfo_short_v, nc_diag_chaninfo_long_v, &
            !nc_diag_chaninfo_rsingle_v, nc_diag_chaninfo_rdouble_v
    end interface nc_diag_chaninfo
    
    contains
        !===============================================================
        ! nc_diag_chaninfo - channel info handling (implementation)
        !===============================================================
        ! DO NOT COMPILE THIS DIRECTLY! THIS IS MEANT TO BE INCLUDED
        ! INSIDE A LARGER F90 SOURCE!
        ! If you compile this directly, you WILL face the WRATH of your
        ! compiler!
        !---------------------------------------------------------------
        ! Depends on:
        !   netcdf_realloc_decl.f90, netcdf_realloc_imp.f90
        ! 
        ! Technically, order shouldn't matter... but just in case,
        ! include netcdf_realloc_imp.f90 FIRST!
        !---------------------------------------------------------------
        ! nc_diag_chaninfo subroutines correspond to the global atributes,
        ! set by NF90_PUT_ATT()
        !---------------------------------------------------------------
        ! This file provides the actual subroutines, referred to by the
        ! interface.
        
        subroutine nc_diag_chaninfo_dim_set(nchans)
            integer(i_long), intent(in) :: nchans
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, I0, A)") "nc_diag_chaninfo_dim_set(nchans = ", nchans, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            if (init_done .AND. allocated(diag_chaninfo_store)) then
                if (nchans < 1) then
                    call nclayer_error("Critical error - specified a nchan < 1!")
                end if
                
                diag_chaninfo_store%nchans = nchans
            else
                call nclayer_error("NetCDF4 layer not initialized yet!")
            end if
        end subroutine nc_diag_chaninfo_dim_set
        
        !subroutine nc_diag_chaninfo(chaninfo_name, chaninfo_data)
        !    
        !end subroutine nc_diag_chaninfo
        
        subroutine nc_diag_chaninfo_allocmulti(multiplier)
            integer(i_long), intent(in)    :: multiplier
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, I0, A)") "nc_diag_chaninfo_allocmulti(multiplier = ", multiplier, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            if (init_done) then
                ! # of times we needed to realloc simple metadata
                ! also the multiplier factor for allocation (2^x)
                diag_chaninfo_store%alloc_multi = multiplier
            end if
        end subroutine nc_diag_chaninfo_allocmulti
        
        !!!!!
        !! TODO:
        !! (1) add all of the long fixes to the other types tomorrow!!!
        !!     DONE!
        !! (2) Write the subroutine below tomorrow!
        !! Make sure to warn when writing about low nchan, aka when
        !! the amount of data < nchan (since we're expecting a full write)
        !!!!!!
        
        subroutine nc_diag_chaninfo_load_def
            integer(i_long) :: ndims, nvars, var_index, type_index
            integer(i_long) :: rel_index, i, j
            
            character(len=NF90_MAX_NAME)               :: tmp_var_name
            integer(i_long)                            :: tmp_var_type, tmp_var_ndims
            
            integer(i_long), dimension(:), allocatable :: tmp_var_dimids, tmp_var_dim_sizes
            character(len=NF90_MAX_NAME) , allocatable :: tmp_var_dim_names(:)
            
            logical                                    :: is_nchans_var
            
            integer(i_byte),    dimension(:), allocatable     :: byte_buffer
            integer(i_short),   dimension(:), allocatable     :: short_buffer
            integer(i_long),    dimension(:), allocatable     :: long_buffer
            
            real(r_single),     dimension(:), allocatable     :: rsingle_buffer
            real(r_double),     dimension(:), allocatable     :: rdouble_buffer
            
            character(1),     dimension(:,:), allocatable     :: string_buffer
            
            integer(i_long)                                   :: dim_ierr
            
            ! Get top level info about the file!
            call nclayer_check(nf90_inquire(ncid, nDimensions = ndims, &
                nVariables = nvars))
            
            ! Fetch nchans first!
            dim_ierr = nf90_inq_dimid(ncid, "nchans", diag_chaninfo_store%nchans_dimid)
            
            ! Check if we found anything!
            ! If we got NF90_EBADDIM, then exit.
            if (dim_ierr == NF90_EBADDIM) then
                return
            else if (dim_ierr /= NF90_NOERR) then
                ! If an error besides not finding the dimension occurs,
                ! raise an exception.
                call nclayer_check(dim_ierr)
            end if
            
            ! Then grab nchans value...
            call nclayer_check(nf90_inquire_dimension(ncid, diag_chaninfo_store%nchans_dimid, &
                len = diag_chaninfo_store%nchans))
            
            ! Now search for variables that use nchans!
            ! Loop through each variable!
            do var_index = 1, nvars
                ! Grab number of dimensions and attributes first
                call nclayer_check(nf90_inquire_variable(ncid, var_index, name = tmp_var_name, ndims = tmp_var_ndims))
                
                ! Allocate temporary variable dimids storage!
                allocate(tmp_var_dimids(tmp_var_ndims))
                allocate(tmp_var_dim_names(tmp_var_ndims))
                allocate(tmp_var_dim_sizes(tmp_var_ndims))
                
                ! Grab the actual dimension IDs and attributes
                
                call nclayer_check(nf90_inquire_variable(ncid, var_index, dimids = tmp_var_dimids, &
                    xtype = tmp_var_type))
                
                if ((tmp_var_ndims == 1) .OR. &
                    ((tmp_var_ndims == 2) .AND. (tmp_var_type == NF90_CHAR))) then
                    is_nchans_var = .FALSE.
                    
                    do i = 1, tmp_var_ndims
                        call nclayer_check(nf90_inquire_dimension(ncid, tmp_var_dimids(i), tmp_var_dim_names(i), &
                            tmp_var_dim_sizes(i)))
                        
                        if (tmp_var_dim_names(i) == "nchans") is_nchans_var = .TRUE.
                    end do
                    
                    if (is_nchans_var) then
                        ! Expand things first!
                        call nc_diag_chaninfo_expand
                        
                        ! Add to the total!
                        diag_chaninfo_store%total = diag_chaninfo_store%total + 1
                        
                        ! Store name and type!
                        diag_chaninfo_store%names(diag_chaninfo_store%total) = trim(tmp_var_name)
                        
                        rel_index = 0
                        
                        if (tmp_var_type == NF90_BYTE) then
                            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_BYTE
                            call nc_diag_chaninfo_resize_byte(int8(diag_chaninfo_store%nchans), .FALSE.)
                            allocate(byte_buffer(diag_chaninfo_store%nchans))
                            call nclayer_check(nf90_get_var(ncid, var_index, byte_buffer))
                            
                            do j = diag_chaninfo_store%nchans, 1, -1
                                if (byte_buffer(j) /= NLAYER_FILL_BYTE) then
                                    exit
                                end if
                            end do
                            
                            rel_index = j
                            
                            deallocate(byte_buffer)
                            
                            type_index = 1
                        else if (tmp_var_type == NF90_SHORT) then
                            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_SHORT
                            call nc_diag_chaninfo_resize_short(int8(diag_chaninfo_store%nchans), .FALSE.)
                            allocate(short_buffer(diag_chaninfo_store%nchans))
                            call nclayer_check(nf90_get_var(ncid, var_index, short_buffer))
                            
                            do j = diag_chaninfo_store%nchans, 1, -1
                                if (short_buffer(j) /= NLAYER_FILL_SHORT) then
                                    exit
                                end if
                            end do
                            
                            rel_index = j
                            
                            deallocate(short_buffer)
                            
                            type_index = 2
                        else if (tmp_var_type == NF90_INT) then
                            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_LONG
                            call nc_diag_chaninfo_resize_long(int8(diag_chaninfo_store%nchans), .FALSE.)
                            allocate(long_buffer(diag_chaninfo_store%nchans))
                            call nclayer_check(nf90_get_var(ncid, var_index, long_buffer))
                            
                            do j = diag_chaninfo_store%nchans, 1, -1
                                if (long_buffer(j) /= NLAYER_FILL_LONG) then
                                    exit
                                end if
                            end do
                            
                            rel_index = j
                            
                            deallocate(long_buffer)
                            
                            type_index = 3
                        else if (tmp_var_type == NF90_FLOAT) then
                            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_FLOAT
                            call nc_diag_chaninfo_resize_rsingle(int8(diag_chaninfo_store%nchans), .FALSE.)
                            allocate(rsingle_buffer(diag_chaninfo_store%nchans))
                            call nclayer_check(nf90_get_var(ncid, var_index, rsingle_buffer))
                            
                            do j = diag_chaninfo_store%nchans, 1, -1
                                if (rsingle_buffer(j) /= NLAYER_FILL_FLOAT) then
                                    exit
                                end if
                            end do
                            
                            rel_index = j
                            
                            deallocate(rsingle_buffer)
                            
                            type_index = 4
                        else if (tmp_var_type == NF90_DOUBLE) then
                            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_DOUBLE
                            call nc_diag_chaninfo_resize_rdouble(int8(diag_chaninfo_store%nchans), .FALSE.)
                            allocate(rdouble_buffer(diag_chaninfo_store%nchans))
                            call nclayer_check(nf90_get_var(ncid, var_index, rdouble_buffer))
                            
                            do j = diag_chaninfo_store%nchans, 1, -1
                                if (rdouble_buffer(j) /= NLAYER_FILL_DOUBLE) then
                                    exit
                                end if
                            end do
                            
                            rel_index = j
                            
                            deallocate(rdouble_buffer)
                            
                            type_index = 5
                        else if (tmp_var_type == NF90_CHAR) then
                            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_STRING
                            call nc_diag_chaninfo_resize_string(int8(diag_chaninfo_store%nchans), .FALSE.)
                            allocate(string_buffer(diag_chaninfo_store%nchans, tmp_var_dim_sizes(1)))
                            call nclayer_check(nf90_get_var(ncid, var_index, string_buffer))
                            
                            do j = diag_chaninfo_store%nchans, 1, -1
                                if (string_buffer(j, 1) /= NLAYER_FILL_CHAR) then
                                    exit
                                end if
                            end do
                            
                            rel_index = j
                            
                            deallocate(string_buffer)
                            
                            ! Set max string length constraint
                            diag_chaninfo_store%max_str_lens(diag_chaninfo_store%total) = tmp_var_dim_sizes(1)
                            
                            type_index = 6
                        else
                            call nclayer_error("NetCDF4 type invalid!")
                        end if
                        
                        print *, trim(tmp_var_name), "rel index", rel_index
                        
                        ! Now add a relative position... based on the next position!
                        
                        ! First, increment the number of variables stored for this type:
                        diag_chaninfo_store%acount_v(type_index) = diag_chaninfo_store%acount_v(type_index) + 1
                        
                        ! Then, set the next variable's relative positioning,
                        ! based on the number of variables stored for this type.
                        diag_chaninfo_store%var_rel_pos(diag_chaninfo_store%total) = diag_chaninfo_store%acount_v(type_index)
                        
                        ! Initialize the amount of memory used to 0.
                        diag_chaninfo_store%var_usage(diag_chaninfo_store%total) = 0
                        
                        ! Set relative index!
                        diag_chaninfo_store%rel_indexes(diag_chaninfo_store%total) = rel_index
                        
                        ! Set variable ID! Note that var_index here is the actual variable ID.
                        diag_chaninfo_store%var_ids(diag_chaninfo_store%total) = var_index
                    end if
                    
                    !call nc_diag_cat_metadata_add_var(trim(tmp_var_name), tmp_var_type, tmp_var_ndims, tmp_var_dim_names)
                end if
                
                ! Deallocate
                deallocate(tmp_var_dimids)
                deallocate(tmp_var_dim_names)
                deallocate(tmp_var_dim_sizes)
            end do
            
            diag_chaninfo_store%def_lock = .TRUE.
        end subroutine nc_diag_chaninfo_load_def
        
        subroutine nc_diag_chaninfo_write_def(internal)
            logical, intent(in), optional :: internal
            
            ! Just write the definitions out!
            integer(i_llong)              :: curdatindex
            integer(i_byte)               :: data_type
            integer(i_long)               :: data_type_index
            character(len=100)            :: data_name
            integer(i_long)               :: nc_data_type
            
            integer(i_long)               :: tmp_dim_id
            character(len=120)            :: data_dim_name
            
            character(len=:), allocatable :: string_arr(:)
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                if (present(internal)) then
                    write(action_str, "(A, L, A)") "nc_diag_chaninfo_write_def(internal = ", internal, ")"
                else
                    write(action_str, "(A)") "nc_diag_chaninfo_write_def(internal = (not specified))"
                end if
                call nclayer_actionm(trim(action_str))
            end if
#endif
            
            if (init_done .AND. allocated(diag_chaninfo_store)) then
                if (diag_chaninfo_store%total > 0) then
                    if (diag_chaninfo_store%nchans /= -1) then
                        if (.NOT. diag_chaninfo_store%def_lock) then
                            ! First, set the dimensions... if necessary!
                            if (.NOT. append_only) &
                                call nclayer_check(nf90_def_dim(ncid, "nchans", diag_chaninfo_store%nchans, diag_chaninfo_store%nchans_dimid))
                            
                            ! Once we have the dimension, we can start writing
                            ! variable definitions!
                            do curdatindex = 1, diag_chaninfo_store%total
                                data_name = diag_chaninfo_store%names(curdatindex)
                                data_type = diag_chaninfo_store%types(curdatindex)
                                data_type_index = 1 + &
                                    ((diag_chaninfo_store%var_rel_pos(curdatindex) - 1) * diag_chaninfo_store%nchans)
                                
                                call nclayer_info("chaninfo: defining " // trim(data_name))
                                                            
                                if (data_type == NLAYER_BYTE)   nc_data_type = NF90_BYTE
                                if (data_type == NLAYER_SHORT)  nc_data_type = NF90_SHORT
                                if (data_type == NLAYER_LONG)   nc_data_type = NF90_INT
                                if (data_type == NLAYER_FLOAT)  nc_data_type = NF90_FLOAT
                                if (data_type == NLAYER_DOUBLE) nc_data_type = NF90_DOUBLE
                                if (data_type == NLAYER_STRING) nc_data_type = NF90_CHAR
                                
#ifdef _DEBUG_MEM_
                                print *, "chaninfo part 1"
#endif
                                
                                if (data_type == NLAYER_STRING) then
                                    write (data_dim_name, "(A, A)") trim(data_name), "_maxstrlen"
                                    
                                    ! Dimension is # of chars by # of obs (unlimited)
                                    allocate(character(10000) :: string_arr(diag_chaninfo_store%var_usage(curdatindex)))
                                    
                                    string_arr = diag_chaninfo_store%ci_string(data_type_index:(data_type_index + &
                                            diag_chaninfo_store%var_usage(curdatindex) - 1))
                                    
                                    ! If trimming is enabled, we haven't found our max_str_len yet.
                                    ! Go find it!
                                    if (enable_trim) then
                                        ! Save the max string len
                                        diag_chaninfo_store%max_str_lens(curdatindex) = &
                                            max_len_string_array(string_arr, diag_chaninfo_store%var_usage(curdatindex))
                                    end if
                                    
                                    if (.NOT. append_only) &
                                        call nclayer_check(nf90_def_dim(ncid, data_dim_name, &
                                            diag_chaninfo_store%max_str_lens(curdatindex), &
                                            tmp_dim_id))
#ifdef _DEBUG_MEM_
                                    print *, "Defining char var type..."
#endif
                                    if (.NOT. append_only) &
                                        call nclayer_check(nf90_def_var(ncid, diag_chaninfo_store%names(curdatindex), &
                                            nc_data_type, (/ tmp_dim_id, diag_chaninfo_store%nchans_dimid /), &
                                            diag_chaninfo_store%var_ids(curdatindex)))
#ifdef _DEBUG_MEM_
                                    print *, "Done defining char var type..."
#endif
                                    deallocate(string_arr)
                                else
                                    if (.NOT. append_only) &
                                        call nclayer_check(nf90_def_var(ncid, diag_chaninfo_store%names(curdatindex), &
                                            nc_data_type, diag_chaninfo_store%nchans_dimid, &
                                            diag_chaninfo_store%var_ids(curdatindex)))
                                end if
                                
#ifdef _DEBUG_MEM_
                                print *, "chaninfo part 2"
#endif
                                
                                call nc_diag_varattr_add_var(diag_chaninfo_store%names(curdatindex), &
                                    diag_chaninfo_store%types(curdatindex), &
                                    diag_chaninfo_store%var_ids(curdatindex))
                                
                                if (.NOT. append_only) then
                                    if (data_type == NLAYER_STRING) then
                                        call nclayer_check(nf90_def_var_chunking(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                            NF90_CHUNKED, (/ diag_chaninfo_store%max_str_lens(curdatindex), diag_chaninfo_store%nchans /)))
                                    else
                                        call nclayer_check(nf90_def_var_chunking(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                            NF90_CHUNKED, (/ diag_chaninfo_store%nchans /)))
                                    end if
                                    
                                    ! Enable compression
                                    ! Args: ncid, varid, enable_shuffle (yes), enable_deflate (yes), deflate_level
                                    call nclayer_check(nf90_def_var_deflate(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                        1, 1, NLAYER_COMPRESSION))
                                end if
                            end do
                            
                            ! Lock the definitions!
                            diag_chaninfo_store%def_lock = .TRUE.
                        else
                            if(.NOT. present(internal)) &
                                call nclayer_error("Can't write definitions - definitions have already been written and locked!")
                        end if
                    else
                        call nclayer_error("Can't write definitions - number of chans not set yet!")
                    end if
                end if
            else
                call nclayer_error("Can't write definitions - NetCDF4 layer not initialized yet!")
            end if
        end subroutine nc_diag_chaninfo_write_def
        
        subroutine nc_diag_chaninfo_write_data(flush_data_only)
            ! Optional internal flag to only flush data - if this is
            ! true, data flushing will be performed, and the data will
            ! NOT be locked.
            logical, intent(in), optional         :: flush_data_only
            
            integer(i_byte)                       :: data_type
            integer(i_long)                       :: data_type_index
            character(len=100)                    :: data_name
            
            character(len=1000)                   :: nchan_empty_msg
            
            integer(i_llong)               :: curdatindex, j
            integer(i_long)                :: string_arr_maxlen
            
            character(len=:), allocatable :: string_arr(:)
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                if (present(flush_data_only)) then
                    write(action_str, "(A, L, A)") "nc_diag_chaninfo_write_data(flush_data_only = ", flush_data_only, ")"
                else
                    write(action_str, "(A)") "nc_diag_chaninfo_write_data(flush_data_only = (not specified))"
                end if
                call nclayer_actionm(trim(action_str))
            end if
#endif
            
            if (init_done .AND. allocated(diag_chaninfo_store)) then
                if (diag_chaninfo_store%total > 0) then
                    if (diag_chaninfo_store%nchans /= -1) then
                        if (.NOT. diag_chaninfo_store%data_lock) then
                            do curdatindex = 1, diag_chaninfo_store%total
                                data_name = diag_chaninfo_store%names(curdatindex)
                                data_type = diag_chaninfo_store%types(curdatindex)
                                data_type_index = 1 + &
                                    ((diag_chaninfo_store%var_rel_pos(curdatindex) - 1) * diag_chaninfo_store%nchans)
                                
                                call nclayer_info("chaninfo: writing " // trim(data_name))
                                
                                ! Warn about low data filling
                                if ((.NOT. (present(flush_data_only) .AND. flush_data_only)) .AND. &
                                    ((diag_chaninfo_store%var_usage(curdatindex) + &
                                        diag_chaninfo_store%rel_indexes(curdatindex)) < diag_chaninfo_store%nchans)) then
                                    ! NOTE - I0 and TRIM are Fortran 95 specs
                                    write (nchan_empty_msg, "(A, A, A, I0, A, I0, A)") "Amount of data written in ", &
                                        trim(data_name), " (", &
                                        diag_chaninfo_store%var_usage(curdatindex) + &
                                            diag_chaninfo_store%rel_indexes(curdatindex), &
                                        ")"  // char(10) // &
                                        "             is less than nchans (", diag_chaninfo_store%nchans, ")!"
                                    
                                    if (diag_chaninfo_store%strict_check) then
                                        call nclayer_error(trim(nchan_empty_msg))
                                    else
                                        call nclayer_warning(trim(nchan_empty_msg))
                                    end if
                                    
                                    !call nclayer_warning("Amount of data written in XXXX (N) is less than nchans (N)!")
                                end if
                                
#ifdef _DEBUG_MEM_
                                print *, "****** Processing ******"
                                print *, "data_name:"
                                print *, data_name
                                print *, "data_type:"
                                print *, data_type
                                print *, "data_type_index:"
                                print *, data_type_index
                                print *, "diag_chaninfo_store%var_ids(curdatindex):"
                                print *, diag_chaninfo_store%var_ids(curdatindex)
                                print *, "diag_chaninfo_store%var_usage(curdatindex):"
                                print *, diag_chaninfo_store%var_usage(curdatindex)
                                print *, "Upper range (data_type_index + &"
                                print *, "  diag_chaninfo_store%var_usage(curdatindex) - 1):"
                                print *, (data_type_index + &
                                            diag_chaninfo_store%var_usage(curdatindex) - 1)
#endif
                                ! Make sure we have data to write in the first place!
                                if (diag_chaninfo_store%var_usage(curdatindex) > 0) then
                                    if (data_type == NLAYER_BYTE) then
#ifdef _DEBUG_MEM_
                                        print *, "Resulting data to be stored:"
                                        print *, diag_chaninfo_store%ci_byte(data_type_index:(data_type_index + &
                                                    diag_chaninfo_store%var_usage(curdatindex) - 1))
#endif
                                        call nclayer_check(nf90_put_var(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                            diag_chaninfo_store%ci_byte(data_type_index:(data_type_index + &
                                                diag_chaninfo_store%var_usage(curdatindex) - 1)), &
                                            start = (/ 1 + diag_chaninfo_store%rel_indexes(curdatindex) /) &
                                            ))
                                    else if (data_type == NLAYER_SHORT) then
                                        call nclayer_check(nf90_put_var(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                            diag_chaninfo_store%ci_short(data_type_index:(data_type_index + &
                                                diag_chaninfo_store%var_usage(curdatindex) - 1)), &
                                            start = (/ 1 + diag_chaninfo_store%rel_indexes(curdatindex) /) &
                                            ))
                                    else if (data_type == NLAYER_LONG) then
#ifdef _DEBUG_MEM_
                                        print *, "Resulting data to be stored:"
                                        print *, diag_chaninfo_store%ci_long(data_type_index:(data_type_index + &
                                                    diag_chaninfo_store%var_usage(curdatindex) - 1))
                                        print *, "start index:"
                                        print *, 1 + diag_chaninfo_store%rel_indexes(curdatindex)
#endif
                                        call nclayer_check(nf90_put_var(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                            diag_chaninfo_store%ci_long(data_type_index:(data_type_index + &
                                                diag_chaninfo_store%var_usage(curdatindex) - 1)), &
                                            start = (/ 1 + diag_chaninfo_store%rel_indexes(curdatindex) /) &
                                            ))
                                    else if (data_type == NLAYER_FLOAT) then
                                        call nclayer_check(nf90_put_var(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                            diag_chaninfo_store%ci_rsingle(data_type_index:(data_type_index + &
                                                diag_chaninfo_store%var_usage(curdatindex) - 1)), &
                                            start = (/ 1 + diag_chaninfo_store%rel_indexes(curdatindex) /) &
                                            ))
                                    else if (data_type == NLAYER_DOUBLE) then
                                        call nclayer_check(nf90_put_var(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                            diag_chaninfo_store%ci_rdouble(data_type_index:(data_type_index + &
                                                diag_chaninfo_store%var_usage(curdatindex) - 1)), &
                                            start = (/ 1 + diag_chaninfo_store%rel_indexes(curdatindex) /) &
                                            ))
                                    else if (data_type == NLAYER_STRING) then
                                        ! Storing to another variable may seem silly, but it's necessary
                                        ! to avoid "undefined variable" errors, thanks to the compiler's
                                        ! super optimization insanity...
                                        string_arr_maxlen = diag_chaninfo_store%max_str_lens(curdatindex)
                                        allocate(character(string_arr_maxlen) :: &
                                                string_arr(diag_chaninfo_store%var_usage(curdatindex)))
                                        if (enable_trim) then
                                            do j = data_type_index, data_type_index + &
                                                    diag_chaninfo_store%var_usage(curdatindex) - 1
                                                string_arr(j - data_type_index + 1) = &
                                                    trim(diag_chaninfo_store%ci_string(j))
                                            end do
                                            
                                            !string_arr = diag_chaninfo_store%ci_string(data_type_index:(data_type_index + &
                                            !        diag_chaninfo_store%var_usage(curdatindex) - 1))
                                            
#ifdef _DEBUG_MEM_
                                            do j = 1, diag_chaninfo_store%var_usage(curdatindex)
                                                write (*, "(A, A, A)") "String: '", string_arr(j), "'"
                                            end do
                                            
                                            write (*, "(A, I0)") "string_arr_maxlen = ", string_arr_maxlen
                                            write (*, "(A, I0)") "diag_chaninfo_store%var_usage(curdatindex) = ", diag_chaninfo_store%var_usage(curdatindex)
#endif
                                        else
                                            do j = data_type_index, data_type_index + &
                                                    diag_chaninfo_store%var_usage(curdatindex) - 1
                                                string_arr(j - data_type_index + 1) = &
                                                    diag_chaninfo_store%ci_string(j)
                                            end do
                                        end if
                                        
                                        call nclayer_check(nf90_put_var(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                            string_arr, &
                                            start = (/ 1, 1 + diag_chaninfo_store%rel_indexes(curdatindex) /), &
                                            count = (/ string_arr_maxlen, &
                                                diag_chaninfo_store%var_usage(curdatindex) /) ))
                                        
                                        deallocate(string_arr)
                                    else
                                        call nclayer_error("Critical error - unknown variable type!")
                                    end if
                                    
                                    ! Check for data flushing, and if so, update the relative indexes
                                    ! and set var_usage to 0.
                                    if (present(flush_data_only) .AND. flush_data_only) then
                                        diag_chaninfo_store%rel_indexes(curdatindex) = &
                                            diag_chaninfo_store%rel_indexes(curdatindex) + &
                                            diag_chaninfo_store%var_usage(curdatindex)
                                        diag_chaninfo_store%var_usage(curdatindex) = 0
                                        
#ifdef _DEBUG_MEM_
                                        print *, "diag_chaninfo_store%rel_indexes(curdatindex) is now:"
                                        print *, diag_chaninfo_store%rel_indexes(curdatindex)
#endif
                                    end if
                                end if
                            end do
                            
                            if (present(flush_data_only) .AND. flush_data_only) then
#ifdef _DEBUG_MEM_
                                print *, "In buffer flush mode!"
#endif
                            else
                                ! Lock data writing
                                diag_chaninfo_store%data_lock = .TRUE.
#ifdef _DEBUG_MEM_
                                print *, "In data lock mode!"
#endif
                            end if
                        else
                            call nclayer_error("Can't write data - data have already been written and locked!")
                        end if
                    else
                        call nclayer_error("Can't write data - number of chans not set yet!")
                    end if
                end if
            else
                call nclayer_error("Can't write data - NetCDF4 layer not initialized yet!")
            end if
            
        end subroutine nc_diag_chaninfo_write_data
        
        ! Set strict checking
        subroutine nc_diag_chaninfo_set_strict(enable_strict)
            logical, intent(in) :: enable_strict
            
            if (init_done .AND. allocated(diag_chaninfo_store)) then
                diag_chaninfo_store%strict_check = enable_strict
            else
                call nclayer_error("Can't set strictness level for chaninfo - NetCDF4 layer not initialized yet!")
            end if
        end subroutine nc_diag_chaninfo_set_strict
        
        ! Preallocate variable name/type/etc. storage.
        subroutine nc_diag_chaninfo_prealloc_vars(num_of_addl_vars)
            integer(i_llong), intent(in)           :: num_of_addl_vars
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, I0, A)") "nc_diag_chaninfo_prealloc_vars(num_of_addl_vars = ", num_of_addl_vars, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            if (init_done .AND. allocated(diag_chaninfo_store)) then
                if (allocated(diag_chaninfo_store%names)) then
                    if (diag_chaninfo_store%total >= size(diag_chaninfo_store%names)) then
                        call nc_diag_realloc(diag_chaninfo_store%names, num_of_addl_vars)
                    end if
                else
                    allocate(diag_chaninfo_store%names(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                end if
                
                if (allocated(diag_chaninfo_store%types)) then
                    if (diag_chaninfo_store%total >= size(diag_chaninfo_store%types)) then
                        call nc_diag_realloc(diag_chaninfo_store%types, num_of_addl_vars)
                    end if
                else
                    allocate(diag_chaninfo_store%types(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                end if
                
                if (allocated(diag_chaninfo_store%var_rel_pos)) then
                    if (diag_chaninfo_store%total >= size(diag_chaninfo_store%var_rel_pos)) then
                        call nc_diag_realloc(diag_chaninfo_store%var_rel_pos, num_of_addl_vars)
                    end if
                else
                    allocate(diag_chaninfo_store%var_rel_pos(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_chaninfo_store%var_rel_pos = -1
                end if
                
                if (allocated(diag_chaninfo_store%var_usage)) then
                    if (diag_chaninfo_store%total >= size(diag_chaninfo_store%var_usage)) then
                        call nc_diag_realloc(diag_chaninfo_store%var_usage, num_of_addl_vars)
                    end if
                else
                    allocate(diag_chaninfo_store%var_usage(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_chaninfo_store%var_usage = 0
                end if
                
                if (allocated(diag_chaninfo_store%var_ids)) then
                    if (diag_chaninfo_store%total >= size(diag_chaninfo_store%var_ids)) then
                        call nc_diag_realloc(diag_chaninfo_store%var_ids, num_of_addl_vars)
                    end if
                else
                    allocate(diag_chaninfo_store%var_ids(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_chaninfo_store%var_ids = -1
                end if
                
                if (allocated(diag_chaninfo_store%max_str_lens)) then
                    if (diag_chaninfo_store%total >= size(diag_chaninfo_store%max_str_lens)) then
                        call nc_diag_realloc(diag_chaninfo_store%max_str_lens, num_of_addl_vars)
                    end if
                else
                    allocate(diag_chaninfo_store%max_str_lens(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_chaninfo_store%max_str_lens = -1
                end if
                
                if (allocated(diag_chaninfo_store%rel_indexes)) then
                    if (diag_chaninfo_store%total >= size(diag_chaninfo_store%rel_indexes)) then
                        call nc_diag_realloc(diag_chaninfo_store%rel_indexes, num_of_addl_vars)
                    end if
                else
                    allocate(diag_chaninfo_store%rel_indexes(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_chaninfo_store%rel_indexes = 0
                end if
            else
                call nclayer_error("NetCDF4 layer not initialized yet!")
            endif
        end subroutine nc_diag_chaninfo_prealloc_vars
        
        ! Preallocate actual variable data storage
        subroutine nc_diag_chaninfo_prealloc_vars_storage(nclayer_type, num_of_addl_slots)
            integer(i_byte), intent(in)           :: nclayer_type
            integer(i_llong), intent(in)          :: num_of_addl_slots
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, I0, A, I0, A)") "nc_diag_chaninfo_prealloc_vars_storage(nclayer_type = ", nclayer_type, ", num_of_addl_slots = ", num_of_addl_slots, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            
            if (nclayer_type == NLAYER_BYTE) then
                call nc_diag_chaninfo_resize_byte(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_SHORT) then
                call nc_diag_chaninfo_resize_short(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_LONG) then
                call nc_diag_chaninfo_resize_long(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_FLOAT) then
                call nc_diag_chaninfo_resize_rsingle(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_DOUBLE) then
                call nc_diag_chaninfo_resize_rdouble(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_STRING) then
                call nc_diag_chaninfo_resize_string(num_of_addl_slots, .FALSE.)
            else
                call nclayer_error("Invalid type specified for variable storage preallocation!")
            end if
            
            ! resize nc_diag_chaninfo_resize_iarr ?
            
            
        end subroutine nc_diag_chaninfo_prealloc_vars_storage
        
        subroutine nc_diag_chaninfo_expand
            integer(i_llong) :: addl_fields
            ! Did we realloc at all?
            logical :: meta_realloc
            meta_realloc = .FALSE.
            
            if (init_done .AND. allocated(diag_chaninfo_store)) then
                addl_fields = 1 + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_chaninfo_store%alloc_multi))
                if (diag_chaninfo_store%nchans /= -1) then
                    if (allocated(diag_chaninfo_store%names)) then
                        if (diag_chaninfo_store%total >= size(diag_chaninfo_store%names)) then
                            call nc_diag_realloc(diag_chaninfo_store%names, addl_fields)
                            meta_realloc = .TRUE.
                        end if
                    else
                        allocate(diag_chaninfo_store%names(NLAYER_DEFAULT_ENT))
                    end if
                    
                    if (allocated(diag_chaninfo_store%types)) then
                        if (diag_chaninfo_store%total >= size(diag_chaninfo_store%types)) then
                            call nc_diag_realloc(diag_chaninfo_store%types, addl_fields)
                            meta_realloc = .TRUE.
                        end if
                    else
                        allocate(diag_chaninfo_store%types(NLAYER_DEFAULT_ENT))
                    end if
                    
                    if (allocated(diag_chaninfo_store%var_rel_pos)) then
                        if (diag_chaninfo_store%total >= size(diag_chaninfo_store%var_rel_pos)) then
                            call nc_diag_realloc(diag_chaninfo_store%var_rel_pos, addl_fields)
                            meta_realloc = .TRUE.
                        end if
                    else
                        allocate(diag_chaninfo_store%var_rel_pos(NLAYER_DEFAULT_ENT))
                        diag_chaninfo_store%var_rel_pos = -1
                    end if
                    
                    if (allocated(diag_chaninfo_store%var_usage)) then
                        if (diag_chaninfo_store%total >= size(diag_chaninfo_store%var_usage)) then
                            call nc_diag_realloc(diag_chaninfo_store%var_usage, addl_fields)
                            meta_realloc = .TRUE.
                        end if
                    else
                        allocate(diag_chaninfo_store%var_usage(NLAYER_DEFAULT_ENT))
                        diag_chaninfo_store%var_usage = 0
                    end if
                    
                    if (allocated(diag_chaninfo_store%var_ids)) then
                        if (diag_chaninfo_store%total >= size(diag_chaninfo_store%var_ids)) then
                            call nc_diag_realloc(diag_chaninfo_store%var_ids, addl_fields)
                            meta_realloc = .TRUE.
                        end if
                    else
                        allocate(diag_chaninfo_store%var_ids(NLAYER_DEFAULT_ENT))
                        diag_chaninfo_store%var_ids = -1
                    end if
                    
                    if (allocated(diag_chaninfo_store%max_str_lens)) then
                        if (diag_chaninfo_store%total >= size(diag_chaninfo_store%max_str_lens)) then
                            call nc_diag_realloc(diag_chaninfo_store%max_str_lens, addl_fields)
                            meta_realloc = .TRUE.
                        end if
                    else
                        allocate(diag_chaninfo_store%max_str_lens(NLAYER_DEFAULT_ENT))
                        diag_chaninfo_store%max_str_lens = -1
                    end if
                    
                    if (allocated(diag_chaninfo_store%rel_indexes)) then
                        if (diag_chaninfo_store%total >= size(diag_chaninfo_store%rel_indexes)) then
                            call nc_diag_realloc(diag_chaninfo_store%rel_indexes, addl_fields)
                            meta_realloc = .TRUE.
                        end if
                    else
                        allocate(diag_chaninfo_store%rel_indexes(NLAYER_DEFAULT_ENT))
                        diag_chaninfo_store%rel_indexes = 0
                    end if
                    
                    if (meta_realloc) then
                        diag_chaninfo_store%alloc_multi = diag_chaninfo_store%alloc_multi + 1
                    end if
                else
                    call nclayer_error("Number of chans not set yet!")
                end if
            else
                call nclayer_error("NetCDF4 layer not initialized yet!")
            end if
        end subroutine nc_diag_chaninfo_expand
        
        ! nc_diag_chaninfo - input integer(i_byte)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_chaninfo_byte(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)    :: chaninfo_name
            integer(i_byte), intent(in)     :: chaninfo_value
            
            integer(i_long) :: i, var_index, var_rel_index, type_index
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, I0, A)") "nc_diag_chaninfo_byte(chaninfo_name = " // chaninfo_name // ", chaninfo_value = ", chaninfo_value, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            
            if (diag_chaninfo_store%data_lock) then
                call nclayer_error("Can't add new data - data have already been written and locked!")
            end if
            
            ! For byte, type index is 1
            type_index = 1
            
            ! Default to -1
            var_index = -1
            
            ! [ 'asdf', 'ghjk', 'zxcv' ]
            ! [   BYTE,  FLOAT,   BYTE ]
            ! [      1,      1,      2 ]
            
            ! find the index first!
            do i = 1, diag_chaninfo_store%total
                if (diag_chaninfo_store%names(i) == chaninfo_name) then
                    var_rel_index = diag_chaninfo_store%var_rel_pos(i)
                    var_index = i
                    exit
                end if
            end do
            
            if (var_index == -1) then
                ! Entry does not exist yet...
                
                ! First, check to make sure we can still define new variables.
                if (diag_chaninfo_store%def_lock) then
                    call nclayer_error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                ! Expand things first!
                call nc_diag_chaninfo_expand
                
                ! Add to the total!
                diag_chaninfo_store%total = diag_chaninfo_store%total + 1
                
                ! Store name and type!
                diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
                diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_BYTE
                
                ! We just need to add one entry...
                call nc_diag_chaninfo_resize_byte(int8(diag_chaninfo_store%nchans))
                
                ! Now add a relative position... based on the next position!
                
                ! First, increment the number of variables stored for this type:
                diag_chaninfo_store%acount_v(type_index) = diag_chaninfo_store%acount_v(type_index) + 1
                
                ! Then, set the next variable's relative positioning,
                ! based on the number of variables stored for this type.
                diag_chaninfo_store%var_rel_pos(diag_chaninfo_store%total) = diag_chaninfo_store%acount_v(type_index)
                
                ! Initialize the amount of memory used to 1.
                diag_chaninfo_store%var_usage(diag_chaninfo_store%total) = 1
                
                ! Set var_index to the total
                var_index = diag_chaninfo_store%total
            else
                ! entry already exists!
                if (diag_chaninfo_store%var_usage(var_index) + &
                    diag_chaninfo_store%rel_indexes(var_index) >= diag_chaninfo_store%nchans) then
                    call nclayer_error("Can't add new data - data added is exceeding nchan! Data must fit within nchan constraint.")
                endif
                
                diag_chaninfo_store%var_usage(var_index) = &
                    diag_chaninfo_store%var_usage(var_index) + 1
            end if
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_byte(1 + &
                    ((diag_chaninfo_store%var_rel_pos(var_index) - 1) &
                        * diag_chaninfo_store%nchans) &
                    + (diag_chaninfo_store%var_usage(var_index) - 1)) = chaninfo_value
        end subroutine nc_diag_chaninfo_byte
        
        ! nc_diag_chaninfo - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_chaninfo_short(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)    :: chaninfo_name
            integer(i_short), intent(in)    :: chaninfo_value
            
            integer(i_long) :: i, var_index, var_rel_index, type_index
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, I0, A)") "nc_diag_chaninfo_short(chaninfo_name = " // chaninfo_name // ", chaninfo_value = ", chaninfo_value, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            
            if (diag_chaninfo_store%data_lock) then
                call nclayer_error("Can't add new data - data have already been written and locked!")
            end if
            
            ! For short, type index is 2
            type_index = 2
            
            ! Default to -1
            var_index = -1
            
            ! [ 'asdf', 'ghjk', 'zxcv' ]
            ! [   BYTE,  FLOAT,   BYTE ]
            ! [      1,      1,      2 ]
            
            ! find the index first!
            do i = 1, diag_chaninfo_store%total
                if (diag_chaninfo_store%names(i) == chaninfo_name) then
                    var_rel_index = diag_chaninfo_store%var_rel_pos(i)
                    var_index = i
                    exit
                end if
            end do
            
            if (var_index == -1) then
                ! Entry does not exist yet...
                
                ! First, check to make sure we can still define new variables.
                if (diag_chaninfo_store%def_lock) then
                    call nclayer_error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                ! Expand things first!
                call nc_diag_chaninfo_expand
                
                ! Add to the total!
                diag_chaninfo_store%total = diag_chaninfo_store%total + 1
                
                ! Store name and type!
                diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
                diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_SHORT
                
                ! We just need to add one entry...
                call nc_diag_chaninfo_resize_short(int8(diag_chaninfo_store%nchans))
                
                ! Now add a relative position... based on the next position!
                
                ! First, increment the number of variables stored for this type:
                diag_chaninfo_store%acount_v(type_index) = diag_chaninfo_store%acount_v(type_index) + 1
                
                ! Then, set the next variable's relative positioning,
                ! based on the number of variables stored for this type.
                diag_chaninfo_store%var_rel_pos(diag_chaninfo_store%total) = diag_chaninfo_store%acount_v(type_index)
                
                ! Initialize the amount of memory used to 1.
                diag_chaninfo_store%var_usage(diag_chaninfo_store%total) = 1
                
                ! Set var_index to the total
                var_index = diag_chaninfo_store%total
            else
                ! entry already exists!
                if (diag_chaninfo_store%var_usage(var_index) + &
                    diag_chaninfo_store%rel_indexes(var_index) >= diag_chaninfo_store%nchans) then
                    call nclayer_error("Can't add new data - data added is exceeding nchan! Data must fit within nchan constraint.")
                endif
                
                diag_chaninfo_store%var_usage(var_index) = &
                    diag_chaninfo_store%var_usage(var_index) + 1
            end if
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_short(1 + &
                    ((diag_chaninfo_store%var_rel_pos(var_index) - 1) &
                        * diag_chaninfo_store%nchans) &
                    + (diag_chaninfo_store%var_usage(var_index) - 1)) = chaninfo_value
        end subroutine nc_diag_chaninfo_short
        
        ! nc_diag_chaninfo - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_chaninfo_long(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)    :: chaninfo_name
            integer(i_long), intent(in)     :: chaninfo_value
            
            integer(i_long) :: i, var_index, var_rel_index, type_index
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, I0, A)") "nc_diag_chaninfo_long(chaninfo_name = " // chaninfo_name // ", chaninfo_value = ", chaninfo_value, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            
            if (diag_chaninfo_store%data_lock) then
                call nclayer_error("Can't add new data - data have already been written and locked!")
            end if
            
            ! For long, type index is 3
            type_index = 3
            
            ! Default to -1
            var_index = -1
            
            ! [ 'asdf', 'ghjk', 'zxcv' ]
            ! [   BYTE,  FLOAT,   BYTE ]
            ! [      1,      1,      2 ]
            
            ! find the index first!
            do i = 1, diag_chaninfo_store%total
                if (diag_chaninfo_store%names(i) == chaninfo_name) then
                    var_rel_index = diag_chaninfo_store%var_rel_pos(i)
                    var_index = i
                    exit
                end if
            end do
            
#ifdef _DEBUG_MEM_
            print *, " *** chaninfo_name"
            print *, chaninfo_name
            print *, " *** var_index is set to:"
            print *, var_index
#endif
            
            if (var_index == -1) then
                ! Entry does not exist yet...
                
                ! First, check to make sure we can still define new variables.
                if (diag_chaninfo_store%def_lock) then
                    call nclayer_error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                ! Expand things first!
                call nc_diag_chaninfo_expand
                
                ! Add to the total!
                diag_chaninfo_store%total = diag_chaninfo_store%total + 1
                
                ! Store name and type!
                diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
                diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_LONG
                
                ! We just need to add one entry...
                call nc_diag_chaninfo_resize_long(int8(diag_chaninfo_store%nchans))
                
                ! Now add a relative position... based on the next position!
                
                ! First, increment the number of variables stored for this type:
                diag_chaninfo_store%acount_v(type_index) = diag_chaninfo_store%acount_v(type_index) + 1
                
                ! Then, set the next variable's relative positioning,
                ! based on the number of variables stored for this type.
                diag_chaninfo_store%var_rel_pos(diag_chaninfo_store%total) = diag_chaninfo_store%acount_v(type_index)
                
                ! Initialize the amount of memory used to 1.
                diag_chaninfo_store%var_usage(diag_chaninfo_store%total) = 1
                
                ! Set var_index to the total
                var_index = diag_chaninfo_store%total
            else
                ! entry already exists!
                if (diag_chaninfo_store%var_usage(var_index) + &
                    diag_chaninfo_store%rel_indexes(var_index) >= diag_chaninfo_store%nchans) then
#ifdef _DEBUG_MEM_
                    print *, "!!!! diag_chaninfo_store%var_usage(var_index)"
                    print *, diag_chaninfo_store%var_usage(var_index)
#endif
                    call nclayer_error("Can't add new data - data added is exceeding nchan! Data must fit within nchan constraint.")
                endif
                
                diag_chaninfo_store%var_usage(var_index) = &
                    diag_chaninfo_store%var_usage(var_index) + 1
            end if
            
            ! Now add the actual entry!
#ifdef _DEBUG_MEM_
            print *, "===================================="
            print *, "diag_chaninfo_store%total"
            print *, diag_chaninfo_store%total
            print *, "var_index"
            print *, var_index
            print *, "diag_chaninfo_store%var_rel_pos(var_index)"
            print *, diag_chaninfo_store%var_rel_pos(var_index)
            print *, "diag_chaninfo_store%nchans"
            print *, diag_chaninfo_store%nchans
            print *, "diag_chaninfo_store%var_usage(var_index)"
            print *, diag_chaninfo_store%var_usage(var_index)
            print *, "===================================="
#endif
            
            diag_chaninfo_store%ci_long(1 + &
                    ((diag_chaninfo_store%var_rel_pos(var_index) - 1) &
                        * diag_chaninfo_store%nchans) &
                    + (diag_chaninfo_store%var_usage(var_index) - 1)) = chaninfo_value
        end subroutine nc_diag_chaninfo_long
        
        ! nc_diag_chaninfo - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_chaninfo_rsingle(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)    :: chaninfo_name
            real(r_single), intent(in)      :: chaninfo_value
            
            integer(i_long) :: i, var_index, var_rel_index, type_index
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, F0.5, A)") "nc_diag_chaninfo_rsingle(chaninfo_name = " // chaninfo_name // ", chaninfo_value = ", chaninfo_value, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            
            if (diag_chaninfo_store%data_lock) then
                call nclayer_error("Can't add new data - data have already been written and locked!")
            end if
            
            ! For rsingle, type index is 4
            type_index = 4
            
            ! Default to -1
            var_index = -1
            
            ! [ 'asdf', 'ghjk', 'zxcv' ]
            ! [   BYTE,  FLOAT,   BYTE ]
            ! [      1,      1,      2 ]
            
            ! find the index first!
            do i = 1, diag_chaninfo_store%total
                if (diag_chaninfo_store%names(i) == chaninfo_name) then
                    var_rel_index = diag_chaninfo_store%var_rel_pos(i)
                    var_index = i
                    exit
                end if
            end do
            
#ifdef _DEBUG_MEM_
            print *, " *** chaninfo_name"
            print *, chaninfo_name
            print *, " *** var_index is set to:"
            print *, var_index
#endif
            
            if (var_index == -1) then
                ! Entry does not exist yet...
                
                ! First, check to make sure we can still define new variables.
                if (diag_chaninfo_store%def_lock) then
                    call nclayer_error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                ! Expand things first!
                call nc_diag_chaninfo_expand
                
                ! Add to the total!
                diag_chaninfo_store%total = diag_chaninfo_store%total + 1
                
                ! Store name and type!
                diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
                diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_FLOAT
                
                ! We just need to add one entry...
                call nc_diag_chaninfo_resize_rsingle(int8(diag_chaninfo_store%nchans))
                
                ! Now add a relative position... based on the next position!
                
                ! First, increment the number of variables stored for this type:
                diag_chaninfo_store%acount_v(type_index) = diag_chaninfo_store%acount_v(type_index) + 1
                
                ! Then, set the next variable's relative positioning,
                ! based on the number of variables stored for this type.
                diag_chaninfo_store%var_rel_pos(diag_chaninfo_store%total) = diag_chaninfo_store%acount_v(type_index)
                
                ! Initialize the amount of memory used to 1.
                diag_chaninfo_store%var_usage(diag_chaninfo_store%total) = 1
                
                ! Set var_index to the total
                var_index = diag_chaninfo_store%total
            else
                ! entry already exists!
                if (diag_chaninfo_store%var_usage(var_index) + &
                    diag_chaninfo_store%rel_indexes(var_index) >= diag_chaninfo_store%nchans) then
                    call nclayer_error("Can't add new data - data added is exceeding nchan! Data must fit within nchan constraint.")
                endif
                
                diag_chaninfo_store%var_usage(var_index) = &
                    diag_chaninfo_store%var_usage(var_index) + 1
            end if
            
#ifdef _DEBUG_MEM_
            print *, "===================================="
            print *, "diag_chaninfo_store%total"
            print *, diag_chaninfo_store%total
            print *, "var_index"
            print *, var_index
            print *, "diag_chaninfo_store%var_rel_pos(var_index)"
            print *, diag_chaninfo_store%var_rel_pos(var_index)
            print *, "diag_chaninfo_store%nchans"
            print *, diag_chaninfo_store%nchans
            print *, "diag_chaninfo_store%var_usage(var_index)"
            print *, diag_chaninfo_store%var_usage(var_index)
            print *, "===================================="
#endif
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_rsingle(1 + &
                    ((diag_chaninfo_store%var_rel_pos(var_index) - 1) &
                        * diag_chaninfo_store%nchans) &
                    + (diag_chaninfo_store%var_usage(var_index) - 1)) = chaninfo_value
        end subroutine nc_diag_chaninfo_rsingle
        
        ! nc_diag_chaninfo - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_chaninfo_rdouble(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)    :: chaninfo_name
            real(r_double), intent(in)      :: chaninfo_value
            
            integer(i_long) :: i, var_index, var_rel_index, type_index
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, F0.5, A)") "nc_diag_chaninfo_rdouble(chaninfo_name = " // chaninfo_name // ", chaninfo_value = ", chaninfo_value, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            
            if (diag_chaninfo_store%data_lock) then
                call nclayer_error("Can't add new data - data have already been written and locked!")
            end if
            
            ! For rdouble, type index is 5
            type_index = 5
            
            ! Default to -1
            var_index = -1
            
            ! [ 'asdf', 'ghjk', 'zxcv' ]
            ! [   BYTE,  FLOAT,   BYTE ]
            ! [      1,      1,      2 ]
            
            ! find the index first!
            do i = 1, diag_chaninfo_store%total
                if (diag_chaninfo_store%names(i) == chaninfo_name) then
                    var_rel_index = diag_chaninfo_store%var_rel_pos(i)
                    var_index = i
                    exit
                end if
            end do
            
            if (var_index == -1) then
                ! Entry does not exist yet...
                
                ! First, check to make sure we can still define new variables.
                if (diag_chaninfo_store%def_lock) then
                    call nclayer_error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                ! Expand things first!
                call nc_diag_chaninfo_expand
                
                ! Add to the total!
                diag_chaninfo_store%total = diag_chaninfo_store%total + 1
                
                ! Store name and type!
                diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
                diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_DOUBLE
                
                ! We just need to add one entry...
                call nc_diag_chaninfo_resize_rdouble(int8(diag_chaninfo_store%nchans))
                
                ! Now add a relative position... based on the next position!
                
                ! First, increment the number of variables stored for this type:
                diag_chaninfo_store%acount_v(type_index) = diag_chaninfo_store%acount_v(type_index) + 1
                
                ! Then, set the next variable's relative positioning,
                ! based on the number of variables stored for this type.
                diag_chaninfo_store%var_rel_pos(diag_chaninfo_store%total) = diag_chaninfo_store%acount_v(type_index)
                
                ! Initialize the amount of memory used to 1.
                diag_chaninfo_store%var_usage(diag_chaninfo_store%total) = 1
                
                ! Set var_index to the total
                var_index = diag_chaninfo_store%total
            else
                ! entry already exists!
                if (diag_chaninfo_store%var_usage(var_index) + &
                    diag_chaninfo_store%rel_indexes(var_index) >= diag_chaninfo_store%nchans) then
                    call nclayer_error("Can't add new data - data added is exceeding nchan! Data must fit within nchan constraint.")
                endif
                
                diag_chaninfo_store%var_usage(var_index) = &
                    diag_chaninfo_store%var_usage(var_index) + 1
            end if
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_rdouble(1 + &
                    ((diag_chaninfo_store%var_rel_pos(var_index) - 1) &
                        * diag_chaninfo_store%nchans) &
                    + (diag_chaninfo_store%var_usage(var_index) - 1)) = chaninfo_value
        end subroutine nc_diag_chaninfo_rdouble

        ! nc_diag_chaninfo - input character(len=*)
        ! Corresponding NetCDF4 type: string? char?
        subroutine nc_diag_chaninfo_string(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)    :: chaninfo_name
            character(len=*), intent(in)    :: chaninfo_value
            
            integer(i_long) :: i, var_index, var_rel_index, type_index
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A)") "nc_diag_chaninfo_string(chaninfo_name = " // chaninfo_name // ", chaninfo_value = " // trim(chaninfo_value) // ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            
            if (diag_chaninfo_store%data_lock) then
                call nclayer_error("Can't add new data - data have already been written and locked!")
            end if
            
            ! For string, type index is 6
            type_index = 6
            
            ! Default to -1
            var_index = -1
            
            ! [ 'asdf', 'ghjk', 'zxcv' ]
            ! [   BYTE,  FLOAT,   BYTE ]
            ! [      1,      1,      2 ]
            
            ! find the index first!
            do i = 1, diag_chaninfo_store%total
                if (diag_chaninfo_store%names(i) == chaninfo_name) then
                    var_rel_index = diag_chaninfo_store%var_rel_pos(i)
                    var_index = i
                    exit
                end if
            end do
            
            if (var_index == -1) then
                ! Entry does not exist yet...
                
                ! First, check to make sure we can still define new variables.
                if (diag_chaninfo_store%def_lock) then
                    call nclayer_error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                ! Expand things first!
                call nc_diag_chaninfo_expand
                
                ! Add to the total!
                diag_chaninfo_store%total = diag_chaninfo_store%total + 1
                
                ! Store name and type!
                diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
                diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_STRING
                
                ! We just need to add one entry...
                call nc_diag_chaninfo_resize_string(int8(diag_chaninfo_store%nchans))
                
                ! Now add a relative position... based on the next position!
                
                ! First, increment the number of variables stored for this type:
                diag_chaninfo_store%acount_v(type_index) = diag_chaninfo_store%acount_v(type_index) + 1
                
                ! Then, set the next variable's relative positioning,
                ! based on the number of variables stored for this type.
                diag_chaninfo_store%var_rel_pos(diag_chaninfo_store%total) = diag_chaninfo_store%acount_v(type_index)
                
                ! Initialize the amount of memory used to 1.
                diag_chaninfo_store%var_usage(diag_chaninfo_store%total) = 1
                
                ! Set var_index to the total
                var_index = diag_chaninfo_store%total
            else
                ! entry already exists!
                if (diag_chaninfo_store%var_usage(var_index) + &
                    diag_chaninfo_store%rel_indexes(var_index) >= diag_chaninfo_store%nchans) then
                    call nclayer_error("Can't add new data - data added is exceeding nchan! Data must fit within nchan constraint.")
                endif
                
                ! Check max string length
                if ((diag_chaninfo_store%def_lock) .AND. &
                    (len_trim(chaninfo_value) > diag_chaninfo_store%max_str_lens(var_index))) &
                    call nclayer_error("Cannot expand variable string length after locking variable definitions!")
                
                diag_chaninfo_store%var_usage(var_index) = &
                    diag_chaninfo_store%var_usage(var_index) + 1
            end if
            
            ! If trim isn't enabled, set our maximum string length here!
            if (.NOT. enable_trim) then
                if (diag_chaninfo_store%max_str_lens(var_index) == -1) then
                    diag_chaninfo_store%max_str_lens(var_index) = len(chaninfo_value)
                else
                    ! Validate that our non-first value isn't different from
                    ! the initial string length
                    if (diag_chaninfo_store%max_str_lens(var_index) /= len(chaninfo_value)) &
                        call nclayer_error("Cannot change string size when trimming is disabled!")
                end if
            end if
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_string(1 + &
                    ((diag_chaninfo_store%var_rel_pos(var_index) - 1) &
                        * diag_chaninfo_store%nchans) &
                    + (diag_chaninfo_store%var_usage(var_index) - 1)) = chaninfo_value
        end subroutine nc_diag_chaninfo_string
end module nclayer_chaninfo
