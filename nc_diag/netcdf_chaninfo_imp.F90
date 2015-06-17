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
            if (init_done .AND. allocated(diag_chaninfo_store)) then
                if (nchans < 1) then
                    call error("Critical error - specified a nchan < 1!")
                end if
                
                diag_chaninfo_store%nchans = nchans
            else
                call error("NetCDF4 layer not initialized yet!")
            end if
        end subroutine nc_diag_chaninfo_dim_set
        
        !subroutine nc_diag_chaninfo(chaninfo_name, chaninfo_data)
        !    
        !end subroutine nc_diag_chaninfo
        
        subroutine nc_diag_chaninfo_allocmulti(multiplier)
            integer(i_long), intent(in)    :: multiplier
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
        
        subroutine nc_diag_chaninfo_write_def
            ! Just write the definitions out!
            integer(i_long)               :: curdatindex
            integer(i_byte)               :: data_type
            integer(i_kind)               :: nc_data_type
            
            if (init_done .AND. allocated(diag_chaninfo_store)) then
                if (diag_chaninfo_store%nchans /= -1) then
                    if (.NOT. diag_chaninfo_store%def_lock) then
                        ! First, set the dimensions!
                        call check(nf90_def_dim(ncid, "nchans", diag_chaninfo_store%nchans, diag_chaninfo_store%nchans_dimid))
                        
                        ! Once we have the dimension, we can start writing
                        ! variable definitions!
                        do curdatindex = 1, diag_chaninfo_store%total
                            data_type = diag_chaninfo_store%types(curdatindex)
                            
                            if (data_type == NLAYER_BYTE)   nc_data_type = nf90_byte
                            if (data_type == NLAYER_SHORT)  nc_data_type = nf90_short
                            if (data_type == NLAYER_LONG)   nc_data_type = nf90_int
                            if (data_type == NLAYER_FLOAT)  nc_data_type = nf90_float
                            if (data_type == NLAYER_DOUBLE) nc_data_type = nf90_double
                            if (data_type == NLAYER_STRING) nc_data_type = nf90_string
                            
                            call check(nf90_def_var(ncid, diag_chaninfo_store%names(curdatindex), &
                                nc_data_type, diag_chaninfo_store%nchans_dimid, &
                                diag_chaninfo_store%var_ids(curdatindex)))
                        end do
                        
                        ! Lock the definitions!
                        diag_chaninfo_store%def_lock = .TRUE.
                    else
                        call error("Can't write definitions - definitions have already been written and locked!")
                    end if
                else
                    call error("Can't write definitions - number of chans not set yet!")
                end if
            else
                call error("Can't write definitions - NetCDF4 layer not initialized yet!")
            end if
        end subroutine nc_diag_chaninfo_write_def
        
        subroutine nc_diag_chaninfo_write_data
            integer(i_byte)                       :: data_type
            integer(i_long)                       :: data_type_index
            character(len=100)                    :: data_name
            
            character(len=1000)                   :: warning_str
            
            integer(i_long)               :: curdatindex
            
            if (init_done .AND. allocated(diag_chaninfo_store)) then
                if (diag_chaninfo_store%nchans /= -1) then
                    if (.NOT. diag_chaninfo_store%data_lock) then
                        do curdatindex = 1, diag_chaninfo_store%total
                            data_name = diag_chaninfo_store%names(curdatindex)
                            data_type = diag_chaninfo_store%types(curdatindex)
                            data_type_index = 1 + &
                                ((diag_chaninfo_store%var_rel_pos(curdatindex) - 1) * diag_chaninfo_store%nchans)
                            
                            ! Warn about low data filling
                            if (diag_chaninfo_store%var_usage(curdatindex) < diag_chaninfo_store%nchans) then
                                ! NOTE - I0 and TRIM are Fortran 95 specs
                                write (warning_str, "(A, A, A, I0, A, I0, A)") "Amount of data written in ", &
                                    trim(data_name), "(", diag_chaninfo_store%var_usage(curdatindex), &
                                    ") is less than nchans (", diag_chaninfo_store%nchans, ")!"
                                call warning(trim(warning_str))
                                !call warning("Amount of data written in XXXX (N) is less than nchans (N)!")
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
                            
                            if (data_type == NLAYER_BYTE) then
#ifdef _DEBUG_MEM_
                                print *, "Resulting data to be stored:"
                                print *, diag_chaninfo_store%ci_byte(data_type_index:(data_type_index + &
                                            diag_chaninfo_store%var_usage(curdatindex) - 1))
#endif
                                call check(nf90_put_var(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                    diag_chaninfo_store%ci_byte(data_type_index:(data_type_index + &
                                        diag_chaninfo_store%var_usage(curdatindex) - 1))))
                            else if (data_type == NLAYER_SHORT) then
                                call check(nf90_put_var(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                    diag_chaninfo_store%ci_short(data_type_index:(data_type_index + &
                                        diag_chaninfo_store%var_usage(curdatindex) - 1))))
                            else if (data_type == NLAYER_LONG) then
#ifdef _DEBUG_MEM_
                                print *, "Resulting data to be stored:"
                                print *, diag_chaninfo_store%ci_long(data_type_index:(data_type_index + &
                                            diag_chaninfo_store%var_usage(curdatindex) - 1))
#endif
                                call check(nf90_put_var(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                    diag_chaninfo_store%ci_long(data_type_index:(data_type_index + &
                                        diag_chaninfo_store%var_usage(curdatindex) - 1))))
                            else if (data_type == NLAYER_FLOAT) then
                                call check(nf90_put_var(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                    diag_chaninfo_store%ci_rsingle(data_type_index:(data_type_index + &
                                        diag_chaninfo_store%var_usage(curdatindex) - 1))))
                            else if (data_type == NLAYER_DOUBLE) then
                                call check(nf90_put_var(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                    diag_chaninfo_store%ci_rdouble(data_type_index:(data_type_index + &
                                        diag_chaninfo_store%var_usage(curdatindex) - 1))))
                            else if (data_type == NLAYER_STRING) then
#ifndef IGNORE_VERSION
                                ! If you manage to sneak in this far...
                                if (NLAYER_STRING_BROKEN) then
                                    call error("Data string storage not supported with NetCDF v4.2.1.1 or lower.")
                                end if
#endif
                                call check(nf90_put_var(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                    diag_chaninfo_store%ci_string(data_type_index:(data_type_index + &
                                        diag_chaninfo_store%var_usage(curdatindex) - 1))))
                            else
                                call error("Critical error - unknown variable type!")
                            end if
                            
                            ! Lock data writing
                            diag_chaninfo_store%data_lock = .TRUE.
                        end do
                    else
                        call error("Can't write data - data have already been written and locked!")
                    end if
                else
                    call error("Can't write data - number of chans not set yet!")
                end if
            else
                call error("Can't write data - NetCDF4 layer not initialized yet!")
            end if
            
        end subroutine nc_diag_chaninfo_write_data
        
        subroutine nc_diag_chaninfo_expand
            ! Did we realloc at all?
            logical :: meta_realloc
            meta_realloc = .FALSE.
            
            if (init_done .AND. allocated(diag_chaninfo_store)) then
                if (diag_chaninfo_store%nchans /= -1) then
                    if (allocated(diag_chaninfo_store%names)) then
                        if (diag_chaninfo_store%total >= size(diag_chaninfo_store%names)) then
                            call nc_diag_realloc(diag_chaninfo_store%names, 1 + (NLAYER_DEFAULT_ENT * (2 ** diag_chaninfo_store%alloc_multi)))
                            meta_realloc = .TRUE.
                        end if
                    else
                        allocate(diag_chaninfo_store%names(NLAYER_DEFAULT_ENT))
                    end if
                    
                    if (allocated(diag_chaninfo_store%types)) then
                        if (diag_chaninfo_store%total >= size(diag_chaninfo_store%types)) then
                            call nc_diag_realloc(diag_chaninfo_store%types, 1 + (NLAYER_DEFAULT_ENT * (2 ** diag_chaninfo_store%alloc_multi)))
                            meta_realloc = .TRUE.
                        end if
                    else
                        allocate(diag_chaninfo_store%types(NLAYER_DEFAULT_ENT))
                    end if
                    
                    if (allocated(diag_chaninfo_store%var_rel_pos)) then
                        if (diag_chaninfo_store%total >= size(diag_chaninfo_store%var_rel_pos)) then
                            call nc_diag_realloc(diag_chaninfo_store%var_rel_pos, 1 + (NLAYER_DEFAULT_ENT * (2 ** diag_chaninfo_store%alloc_multi)))
                            meta_realloc = .TRUE.
                        end if
                    else
                        allocate(diag_chaninfo_store%var_rel_pos(NLAYER_DEFAULT_ENT))
                        diag_chaninfo_store%var_rel_pos = -1
                    end if
                    
                    if (allocated(diag_chaninfo_store%var_usage)) then
                        if (diag_chaninfo_store%total >= size(diag_chaninfo_store%var_usage)) then
                            call nc_diag_realloc(diag_chaninfo_store%var_usage, 1 + (NLAYER_DEFAULT_ENT * (2 ** diag_chaninfo_store%alloc_multi)))
                            meta_realloc = .TRUE.
                        end if
                    else
                        allocate(diag_chaninfo_store%var_usage(NLAYER_DEFAULT_ENT))
                        diag_chaninfo_store%var_usage = 0
                    end if
                    
                    if (allocated(diag_chaninfo_store%var_ids)) then
                        if (diag_chaninfo_store%total >= size(diag_chaninfo_store%var_ids)) then
                            call nc_diag_realloc(diag_chaninfo_store%var_ids, 1 + (NLAYER_DEFAULT_ENT * (2 ** diag_chaninfo_store%alloc_multi)))
                            meta_realloc = .TRUE.
                        end if
                    else
                        allocate(diag_chaninfo_store%var_ids(NLAYER_DEFAULT_ENT))
                        diag_chaninfo_store%var_ids = -1
                    end if
                    
                    if (meta_realloc) then
                        diag_chaninfo_store%alloc_multi = diag_chaninfo_store%alloc_multi + 1
                    end if
                else
                    call error("Number of chans not set yet!")
                end if
            else
                call error("NetCDF4 layer not initialized yet!")
            end if
        end subroutine nc_diag_chaninfo_expand
        
        ! nc_diag_chaninfo - input integer(i_byte)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_chaninfo_byte(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)    :: chaninfo_name
            integer(i_byte), intent(in)     :: chaninfo_value
            
            integer(i_long) :: i, var_index, var_rel_index, type_index
            
            if (diag_chaninfo_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
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
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                ! Expand things first!
                call nc_diag_chaninfo_expand
                
                ! Add to the total!
                diag_chaninfo_store%total = diag_chaninfo_store%total + 1
                
                ! Store name and type!
                diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
                diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_BYTE
                
                ! We just need to add one entry...
                call nc_diag_chaninfo_resize_byte(diag_chaninfo_store%nchans)
                
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
                if (diag_chaninfo_store%var_usage(var_index) >= diag_chaninfo_store%nchans) then
                    call error("Can't add new data - data added is exceeding nchan! Data must fit within nchan constraint.")
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
            
            if (diag_chaninfo_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
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
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                ! Expand things first!
                call nc_diag_chaninfo_expand
                
                ! Add to the total!
                diag_chaninfo_store%total = diag_chaninfo_store%total + 1
                
                ! Store name and type!
                diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
                diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_SHORT
                
                ! We just need to add one entry...
                call nc_diag_chaninfo_resize_short(diag_chaninfo_store%nchans)
                
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
                if (diag_chaninfo_store%var_usage(var_index) >= diag_chaninfo_store%nchans) then
                    call error("Can't add new data - data added is exceeding nchan! Data must fit within nchan constraint.")
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
            
            if (diag_chaninfo_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
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
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                ! Expand things first!
                call nc_diag_chaninfo_expand
                
                ! Add to the total!
                diag_chaninfo_store%total = diag_chaninfo_store%total + 1
                
                ! Store name and type!
                diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
                diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_LONG
                
                ! We just need to add one entry...
                call nc_diag_chaninfo_resize_long(diag_chaninfo_store%nchans)
                
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
                if (diag_chaninfo_store%var_usage(var_index) >= diag_chaninfo_store%nchans) then
#ifdef _DEBUG_MEM_
                    print *, "!!!! diag_chaninfo_store%var_usage(var_index)"
                    print *, diag_chaninfo_store%var_usage(var_index)
#endif
                    call error("Can't add new data - data added is exceeding nchan! Data must fit within nchan constraint.")
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
            
            if (diag_chaninfo_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
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
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                ! Expand things first!
                call nc_diag_chaninfo_expand
                
                ! Add to the total!
                diag_chaninfo_store%total = diag_chaninfo_store%total + 1
                
                ! Store name and type!
                diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
                diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_FLOAT
                
                ! We just need to add one entry...
                call nc_diag_chaninfo_resize_rsingle(diag_chaninfo_store%nchans)
                
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
                if (diag_chaninfo_store%var_usage(var_index) >= diag_chaninfo_store%nchans) then
                    call error("Can't add new data - data added is exceeding nchan! Data must fit within nchan constraint.")
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
            
            if (diag_chaninfo_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
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
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                ! Expand things first!
                call nc_diag_chaninfo_expand
                
                ! Add to the total!
                diag_chaninfo_store%total = diag_chaninfo_store%total + 1
                
                ! Store name and type!
                diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
                diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_DOUBLE
                
                ! We just need to add one entry...
                call nc_diag_chaninfo_resize_rdouble(diag_chaninfo_store%nchans)
                
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
                if (diag_chaninfo_store%var_usage(var_index) >= diag_chaninfo_store%nchans) then
                    call error("Can't add new data - data added is exceeding nchan! Data must fit within nchan constraint.")
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
            
            if (diag_chaninfo_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
            end if
            
#ifndef IGNORE_VERSION
            if (NLAYER_STRING_BROKEN) then
                call error("Data string storage not supported with NetCDF v4.2.1.1 or lower.")
            end if
#endif
            
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
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                ! Expand things first!
                call nc_diag_chaninfo_expand
                
                ! Add to the total!
                diag_chaninfo_store%total = diag_chaninfo_store%total + 1
                
                ! Store name and type!
                diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
                diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_STRING
                
                ! We just need to add one entry...
                call nc_diag_chaninfo_resize_string(diag_chaninfo_store%nchans)
                
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
                if (diag_chaninfo_store%var_usage(var_index) >= diag_chaninfo_store%nchans) then
                    call error("Can't add new data - data added is exceeding nchan! Data must fit within nchan constraint.")
                endif
                
                diag_chaninfo_store%var_usage(var_index) = &
                    diag_chaninfo_store%var_usage(var_index) + 1
            end if
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_string(1 + &
                    ((diag_chaninfo_store%var_rel_pos(var_index) - 1) &
                        * diag_chaninfo_store%nchans) &
                    + (diag_chaninfo_store%var_usage(var_index) - 1)) = chaninfo_value
        end subroutine nc_diag_chaninfo_string
        
        !=============================================================
        ! VECTOR TYPES
        !=============================================================
        
        ! nc_diag_chaninfo - input integer(i_byte), dimension(:)
        ! Corresponding NetCDF4 type: byte
        !subroutine nc_diag_chaninfo_byte_v(chaninfo_name, chaninfo_value)
        !    character(len=*), intent(in)               :: chaninfo_name
        !    integer(i_byte), dimension(:), intent(in)  :: chaninfo_value
        !    
        !    integer(i_long)                            :: endpos
        !    integer(i_long)                            :: value_size
        !    
        !    value_size = size(chaninfo_value)
        !    
        !    call nc_diag_chaninfo_expand
        !    
        !    diag_chaninfo_store%total = diag_chaninfo_store%total + 1
        !    
        !    diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
        !    diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_BYTE
        !    
        !    ! Resize to add value_size
        !    call nc_diag_chaninfo_resize_byte(value_size)
        !    
        !    endpos = diag_chaninfo_store%acount(1)
        !    
        !    ! Add the size of the array in:
        !    diag_chaninfo_store%ci_byte_vi(diag_chaninfo_store%acount(6+1)) = value_size
        !    
        !    ! Now add the actual entry!
        !    diag_chaninfo_store%ci_byte(endpos - size(chaninfo_value) + 1:endpos) = chaninfo_value
        !end subroutine nc_diag_chaninfo_byte_v
        
        ! nc_diag_chaninfo - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        !subroutine nc_diag_chaninfo_short_v(chaninfo_name, chaninfo_value)
        !    character(len=*), intent(in)               :: chaninfo_name
        !    integer(i_short), dimension(:), intent(in) :: chaninfo_value
        !    
        !    integer(i_long)                            :: endpos
        !    integer(i_long)                            :: value_size
        !    
        !    value_size = size(chaninfo_value)
        !    
        !    call nc_diag_chaninfo_expand
        !    
        !    diag_chaninfo_store%total = diag_chaninfo_store%total + 1
        !    
        !    diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
        !    diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_SHORT
        !    
        !    ! Resize to add value_size
        !    call nc_diag_chaninfo_resize_short(value_size)
        !    
        !    endpos = diag_chaninfo_store%acount(2)
        !    
        !    ! Add the size of the array in:
        !    diag_chaninfo_store%ci_short_vi(diag_chaninfo_store%acount(6+2)) = value_size
        !    
        !    ! Now add the actual entry!
        !    diag_chaninfo_store%ci_short(endpos - size(chaninfo_value) + 1:endpos) = chaninfo_value
        !end subroutine nc_diag_chaninfo_short_v
        
        ! nc_diag_chaninfo - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        !subroutine nc_diag_chaninfo_long_v(chaninfo_name, chaninfo_value)
        !    character(len=*), intent(in)               :: chaninfo_name
        !    integer(i_long), dimension(:), intent(in)  :: chaninfo_value
        !    
        !    integer(i_long)                            :: endpos
        !    integer(i_long)                            :: value_size
        !    
        !    value_size = size(chaninfo_value)
        !    
        !    call nc_diag_chaninfo_expand
        !    
        !    diag_chaninfo_store%total = diag_chaninfo_store%total + 1
        !    
        !    diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
        !    diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_LONG
        !    
        !    ! Resize to add value_size
        !    call nc_diag_chaninfo_resize_long(value_size)
        !    
        !    endpos = diag_chaninfo_store%acount(3)
        !    
        !    ! Add the size of the array in:
        !    diag_chaninfo_store%ci_long_vi(diag_chaninfo_store%acount(6+3)) = value_size
        !    
        !    ! Now add the actual entry!
        !    diag_chaninfo_store%ci_long(endpos - size(chaninfo_value) + 1:endpos) = chaninfo_value
        !end subroutine nc_diag_chaninfo_long_v
        
        ! nc_diag_chaninfo - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        !subroutine nc_diag_chaninfo_rsingle_v(chaninfo_name, chaninfo_value)
        !    character(len=*), intent(in)               :: chaninfo_name
        !    real(r_single), dimension(:), intent(in)   :: chaninfo_value
        !    
        !    integer(i_long)                            :: endpos
        !    integer(i_long)                            :: value_size
        !    
        !    value_size = size(chaninfo_value)
        !    
        !    call nc_diag_chaninfo_expand
        !    
        !    diag_chaninfo_store%total = diag_chaninfo_store%total + 1
        !    
        !    diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
        !    diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_FLOAT
        !    
        !    ! Resize to add value_size
        !    call nc_diag_chaninfo_resize_rsingle(value_size)
        !    
        !    endpos = diag_chaninfo_store%acount(4)
        !    
        !    ! Add the size of the array in:
        !    diag_chaninfo_store%ci_rsingle_vi(diag_chaninfo_store%acount(6+4)) = value_size
        !    
        !    ! Now add the actual entry!
        !    diag_chaninfo_store%ci_rsingle(endpos - size(chaninfo_value) + 1:endpos) = chaninfo_value
        !end subroutine nc_diag_chaninfo_rsingle_v
        
        ! nc_diag_chaninfo - input real(r_double)
        ! Corresponding NetCDF4 type: double
        !subroutine nc_diag_chaninfo_rdouble_v(chaninfo_name, chaninfo_value)
        !    character(len=*), intent(in)               :: chaninfo_name
        !    real(r_double), dimension(:), intent(in)   :: chaninfo_value
        !    
        !    integer(i_long)                            :: endpos
        !    integer(i_long)                            :: value_size
        !    
        !    value_size = size(chaninfo_value)
        !    
        !    call nc_diag_chaninfo_expand
        !    
        !    diag_chaninfo_store%total = diag_chaninfo_store%total + 1
        !    
        !    diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
        !    diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_DOUBLE
        !    
        !    ! Resize to add value_size
        !    call nc_diag_chaninfo_resize_rdouble(value_size)
        !    
        !    endpos = diag_chaninfo_store%acount(5)
        !    
        !    ! Add the size of the array in:
        !    diag_chaninfo_store%ci_rdouble_vi(diag_chaninfo_store%acount(6+5)) = value_size
        !    
        !    ! Now add the actual entry!
        !    diag_chaninfo_store%ci_rdouble(endpos - value_size:endpos) = chaninfo_value
        !end subroutine nc_diag_chaninfo_rdouble_v
        
        ! String array not available with NF90 attributes
        
        ! nc_diag_chaninfo - input character(len=1000)
        ! Corresponding NetCDF4 type: string
        !subroutine nc_diag_chaninfo_string_v(chaninfo_name, chaninfo_value)
        !    character(len=*),    intent(in)               :: chaninfo_name
        !    character(len=1000), dimension(:), intent(in) :: chaninfo_value
        !    
        !    integer(i_long)                               :: endpos
        !    integer(i_long)                               :: value_size
        !    
        !    value_size = size(chaninfo_value)
        !    
        !    call nc_diag_chaninfo_expand
        !    
        !    diag_chaninfo_store%total = diag_chaninfo_store%total + 1
        !    
        !    diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
        !    diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_STRING
        !    
        !    ! Resize to add value_size
        !    call nc_diag_chaninfo_resize_string(value_size)
        !    
        !    endpos = diag_chaninfo_store%acount(6)
        !    
        !    ! Add the size of the array in:
        !    diag_chaninfo_store%ci_string_vi(diag_chaninfo_store%acount(6+6)) = value_size
        !    
        !    ! Now add the actual entry!
        !    diag_chaninfo_store%ci_string(endpos - value_size:endpos) = chaninfo_value
        !end subroutine nc_diag_chaninfo_string_v
