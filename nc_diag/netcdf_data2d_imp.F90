        !===============================================================
        ! nc_diag_data - data handling (implementation)
        !===============================================================
        ! DO NOT COMPILE THIS DIRECTLY! THIS IS MEANT TO BE INCLUDED
        ! INSIDE A LARGER F90 SOURCE!
        ! If you compile this directly, you WILL face the WRATH of your
        ! compiler!
        !---------------------------------------------------------------
        ! Depends on:
        !   netcdf_mresize.F90, netcdf_realloc_data.f90
        ! 
        ! Technically, order shouldn't matter... but just in case,
        ! include netcdf_realloc_imp.f90 FIRST!
        !---------------------------------------------------------------
        ! nc_diag_data stores data data as NetCDF4 global
        ! attributes. The nc_diag_data subroutines temporarily cache
        ! any data data until write, where it will be set by
        ! NF90_PUT_ATT().
        !---------------------------------------------------------------
        ! This file provides the actual data data adding subroutines,
        ! referred to by the interface. It also provides support
        ! subroutines for data writing and allocation setup.
        
        subroutine nc_diag_data2d_write_def
            integer(i_byte)                       :: data_type
            character(len=100)                    :: data_name
            
            character(len=110)                    :: data_dim_name
            
            integer(i_long)                       :: curdatindex
            integer(i_kind)                       :: nc_data_type
            
            if (init_done) then
                call check(nf90_def_dim(ncid, "nobs_data2d", NF90_UNLIMITED, diag_data2d_store%nobs_dim_id))
                
                do curdatindex = 1, diag_data2d_store%total
                    data_name = diag_data2d_store%names(curdatindex)
                    data_type = diag_data2d_store%types(curdatindex)
                    
                    if (data_type == NLAYER_BYTE)   nc_data_type = nf90_byte
                    if (data_type == NLAYER_SHORT)  nc_data_type = nf90_short
                    if (data_type == NLAYER_LONG)   nc_data_type = nf90_int
                    if (data_type == NLAYER_FLOAT)  nc_data_type = nf90_float
                    if (data_type == NLAYER_DOUBLE) nc_data_type = nf90_double
                    if (data_type == NLAYER_STRING) nc_data_type = nf90_char
                    
                    write (data_dim_name, "(A, A)") trim(data_name), "_dim"
                    
                    call check(nf90_def_dim(ncid, data_dim_name, max_len_var(curdatindex), diag_data2d_store%var_dim_ids(curdatindex)))
                    
                    call check(nf90_def_var(ncid, data_name, nc_data_type, &
                        (/ diag_data2d_store%var_dim_ids(curdatindex), diag_data2d_store%nobs_dim_id /), &
                        diag_data2d_store%var_ids(curdatindex)))
                end do
            end if
        end subroutine nc_diag_data2d_write_def
        
        subroutine nc_diag_data2d_write_data
            integer(i_byte)                       :: data_type
            character(len=100)                    :: data_name
            
            integer(i_long)                       :: curdatindex, j, k
            
            if (init_done .AND. allocated(diag_data2d_store)) then
                if (.NOT. diag_data2d_store%data_lock) then
                    do curdatindex = 1, diag_data2d_store%total
                        data_name = diag_data2d_store%names(curdatindex)
                        data_type = diag_data2d_store%types(curdatindex)
                        
                        write (*, "(A, A, A, I0, A)") "Data name: ", trim(data_name), " (type: ", data_type, ")"
                        
                        if (data_type == NLAYER_BYTE) then
                            do j = 1, diag_data2d_store%stores(curdatindex)%acount
                                call check(nf90_put_var(&
                                    ncid, diag_data2d_store%var_ids(curdatindex), &
                                    diag_data2d_store%stores(curdatindex)%storage(j)%byte(1:diag_data2d_store%stores(curdatindex)%storage(j)%acount), &
                                    (/ 1, j /), &
                                    (/ diag_data2d_store%stores(curdatindex)%storage(j)%acount, 1 /) &
                                    ))
                            end do
                        else if (data_type == NLAYER_SHORT) then
                            do j = 1, diag_data2d_store%stores(curdatindex)%acount
                                call check(nf90_put_var(&
                                    ncid, diag_data2d_store%var_ids(curdatindex), &
                                    diag_data2d_store%stores(curdatindex)%storage(j)%short(1:diag_data2d_store%stores(curdatindex)%storage(j)%acount), &
                                    (/ 1, j /), &
                                    (/ diag_data2d_store%stores(curdatindex)%storage(j)%acount, 1 /) &
                                    ))
                            end do
                        else if (data_type == NLAYER_LONG) then
                            do j = 1, diag_data2d_store%stores(curdatindex)%acount
                                if (diag_data2d_store%stores(curdatindex)%storage(j)%acount > 0) then
#ifdef _DEBUG_MEM_
                                    print *, "j = "
                                    print *, j
                                    print *, "acount = "
                                    print *, diag_data2d_store%stores(curdatindex)%storage(j)%acount
                                    print *, "array = "
                                    print *, diag_data2d_store%stores(curdatindex)%storage(j)%long(1:diag_data2d_store%stores(curdatindex)%storage(j)%acount)
#endif
                                    call check(nf90_put_var(&
                                        ncid, diag_data2d_store%var_ids(curdatindex), &
                                        diag_data2d_store%stores(curdatindex)%storage(j)%long(1:diag_data2d_store%stores(curdatindex)%storage(j)%acount), &
                                        (/ 1, j /), &
                                        (/ diag_data2d_store%stores(curdatindex)%storage(j)%acount, 1 /) &
                                        ))
                                end if
                            end do
                        else if (data_type == NLAYER_FLOAT) then
                            do j = 1, diag_data2d_store%stores(curdatindex)%acount
                                call check(nf90_put_var(&
                                    ncid, diag_data2d_store%var_ids(curdatindex), &
                                    diag_data2d_store%stores(curdatindex)%storage(j)%rsingle(1:diag_data2d_store%stores(curdatindex)%storage(j)%acount), &
                                    (/ 1, j /), &
                                    (/ diag_data2d_store%stores(curdatindex)%storage(j)%acount, 1 /) &
                                    ))
                            end do
                        else if (data_type == NLAYER_DOUBLE) then
                            do j = 1, diag_data2d_store%stores(curdatindex)%acount
                                call check(nf90_put_var(&
                                    ncid, diag_data2d_store%var_ids(curdatindex), &
                                    diag_data2d_store%stores(curdatindex)%storage(j)%rdouble(1:diag_data2d_store%stores(curdatindex)%storage(j)%acount), &
                                    (/ 1, j /), &
                                    (/ diag_data2d_store%stores(curdatindex)%storage(j)%acount, 1 /) &
                                    ))
                            end do
                        else if (data_type == NLAYER_STRING) then
                            do j = 1, diag_data2d_store%stores(curdatindex)%acount
                                call check(nf90_put_var(&
                                    ncid, diag_data2d_store%var_ids(curdatindex), &
                                    diag_data2d_store%stores(curdatindex)%storage(j)%string(1:diag_data2d_store%stores(curdatindex)%storage(j)%acount), &
                                    (/ 1, j /), &
                                    (/ diag_data2d_store%stores(curdatindex)%storage(j)%acount, 1 /) &
                                    ))
                            end do
                        end if
                    end do
                else
                    call error("Can't write data - data have already been written and locked!")
                end if
            else
                call error("Can't write data - NetCDF4 layer not initialized yet!")
            end if
        end subroutine nc_diag_data2d_write_data
        
        function max_len_var(var_index) result(max_len)
            integer(i_long), intent(in)    :: var_index
            
            integer :: i, max_len
            
            max_len = -1
            
            do i = 1, diag_data2d_store%stores(var_index)%acount
                if (diag_data2d_store%stores(var_index)%storage(i)%acount > max_len) &
                    max_len = diag_data2d_store%stores(var_index)%storage(i)%acount
            end do
        end function max_len_var
        
        subroutine nc_diag_data2d_expand
            ! Did we realloc at all?
            logical :: meta_realloc
            integer(i_llong) :: tmp_size
            
            meta_realloc = .FALSE.
            
            if (init_done .AND. allocated(diag_data2d_store)) then
                if (allocated(diag_data2d_store%names)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%names)) then
                        tmp_size = size(diag_data2d_store%names) * 0.5
                        call nc_diag_realloc(diag_data2d_store%names, tmp_size)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_data2d_store%names(NLAYER_DEFAULT_ENT))
                end if
                
                if (allocated(diag_data2d_store%types)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%types)) then
                        tmp_size = size(diag_data2d_store%types) * 0.5
                        call nc_diag_realloc(diag_data2d_store%types, tmp_size)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_data2d_store%types(NLAYER_DEFAULT_ENT))
                end if
                
                if (allocated(diag_data2d_store%stores)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%stores)) then
                        tmp_size = size(diag_data2d_store%stores) * 0.5
                        call nc_diag_data2d_resize_storage_type(tmp_size)
                        
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_data2d_store%stores(NLAYER_DEFAULT_ENT))
                end if
                
                if (allocated(diag_data2d_store%var_ids)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%var_ids)) then
                        tmp_size = size(diag_data2d_store%var_ids) * 0.5
                        call nc_diag_realloc(diag_data2d_store%var_ids, tmp_size)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_data2d_store%var_ids(NLAYER_DEFAULT_ENT))
                    diag_data2d_store%var_ids = -1
                end if
                
                if (allocated(diag_data2d_store%var_dim_ids)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%var_dim_ids)) then
                        tmp_size = size(diag_data2d_store%var_dim_ids) * 0.5
                        call nc_diag_realloc(diag_data2d_store%var_dim_ids, tmp_size)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_data2d_store%var_dim_ids(NLAYER_DEFAULT_ENT))
                    diag_data2d_store%var_dim_ids = -1
                end if
            else
                call error("NetCDF4 layer not initialized yet!")
            endif
            
        end subroutine nc_diag_data2d_expand
        
        function nc_diag_data2d_check_var(data_name) result(found)
            character(len=*), intent(in)    :: data_name
            integer :: i
            logical :: found
            found = .FALSE.
            
            if (init_done .AND. allocated(diag_data2d_store)) then
                do i = 1, diag_data2d_store%total
                    if (diag_data2d_store%names(i) == data_name) then
                        found = .TRUE.
                        exit
                    end if
                end do
            end if
        end function nc_diag_data2d_check_var
        
        function nc_diag_data2d_lookup_var(data_name) result(ind)
            character(len=*), intent(in)    :: data_name
            integer :: i, ind
            
            ind = -1
            
            if (init_done .AND. allocated(diag_data2d_store)) then
                do i = 1, diag_data2d_store%total
                    if (diag_data2d_store%names(i) == data_name) then
                        ind = i
                        exit
                    end if
                end do
            end if
        end function nc_diag_data2d_lookup_var
        
        ! nc_diag_data - input integer(i_byte)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_data2d_byte(data_name, data_index, data_value)
            character(len=*), intent(in)              :: data_name
            integer(i_llong), intent(in)              :: data_index
            integer(i_byte), dimension(:), intent(in) :: data_value
            
            integer(i_llong)                 :: var_index
            integer(i_llong)                 :: input_size
            
            if (diag_data2d_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
            end if
            
            print *, "CALL: nc_diag_data2d_byte"
            
            if (.NOT. nc_diag_data2d_check_var(data_name)) then
                ! First, check to make sure we can still define new variables.
                if (diag_data2d_store%def_lock) then
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                call nc_diag_data2d_expand
                
                diag_data2d_store%total = diag_data2d_store%total + 1
                
                diag_data2d_store%names(diag_data2d_store%total) = data_name
                diag_data2d_store%types(diag_data2d_store%total) = NLAYER_BYTE
                
                var_index = diag_data2d_store%total
            else
                var_index = nc_diag_data2d_lookup_var(data_name)
                
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index!")
            end if
            
            input_size = size(data_value)
            
            ! We just need to add one entry...
            call nc_diag_data2d_resize_internal_storage(var_index, data_index)
            call nc_diag_data2d_resize_byte(var_index, data_index, input_size)
            
            ! Now add the actual entry!
            diag_data2d_store%stores(var_index)%storage(data_index)%byte(1:input_size) = data_value
        end subroutine nc_diag_data2d_byte
        
        ! nc_diag_data - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_data2d_short(data_name, data_index, data_value)
            character(len=*), intent(in)               :: data_name
            integer(i_llong), intent(in)               :: data_index
            integer(i_short), dimension(:), intent(in) :: data_value
            
            integer(i_llong)                 :: var_index
            integer(i_llong)                 :: input_size
            
            if (diag_data2d_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
            end if
            
            if (.NOT. nc_diag_data2d_check_var(data_name)) then
                ! First, check to make sure we can still define new variables.
                if (diag_data2d_store%def_lock) then
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                call nc_diag_data2d_expand
                
                diag_data2d_store%total = diag_data2d_store%total + 1
                
                diag_data2d_store%names(diag_data2d_store%total) = data_name
                diag_data2d_store%types(diag_data2d_store%total) = NLAYER_SHORT
                
                var_index = diag_data2d_store%total
            else
                var_index = nc_diag_data2d_lookup_var(data_name)
                
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index!")
            end if
            
            input_size = size(data_value)
            
            ! We just need to add one entry...
            call nc_diag_data2d_resize_internal_storage(var_index, data_index)
            call nc_diag_data2d_resize_short(var_index, data_index, input_size)
            
            ! Now add the actual entry!
            diag_data2d_store%stores(var_index)%storage(data_index)%short(1:input_size) = data_value
        end subroutine nc_diag_data2d_short
        
        ! nc_diag_data - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_data2d_long(data_name, data_index, data_value)
            character(len=*), intent(in)              :: data_name
            integer(i_llong), intent(in)              :: data_index
            integer(i_long), dimension(:), intent(in) :: data_value
            
            integer(i_llong)                 :: var_index
            integer(i_llong)                 :: input_size
            
#ifdef _DEBUG_MEM_
            write (*, "(A, A, A, I0)") " ***** nc_diag_data2d_long: data_name = ", data_name, "; data_index = ", data_index
#endif
            
            if (diag_data2d_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
            end if
            
            if (.NOT. nc_diag_data2d_check_var(data_name)) then
                ! First, check to make sure we can still define new variables.
                if (diag_data2d_store%def_lock) then
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                call nc_diag_data2d_expand
                
                diag_data2d_store%total = diag_data2d_store%total + 1
                
                diag_data2d_store%names(diag_data2d_store%total) = data_name
                diag_data2d_store%types(diag_data2d_store%total) = NLAYER_LONG
                
                var_index = diag_data2d_store%total
            else
                var_index = nc_diag_data2d_lookup_var(data_name)
                
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index!")
            end if
            
            input_size = size(data_value)
            
            ! We just need to add one entry...
            call nc_diag_data2d_resize_internal_storage(var_index, data_index)
            
            if (.NOT. diag_data2d_store%stores(var_index)%storage(data_index)%filled) then
                call nc_diag_data2d_resize_long(var_index, data_index, input_size)
            end if
            
            ! Now add the actual entry!
#ifdef _DEBUG_MEM_
            write (*, "(A, I0)") "var_index = ", var_index
            write (*, "(A, I0)") "data_index = ", data_index
            write (*, "(A, I0)") "size(diag_data2d_store%stores(var_index)%storage(data_index)%long) = ", &
                size(diag_data2d_store%stores(var_index)%storage(data_index)%long)
            write (*, "(A, I0)") "size(data_value) = ", size(data_value)
#endif
            diag_data2d_store%stores(var_index)%storage(data_index)%long(1:input_size) = data_value
        end subroutine nc_diag_data2d_long
        
        ! nc_diag_data - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_data2d_rsingle(data_name, data_index, data_value)
            character(len=*), intent(in)             :: data_name
            integer(i_llong), intent(in)             :: data_index
            real(r_single), dimension(:), intent(in) :: data_value

            integer(i_llong)                 :: var_index
            integer(i_llong)                 :: input_size
            
            if (diag_data2d_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
            end if
            
            if (.NOT. nc_diag_data2d_check_var(data_name)) then
                ! First, check to make sure we can still define new variables.
                if (diag_data2d_store%def_lock) then
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                call nc_diag_data2d_expand
                
                diag_data2d_store%total = diag_data2d_store%total + 1
                
                diag_data2d_store%names(diag_data2d_store%total) = data_name
                diag_data2d_store%types(diag_data2d_store%total) = NLAYER_FLOAT
                
                var_index = diag_data2d_store%total
            else
                var_index = nc_diag_data2d_lookup_var(data_name)
                
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index!")
            end if
            
            input_size = size(data_value)
            
            ! We just need to add one entry...
            call nc_diag_data2d_resize_internal_storage(var_index, data_index)
            call nc_diag_data2d_resize_rsingle(var_index, data_index, input_size)
            
            ! Now add the actual entry!
            diag_data2d_store%stores(var_index)%storage(data_index)%rsingle(1:input_size) = data_value
        end subroutine nc_diag_data2d_rsingle
        
        ! nc_diag_data - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_data2d_rdouble(data_name, data_index, data_value)
            character(len=*), intent(in)             :: data_name
            integer(i_llong), intent(in)             :: data_index
            real(r_double), intent(in), dimension(:) :: data_value

            integer(i_llong)                 :: var_index
            integer(i_llong)                 :: input_size
            
            if (diag_data2d_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
            end if
            
            if (.NOT. nc_diag_data2d_check_var(data_name)) then
                ! First, check to make sure we can still define new variables.
                if (diag_data2d_store%def_lock) then
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                call nc_diag_data2d_expand
                
                diag_data2d_store%total = diag_data2d_store%total + 1
                
                diag_data2d_store%names(diag_data2d_store%total) = data_name
                diag_data2d_store%types(diag_data2d_store%total) = NLAYER_DOUBLE
                
                var_index = diag_data2d_store%total
            else
                var_index = nc_diag_data2d_lookup_var(data_name)
                
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index!")
            end if
            
            input_size = size(data_value)
            
            ! We just need to add one entry...
            call nc_diag_data2d_resize_internal_storage(var_index, data_index)
            call nc_diag_data2d_resize_rdouble(var_index, data_index, input_size)
            
            ! Now add the actual entry!
            diag_data2d_store%stores(var_index)%storage(data_index)%rdouble(1:input_size) = data_value
        end subroutine nc_diag_data2d_rdouble

        ! nc_diag_data - input character(len=*)
        ! Corresponding NetCDF4 type: string? char?
        subroutine nc_diag_data2d_string(data_name, data_index, data_value)
            character(len=*), intent(in)               :: data_name
            integer(i_llong), intent(in)               :: data_index
            character(len=*), dimension(:), intent(in) :: data_value

            integer(i_llong)                 :: var_index
            integer(i_llong)                 :: input_size
            
            if (diag_data2d_store%data_lock) then
                call error("Can't add new data - data have already been written and locked!")
            end if
            
            if (.NOT. nc_diag_data2d_check_var(data_name)) then
                ! First, check to make sure we can still define new variables.
                if (diag_data2d_store%def_lock) then
                    call error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                call nc_diag_data2d_expand
                
                diag_data2d_store%total = diag_data2d_store%total + 1
                
                diag_data2d_store%names(diag_data2d_store%total) = data_name
                diag_data2d_store%types(diag_data2d_store%total) = NLAYER_STRING
                
                var_index = diag_data2d_store%total
            else
                var_index = nc_diag_data2d_lookup_var(data_name)
                
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index!")
            end if
            
            input_size = size(data_value)
            
            ! We just need to add one entry...
            call nc_diag_data2d_resize_internal_storage(var_index, data_index)
            call nc_diag_data2d_resize_string(var_index, data_index, input_size)
            
            ! Now add the actual entry!
            diag_data2d_store%stores(var_index)%storage(data_index)%string(1:input_size) = data_value
        end subroutine nc_diag_data2d_string

