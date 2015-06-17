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
        
        subroutine nc_diag_metadata_write_def
            integer(i_byte)                       :: data_type
            character(len=100)                    :: data_name
            
            integer(i_long)                       :: curdatindex
            integer(i_kind)                       :: nc_data_type
            
            print *, "start metadata define"
            if (init_done) then
                call check(nf90_def_dim(ncid, "nobs", NF90_UNLIMITED, diag_metadata_store%nobs_dim_id))
                
                do curdatindex = 1, diag_metadata_store%total
                    data_name = diag_metadata_store%names(curdatindex)
                    data_type = diag_metadata_store%types(curdatindex)
                    
                    print *, "defining new metadata var:"
                    print *, data_name
                    
                    if (data_type == NLAYER_BYTE)   nc_data_type = nf90_byte
                    if (data_type == NLAYER_SHORT)  nc_data_type = nf90_short
                    if (data_type == NLAYER_LONG)   nc_data_type = nf90_int
                    if (data_type == NLAYER_FLOAT)  nc_data_type = nf90_float
                    if (data_type == NLAYER_DOUBLE) nc_data_type = nf90_double
                    if (data_type == NLAYER_STRING) nc_data_type = nf90_string
                    
                    call check(nf90_def_var(ncid, data_name, nc_data_type, diag_metadata_store%nobs_dim_id, &
                        diag_metadata_store%var_ids(curdatindex)))
                end do
            end if
            print *, "end metadata define"
        end subroutine nc_diag_metadata_write_def
        
        subroutine nc_diag_metadata_write_data
            integer(i_byte)                       :: data_type
            character(len=100)                    :: data_name
            
            integer(i_long)                       :: curdatindex, j
            
            if (init_done) then
                do curdatindex = 1, diag_metadata_store%total
                    data_name = diag_metadata_store%names(curdatindex)
                    data_type = diag_metadata_store%types(curdatindex)
                    
                    if (data_type == NLAYER_BYTE) then
                        do j = 1, diag_metadata_store%stor_i_arr(curdatindex)%icount
                            call check(nf90_put_var(&
                                ncid, diag_metadata_store%var_ids(curdatindex), &
                                diag_metadata_store%m_byte(diag_metadata_store%stor_i_arr(curdatindex)%index_arr(j)), &
                                (/ j /) &
                                ))
                        end do
                    else if (data_type == NLAYER_SHORT) then
                        do j = 1, diag_metadata_store%stor_i_arr(curdatindex)%icount
                            call check(nf90_put_var(&
                                ncid, diag_metadata_store%var_ids(curdatindex), &
                                diag_metadata_store%m_short(diag_metadata_store%stor_i_arr(curdatindex)%index_arr(j)), &
                                (/ j /) &
                                ))
                        end do
                    else if (data_type == NLAYER_LONG) then
                        do j = 1, diag_metadata_store%stor_i_arr(curdatindex)%icount
                            print *, "Writing data!"
                            write (*, "(A, I0, A, I0)") "Data ", j, " stored at: ", diag_metadata_store%stor_i_arr(curdatindex)%index_arr(j)
                            write (*, "(A, I0)") "Actual data: ", diag_metadata_store%m_long(diag_metadata_store%stor_i_arr(curdatindex)%index_arr(j))
                            call check(nf90_put_var(&
                                ncid, diag_metadata_store%var_ids(curdatindex), &
                                diag_metadata_store%m_long(diag_metadata_store%stor_i_arr(curdatindex)%index_arr(j)), &
                                (/ j /) &
                                ))
                        end do
                    else if (data_type == NLAYER_FLOAT) then
                        do j = 1, diag_metadata_store%stor_i_arr(curdatindex)%icount
                            call check(nf90_put_var(&
                                ncid, diag_metadata_store%var_ids(curdatindex), &
                                diag_metadata_store%m_rsingle(diag_metadata_store%stor_i_arr(curdatindex)%index_arr(j)), &
                                (/ j /) &
                                ))
                        end do
                    else if (data_type == NLAYER_DOUBLE) then
                        do j = 1, diag_metadata_store%stor_i_arr(curdatindex)%icount
                            call check(nf90_put_var(&
                                ncid, diag_metadata_store%var_ids(curdatindex), &
                                diag_metadata_store%m_rdouble(diag_metadata_store%stor_i_arr(curdatindex)%index_arr(j)), &
                                (/ j /) &
                                ))
                        end do
                    else if (data_type == NLAYER_STRING) then
#ifndef IGNORE_VERSION
                        ! If you manage to sneak in this far...
                        if (NLAYER_STRING_BROKEN) then
                            call error("Data string storage not supported with NetCDF v4.2.1.1 or lower.")
                        end if
#endif
                        do j = 1, diag_metadata_store%stor_i_arr(curdatindex)%icount
                            call check(nf90_put_var(&
                                ncid, diag_metadata_store%var_ids(curdatindex), &
                                diag_metadata_store%m_string(diag_metadata_store%stor_i_arr(curdatindex)%index_arr(j)), &
                                (/ j /) &
                                ))
                        end do
                    end if
                end do
            end if
        end subroutine nc_diag_metadata_write_data
        
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
        
        subroutine nc_diag_metadata_expand
            ! Did we realloc at all?
            logical :: meta_realloc
            
            meta_realloc = .FALSE.
            
            print *, "expand start"
            
            if (init_done .AND. allocated(diag_metadata_store)) then
                print *, "expand gogogo: init all good"
#ifdef _DEBUG_MEM_
                call debug("INITIAL value of diag_metadata_store%alloc_s_multi:")
                print *, diag_metadata_store%alloc_s_multi
#endif
                
                if (allocated(diag_metadata_store%names)) then
                    if (diag_metadata_store%total >= size(diag_metadata_store%names)) then
#ifdef _DEBUG_MEM_
                        call debug("Reallocating diag_metadata_store%names...")
                        print *, (2 ** diag_metadata_store%alloc_s_multi)
                        print *, (1 + (NLAYER_DEFAULT_ENT * (2 ** diag_metadata_store%alloc_s_multi)))
#endif
                        call nc_diag_realloc(diag_metadata_store%names, 1 + (NLAYER_DEFAULT_ENT * (2 ** diag_metadata_store%alloc_s_multi)))
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
                        print *, (2 ** diag_metadata_store%alloc_s_multi)
                        print *, (1 + (NLAYER_DEFAULT_ENT * (2 ** diag_metadata_store%alloc_s_multi)))
#endif
                        call nc_diag_realloc(diag_metadata_store%types, 1 + (NLAYER_DEFAULT_ENT * (2 ** diag_metadata_store%alloc_s_multi)))
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_metadata_store%types(NLAYER_DEFAULT_ENT))
                end if
                
                if (allocated(diag_metadata_store%stor_i_arr)) then
                    if (diag_metadata_store%total >= size(diag_metadata_store%stor_i_arr)) then
#ifdef _DEBUG_MEM_
                        call debug("Reallocating diag_metadata_store%stor_i_arr...")
                        print *, (2 ** diag_metadata_store%alloc_s_multi)
                        print *, (1 + (NLAYER_DEFAULT_ENT * (2 ** diag_metadata_store%alloc_s_multi)))
#endif
                        call nc_diag_metadata_resize_iarr_type((NLAYER_DEFAULT_ENT * (2 ** diag_metadata_store%alloc_s_multi)))
                        
                        !call nc_diag_realloc(diag_metadata_store%stor_i_arr, 1 + (NLAYER_DEFAULT_ENT * (2 ** diag_metadata_store%alloc_s_multi)))
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_metadata_store%stor_i_arr(NLAYER_DEFAULT_ENT))
                end if
                
                if (allocated(diag_metadata_store%var_ids)) then
                    if (diag_metadata_store%total >= size(diag_metadata_store%var_ids)) then
                        call nc_diag_realloc(diag_metadata_store%var_ids, 1 + (NLAYER_DEFAULT_ENT * (2 ** diag_metadata_store%alloc_s_multi)))
                        meta_realloc = .TRUE.
                        print *, "var_ids REALLOCATED"
                    end if
                else
                    allocate(diag_metadata_store%var_ids(NLAYER_DEFAULT_ENT))
                    diag_metadata_store%var_ids = -1
                    print *, "var_ids ALLOCATED"
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
        
        function nc_diag_metadata_check_var(metadata_name) result(found)
            character(len=*), intent(in)    :: metadata_name
            integer :: i
            logical :: found
            found = .FALSE.
            
            if (init_done .AND. allocated(diag_metadata_store)) then
                do i = 1, diag_metadata_store%total
                    if (diag_metadata_store%names(i) == metadata_name) then
                        found = .TRUE.
                        exit
                    end if
                end do
            end if
        end function nc_diag_metadata_check_var
        
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
            
            if (.NOT. nc_diag_metadata_check_var(metadata_name)) then
                call nc_diag_metadata_expand
                
                diag_metadata_store%total = diag_metadata_store%total + 1
                
                diag_metadata_store%names(diag_metadata_store%total) = metadata_name
                diag_metadata_store%types(diag_metadata_store%total) = NLAYER_BYTE
                
                var_index = diag_metadata_store%total
            else
                var_index = nc_diag_metadata_lookup_var(metadata_name)
                
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index!")
            end if
            
            ! We just need to add one entry...
            call nc_diag_metadata_resize_iarr(var_index, 1, 1)
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
            
            if (.NOT. nc_diag_metadata_check_var(metadata_name)) then
                call nc_diag_metadata_expand
                
                diag_metadata_store%total = diag_metadata_store%total + 1
                
                diag_metadata_store%names(diag_metadata_store%total) = metadata_name
                diag_metadata_store%types(diag_metadata_store%total) = NLAYER_SHORT
                
                var_index = diag_metadata_store%total
            else
                var_index = nc_diag_metadata_lookup_var(metadata_name)
                
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index!")
            end if
            
            ! We just need to add one entry...
            call nc_diag_metadata_resize_iarr(var_index, 2, 1)
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
            
            if (.NOT. nc_diag_metadata_check_var(metadata_name)) then
                print *, "metadata: long var doesn't exist, go create it!"
                call nc_diag_metadata_expand
                print *, "metadata: diag_metadata_store%var_ids(1) is:"
                print *, diag_metadata_store%var_ids(1)
                
                diag_metadata_store%total = diag_metadata_store%total + 1
                
                diag_metadata_store%names(diag_metadata_store%total) = metadata_name
                diag_metadata_store%types(diag_metadata_store%total) = NLAYER_LONG
                
                var_index = diag_metadata_store%total
            else
                print *, "metadata: long var exists, fetch the index!"
                var_index = nc_diag_metadata_lookup_var(metadata_name)
                
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index!")
            end if
            
#ifdef _DEBUG_MEM_
            call debug("Current total:")
            print *, diag_metadata_store%total
#endif
            
            ! We just need to add one entry...
            call nc_diag_metadata_resize_iarr(var_index, 3, 1)
            call nc_diag_metadata_resize_long(1)
            
            ! Now add the actual entry!
            diag_metadata_store%m_long(diag_metadata_store%acount(3)) = metadata_value
            write (*, "(A, I0)") "diag_metadata_store%stor_i_arr(var_index)%icount = ", diag_metadata_store%stor_i_arr(var_index)%icount
            write (*, "(A, I0)") "diag_metadata_store%acount(1) =", diag_metadata_store%acount(3)
            !write (*, "(A, I") "diag_metadata_store%stor_i_arr(var_index)%index_arr(diag_metadata_store%stor_i_arr(var_index)%icount)"
            
            diag_metadata_store%stor_i_arr(var_index)%index_arr(diag_metadata_store%stor_i_arr(var_index)%icount) = &
                diag_metadata_store%acount(3)
        end subroutine nc_diag_metadata_long
        
        ! nc_diag_metadata - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_metadata_rsingle(metadata_name, metadata_value)
            character(len=*), intent(in)    :: metadata_name
            real(r_single), intent(in)      :: metadata_value

            integer(i_long)                 :: var_index
            
            if (.NOT. nc_diag_metadata_check_var(metadata_name)) then
                call nc_diag_metadata_expand
                
                diag_metadata_store%total = diag_metadata_store%total + 1
                
                diag_metadata_store%names(diag_metadata_store%total) = metadata_name
                diag_metadata_store%types(diag_metadata_store%total) = NLAYER_FLOAT
                
                var_index = diag_metadata_store%total
            else
                var_index = nc_diag_metadata_lookup_var(metadata_name)
                
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index!")
            end if
            
            ! We just need to add one entry...
            call nc_diag_metadata_resize_iarr(var_index, 4, 1)
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
            
            if (.NOT. nc_diag_metadata_check_var(metadata_name)) then
                call nc_diag_metadata_expand
                
                diag_metadata_store%total = diag_metadata_store%total + 1
                
                diag_metadata_store%names(diag_metadata_store%total) = metadata_name
                diag_metadata_store%types(diag_metadata_store%total) = NLAYER_DOUBLE
                
                var_index = diag_metadata_store%total
            else
                var_index = nc_diag_metadata_lookup_var(metadata_name)
                
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index!")
            end if
            
            ! We just need to add one entry...
            call nc_diag_metadata_resize_iarr(var_index, 5, 1)
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
            
#ifndef IGNORE_VERSION
            if (NLAYER_STRING_BROKEN) then
                call error("Data string storage not supported with NetCDF v4.2.1.1 or lower.")
            end if
#endif
            
            if (.NOT. nc_diag_metadata_check_var(metadata_name)) then
                call nc_diag_metadata_expand
                
                diag_metadata_store%total = diag_metadata_store%total + 1
                
                diag_metadata_store%names(diag_metadata_store%total) = metadata_name
                diag_metadata_store%types(diag_metadata_store%total) = NLAYER_STRING
                
                var_index = diag_metadata_store%total
            else
                var_index = nc_diag_metadata_lookup_var(metadata_name)
                
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index!")
            end if
            
            ! We just need to add one entry...
            ! Strings can't be vectored (at least for attributes), so no 2nd argument
            ! here.
            call nc_diag_metadata_resize_iarr(var_index, 6, 1)
            call nc_diag_metadata_resize_string(1)
            
            ! Now add the actual entry!
            diag_metadata_store%m_string(diag_metadata_store%acount(6)) = metadata_value
            diag_metadata_store%stor_i_arr(var_index)%index_arr(diag_metadata_store%stor_i_arr(var_index)%icount) = &
                diag_metadata_store%acount(6)
        end subroutine nc_diag_metadata_string

