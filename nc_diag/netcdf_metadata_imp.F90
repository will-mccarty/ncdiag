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
                diag_metadata_store%alloc_hi_multi = multiplier
            end if
        end subroutine nc_diag_metadata_allocmulti
        
        subroutine nc_diag_metadata_write
            integer(i_byte)                       :: data_type
            logical                               :: data_vect
            integer(i_long), dimension(6)         :: data_type_index
            integer(i_long), dimension(5)         :: data_type_index_vi
            character(len=100)                    :: data_name
            
            integer(i_long)               :: curdatindex
            integer(i_long)               :: curdatvecsize
            
            if (init_done) then
                data_type_index    = (/ 1, 1, 1, 1, 1, 1 /)
                data_type_index_vi = (/ 1, 1, 1, 1, 1 /)
                do curdatindex = 1, diag_metadata_store%total
                    data_name = diag_metadata_store%names(curdatindex)
                    data_type = diag_metadata_store%types(curdatindex)
                    data_vect = diag_metadata_store%vectored(curdatindex)
                    
                    if (data_type == NLAYER_BYTE) then
                        if (data_vect) then
                            if (data_type_index_vi(1) <= diag_metadata_store%acount(7)) then
                                ! Grab the vector size, and allocate as needed.
                                curdatvecsize = diag_metadata_store%m_byte_vi(data_type_index_vi(1))
                                data_type_index_vi(1) = data_type_index_vi(1) + 1
                            else
                                call error("Critical error - byte index exceeds internal count!")
                            end if
                            
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_metadata_store%m_byte(data_type_index(1):(data_type_index(1) + curdatvecsize - 1))))
                            data_type_index(1) = data_type_index(1) + curdatvecsize
                        else
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_metadata_store%m_byte(data_type_index(1))))
                            data_type_index(1) = data_type_index(1) + 1
                        end if
                    else if (data_type == NLAYER_SHORT) then
                        if (data_vect) then
                            if (data_type_index_vi(2) <= diag_metadata_store%acount(8)) then
                                ! Grab the vector size, and allocate as needed.
                                curdatvecsize = diag_metadata_store%m_short_vi(data_type_index_vi(2))
                                data_type_index_vi(2) = data_type_index_vi(2) + 1
                            else
                                call error("Critical error - short index exceeds internal count!")
                            end if
                            
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_metadata_store%m_short(data_type_index(2):(data_type_index(2) + curdatvecsize - 1))))
                            data_type_index(2) = data_type_index(2) + curdatvecsize
                        else
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_metadata_store%m_short(data_type_index(2))))
                            data_type_index(2) = data_type_index(2) + 1
                        end if
                    else if (data_type == NLAYER_LONG) then
                        if (data_vect) then
                            if (data_type_index_vi(3) <= diag_metadata_store%acount(9)) then
                                ! Grab the vector size...
                                curdatvecsize = diag_metadata_store%m_long_vi(data_type_index_vi(3))
                                data_type_index_vi(3) = data_type_index_vi(3) + 1
                            else
                                call error("Critical error - long index exceeds internal count!")
                            end if
                            
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_metadata_store%m_long(data_type_index(3):(data_type_index(3) + curdatvecsize - 1))))
                            data_type_index(3) = data_type_index(3) + curdatvecsize
                        else
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_metadata_store%m_long(data_type_index(3))))
                            data_type_index(3) = data_type_index(3) + 1
                        end if
                    else if (data_type == NLAYER_FLOAT) then
                        if (data_vect) then
                            if (data_type_index_vi(4) <= diag_metadata_store%acount(10)) then
                                ! Grab the vector size, and allocate as needed.
                                curdatvecsize = diag_metadata_store%m_rsingle_vi(data_type_index_vi(4))
                                data_type_index_vi(4) = data_type_index_vi(4) + 1
                            else
                                call error("Critical error - rsingle index exceeds internal count!")
                            end if
                            
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_metadata_store%m_rsingle(data_type_index(4):(data_type_index(4) + curdatvecsize - 1))))
                            data_type_index(4) = data_type_index(4) + curdatvecsize
                        else
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_metadata_store%m_rsingle(data_type_index(4))))
                            data_type_index(4) = data_type_index(4) + 1
                        end if
                    else if (data_type == NLAYER_DOUBLE) then
                        if (data_vect) then
                            if (data_type_index_vi(5) <= diag_metadata_store%acount(11)) then
                                ! Grab the vector size, and allocate as needed.
                                curdatvecsize = diag_metadata_store%m_rdouble_vi(data_type_index_vi(5))
                                data_type_index_vi(5) = data_type_index_vi(5) + 1
                            else
                                call error("Critical error - rdouble index exceeds internal count!")
                            end if
                            
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_metadata_store%m_rdouble(data_type_index(5):(data_type_index(5) + curdatvecsize - 1))))
                            data_type_index(5) = data_type_index(5) + curdatvecsize
                        else
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_metadata_store%m_rdouble(data_type_index(5))))
                            data_type_index(5) = data_type_index(5) + 1
                        end if
                    else if (data_type == NLAYER_STRING) then
                        ! String array not available with NF90 attributes
                        !if (data_vect) then
                        !    if (data_type_index_vi(6) <= diag_metadata_store%acount(12)) then
                        !        ! Grab the vector size, and allocate as needed.
                        !        curdatvecsize = diag_metadata_store%m_string_vi(data_type_index_vi(6))
                        !        data_type_index_vi(6) = data_type_index_vi(6) + 1
                        !    else
                        !        call error("Critical error - string index exceeds internal count!")
                        !    end if
                        !    
                        !    data_type_index(6) = data_type_index(6) + curdatvecsize
                        !    call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_metadata_store%m_string(data_type_index(6):(data_type_index(6) + curdatvecsize - 1))))
                        !else
#ifdef _DEBUG_MEM_
                            ! NOTE: trim() is F95
                            print *, "On curdatindex:"
                            print *, curdatindex
                            print *, "For variable:"
                            print *, trim(data_name)
                            print *, "Writing metadata string:"
                            print *, trim(diag_metadata_store%m_string(data_type_index(6)))
#endif
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, trim(diag_metadata_store%m_string(data_type_index(6)))))
                            data_type_index(6) = data_type_index(6) + 1
                        !end if
                    else
                        call error("Critical error - unknown variable type!")
                    end if
                    
                end do
            else
                call error("No nc_diag initialized yet!")
            end if
            
        end subroutine nc_diag_metadata_write
        
        subroutine nc_diag_metadata_expand
            ! Did we realloc at all?
            logical :: meta_realloc
            meta_realloc = .FALSE.
            
            if (init_done .AND. allocated(diag_metadata_store)) then
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
                
                if (allocated(diag_metadata_store%vectored)) then
                    if (diag_metadata_store%total >= size(diag_metadata_store%vectored)) then
#ifdef _DEBUG_MEM_
                        call debug("Reallocating diag_metadata_store%vectored...")
                        print *, (2 ** diag_metadata_store%alloc_s_multi)
                        print *, (1 + (NLAYER_DEFAULT_ENT * (2 ** diag_metadata_store%alloc_s_multi)))
#endif
                        call nc_diag_realloc(diag_metadata_store%vectored, 1 + (NLAYER_DEFAULT_ENT * (2 ** diag_metadata_store%alloc_s_multi)))
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_metadata_store%vectored(NLAYER_DEFAULT_ENT))
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
        
        ! nc_diag_metadata - input integer(i_byte)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_metadata_byte(metadata_name, metadata_value)
            character(len=*), intent(in)    :: metadata_name
            integer(i_byte), intent(in)     :: metadata_value
            
            call nc_diag_metadata_expand
            
            diag_metadata_store%total = diag_metadata_store%total + 1
            
            diag_metadata_store%names(diag_metadata_store%total) = metadata_name
            diag_metadata_store%types(diag_metadata_store%total) = NLAYER_BYTE
            diag_metadata_store%vectored(diag_metadata_store%total) = .FALSE.
            
            ! We just need to add one entry...
            call nc_diag_metadata_resize_byte(1, .FALSE.)
            
            ! Now add the actual entry!
            diag_metadata_store%m_byte(diag_metadata_store%acount(1)) = metadata_value
        end subroutine nc_diag_metadata_byte
        
        ! nc_diag_metadata - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_metadata_short(metadata_name, metadata_value)
            character(len=*), intent(in)    :: metadata_name
            integer(i_short), intent(in)    :: metadata_value
            
            call nc_diag_metadata_expand
            
            diag_metadata_store%total = diag_metadata_store%total + 1
            
            diag_metadata_store%names(diag_metadata_store%total) = metadata_name
            diag_metadata_store%types(diag_metadata_store%total) = NLAYER_SHORT
            diag_metadata_store%vectored(diag_metadata_store%total) = .FALSE.
            
            ! We just need to add one entry...
            call nc_diag_metadata_resize_short(1, .FALSE.)
            
            ! Now add the actual entry!
            diag_metadata_store%m_short(diag_metadata_store%acount(2)) = metadata_value
        end subroutine nc_diag_metadata_short
        
        ! nc_diag_metadata - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_metadata_long(metadata_name, metadata_value)
            character(len=*), intent(in)    :: metadata_name
            integer(i_long), intent(in)     :: metadata_value
            
            call nc_diag_metadata_expand
            
            diag_metadata_store%total = diag_metadata_store%total + 1
            
#ifdef _DEBUG_MEM_
            call debug("Current total:")
            print *, diag_metadata_store%total
#endif
            
            diag_metadata_store%names(diag_metadata_store%total) = metadata_name
            diag_metadata_store%types(diag_metadata_store%total) = NLAYER_LONG
            diag_metadata_store%vectored(diag_metadata_store%total) = .FALSE.
            
            ! We just need to add one entry...
            call nc_diag_metadata_resize_long(1, .FALSE.)
            
            ! Now add the actual entry!
            diag_metadata_store%m_long(diag_metadata_store%acount(3)) = metadata_value
        end subroutine nc_diag_metadata_long
        
        ! nc_diag_metadata - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_metadata_rsingle(metadata_name, metadata_value)
            character(len=*), intent(in)    :: metadata_name
            real(r_single), intent(in)      :: metadata_value
            
            call nc_diag_metadata_expand
            
            diag_metadata_store%total = diag_metadata_store%total + 1
            
            diag_metadata_store%names(diag_metadata_store%total) = metadata_name
            diag_metadata_store%types(diag_metadata_store%total) = NLAYER_FLOAT
            diag_metadata_store%vectored(diag_metadata_store%total) = .FALSE.
            
            ! We just need to add one entry...
            call nc_diag_metadata_resize_rsingle(1, .FALSE.)
            
            ! Now add the actual entry!
            diag_metadata_store%m_rsingle(diag_metadata_store%acount(4)) = metadata_value
        end subroutine nc_diag_metadata_rsingle
        
        ! nc_diag_metadata - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_metadata_rdouble(metadata_name, metadata_value)
            character(len=*), intent(in)    :: metadata_name
            real(r_double), intent(in)      :: metadata_value
            
            call nc_diag_metadata_expand
            
            diag_metadata_store%total = diag_metadata_store%total + 1
            
            diag_metadata_store%names(diag_metadata_store%total) = metadata_name
            diag_metadata_store%types(diag_metadata_store%total) = NLAYER_DOUBLE
            diag_metadata_store%vectored(diag_metadata_store%total) = .FALSE.
            
            ! We just need to add one entry...
            call nc_diag_metadata_resize_rdouble(1, .FALSE.)
            
            ! Now add the actual entry!
            diag_metadata_store%m_rdouble(diag_metadata_store%acount(5)) = metadata_value
        end subroutine nc_diag_metadata_rdouble

        ! nc_diag_metadata - input character(len=*)
        ! Corresponding NetCDF4 type: string? char?
        subroutine nc_diag_metadata_string(metadata_name, metadata_value)
            character(len=*), intent(in)    :: metadata_name
            character(len=*), intent(in)    :: metadata_value
            
            call nc_diag_metadata_expand
            
            diag_metadata_store%total = diag_metadata_store%total + 1
            
            diag_metadata_store%names(diag_metadata_store%total) = metadata_name
            diag_metadata_store%types(diag_metadata_store%total) = NLAYER_STRING
            diag_metadata_store%vectored(diag_metadata_store%total) = .FALSE.
            
            ! We just need to add one entry...
            ! Strings can't be vectored (at least for attributes), so no 2nd argument
            ! here.
            call nc_diag_metadata_resize_string(1)
            
            ! Now add the actual entry!
            diag_metadata_store%m_string(diag_metadata_store%acount(6)) = metadata_value
        end subroutine nc_diag_metadata_string
        
        !=============================================================
        ! VECTOR TYPES
        !=============================================================
        
        ! nc_diag_metadata - input integer(i_byte), dimension(:)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_metadata_byte_v(metadata_name, metadata_value)
            character(len=*), intent(in)               :: metadata_name
            integer(i_byte), dimension(:), intent(in)  :: metadata_value
            
            integer(i_long)                            :: endpos
            integer(i_long)                            :: value_size
            
            value_size = size(metadata_value)
            
            call nc_diag_metadata_expand
            
            diag_metadata_store%total = diag_metadata_store%total + 1
            
            diag_metadata_store%names(diag_metadata_store%total) = metadata_name
            diag_metadata_store%types(diag_metadata_store%total) = NLAYER_BYTE
            diag_metadata_store%vectored(diag_metadata_store%total) = .TRUE.
            
            ! Resize to add value_size
            call nc_diag_metadata_resize_byte(value_size, .TRUE.)
            
            endpos = diag_metadata_store%acount(1)
            
            ! Add the size of the array in:
            diag_metadata_store%m_byte_vi(diag_metadata_store%acount(6+1)) = value_size
            
            ! Now add the actual entry!
            diag_metadata_store%m_byte(endpos - size(metadata_value) + 1:endpos) = metadata_value
        end subroutine nc_diag_metadata_byte_v
        
        ! nc_diag_metadata - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_metadata_short_v(metadata_name, metadata_value)
            character(len=*), intent(in)               :: metadata_name
            integer(i_short), dimension(:), intent(in) :: metadata_value
            
            integer(i_long)                            :: endpos
            integer(i_long)                            :: value_size
            
            value_size = size(metadata_value)
            
            call nc_diag_metadata_expand
            
            diag_metadata_store%total = diag_metadata_store%total + 1
            
            diag_metadata_store%names(diag_metadata_store%total) = metadata_name
            diag_metadata_store%types(diag_metadata_store%total) = NLAYER_SHORT
            diag_metadata_store%vectored(diag_metadata_store%total) = .TRUE.
            
            ! Resize to add value_size
            call nc_diag_metadata_resize_short(value_size, .TRUE.)
            
            endpos = diag_metadata_store%acount(2)
            
            ! Add the size of the array in:
            diag_metadata_store%m_short_vi(diag_metadata_store%acount(6+2)) = value_size
            
            ! Now add the actual entry!
            diag_metadata_store%m_short(endpos - size(metadata_value) + 1:endpos) = metadata_value
        end subroutine nc_diag_metadata_short_v
        
        ! nc_diag_metadata - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_metadata_long_v(metadata_name, metadata_value)
            character(len=*), intent(in)               :: metadata_name
            integer(i_long), dimension(:), intent(in)  :: metadata_value
            
            integer(i_long)                            :: endpos
            integer(i_long)                            :: value_size
            
            value_size = size(metadata_value)
            
            call nc_diag_metadata_expand
            
            diag_metadata_store%total = diag_metadata_store%total + 1
            
            diag_metadata_store%names(diag_metadata_store%total) = metadata_name
            diag_metadata_store%types(diag_metadata_store%total) = NLAYER_LONG
            diag_metadata_store%vectored(diag_metadata_store%total) = .TRUE.
            
            ! Resize to add value_size
            call nc_diag_metadata_resize_long(value_size, .TRUE.)
            
            endpos = diag_metadata_store%acount(3)
            
            ! Add the size of the array in:
            diag_metadata_store%m_long_vi(diag_metadata_store%acount(6+3)) = value_size
            
            ! Now add the actual entry!
            diag_metadata_store%m_long(endpos - size(metadata_value) + 1:endpos) = metadata_value
        end subroutine nc_diag_metadata_long_v
        
        ! nc_diag_metadata - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_metadata_rsingle_v(metadata_name, metadata_value)
            character(len=*), intent(in)               :: metadata_name
            real(r_single), dimension(:), intent(in)   :: metadata_value
            
            integer(i_long)                            :: endpos
            integer(i_long)                            :: value_size
            
            value_size = size(metadata_value)
            
            call nc_diag_metadata_expand
            
            diag_metadata_store%total = diag_metadata_store%total + 1
            
            diag_metadata_store%names(diag_metadata_store%total) = metadata_name
            diag_metadata_store%types(diag_metadata_store%total) = NLAYER_FLOAT
            diag_metadata_store%vectored(diag_metadata_store%total) = .TRUE.
            
            ! Resize to add value_size
            call nc_diag_metadata_resize_rsingle(value_size, .TRUE.)
            
            endpos = diag_metadata_store%acount(4)
            
            ! Add the size of the array in:
            diag_metadata_store%m_rsingle_vi(diag_metadata_store%acount(6+4)) = value_size
            
            ! Now add the actual entry!
            diag_metadata_store%m_rsingle(endpos - size(metadata_value) + 1:endpos) = metadata_value
        end subroutine nc_diag_metadata_rsingle_v
        
        ! nc_diag_metadata - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_metadata_rdouble_v(metadata_name, metadata_value)
            character(len=*), intent(in)               :: metadata_name
            real(r_double), dimension(:), intent(in)   :: metadata_value
            
            integer(i_long)                            :: endpos
            integer(i_long)                            :: value_size
            
            value_size = size(metadata_value)
            
            call nc_diag_metadata_expand
            
            diag_metadata_store%total = diag_metadata_store%total + 1
            
            diag_metadata_store%names(diag_metadata_store%total) = metadata_name
            diag_metadata_store%types(diag_metadata_store%total) = NLAYER_DOUBLE
            diag_metadata_store%vectored(diag_metadata_store%total) = .TRUE.
            
            ! Resize to add value_size
            call nc_diag_metadata_resize_rdouble(value_size, .TRUE.)
            
            endpos = diag_metadata_store%acount(5)
            
            ! Add the size of the array in:
            diag_metadata_store%m_rdouble_vi(diag_metadata_store%acount(6+5)) = value_size
            
            ! Now add the actual entry!
            diag_metadata_store%m_rdouble(endpos - size(metadata_value) + 1:endpos) = metadata_value
        end subroutine nc_diag_metadata_rdouble_v
        
        ! String array not available with NF90 attributes
        
        ! nc_diag_metadata - input character(len=1000)
        ! Corresponding NetCDF4 type: string
        !subroutine nc_diag_metadata_string_v(metadata_name, metadata_value)
        !    character(len=*),    intent(in)               :: metadata_name
        !    character(len=1000), dimension(:), intent(in) :: metadata_value
        !    
        !    integer(i_long)                               :: endpos
        !    integer(i_long)                               :: value_size
        !    
        !    value_size = size(metadata_value)
        !    
        !    call nc_diag_metadata_expand
        !    
        !    diag_metadata_store%total = diag_metadata_store%total + 1
        !    
        !    diag_metadata_store%names(diag_metadata_store%total) = metadata_name
        !    diag_metadata_store%types(diag_metadata_store%total) = NLAYER_STRING
        !    diag_metadata_store%vectored(diag_metadata_store%total) = .TRUE.
        !    
        !    ! Resize to add value_size
        !    call nc_diag_metadata_resize_string(value_size, .TRUE.)
        !    
        !    endpos = diag_metadata_store%acount(6)
        !    
        !    ! Add the size of the array in:
        !    diag_metadata_store%m_string_vi(diag_metadata_store%acount(6+6)) = value_size
        !    
        !    ! Now add the actual entry!
        !    diag_metadata_store%m_string(endpos - value_size:endpos) = metadata_value
        !end subroutine nc_diag_metadata_string_v

