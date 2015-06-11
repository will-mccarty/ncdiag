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
        
        subroutine nc_diag_chaninfo_dim_set
            integer(i_long), intent(in) :: nchan
            if (init_done .AND. allocated(diag_chaninfo_store)) then
                if (nchan < 1) then
                    call error("Critical error - specified a nchan < 1!")
                end if
                
                diag_chaninfo_store%nchan = nchan
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
                
                ! # of times we needed to realloc chaninfo data storage
                ! also the multiplier factor for allocation (2^x)
                diag_chaninfo_store%alloc_ci_multi = multiplier
                
                ! # of times we needed to realloc chaninfo INDEX data storage
                ! also the multiplier factor for allocation (2^x)
                diag_chaninfo_store%alloc_hi_multi = multiplier
            end if
        end subroutine nc_diag_chaninfo_allocmulti
        
        subroutine nc_diag_chaninfo_write
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
                do curdatindex = 1, diag_chaninfo_store%total
                    data_name = diag_chaninfo_store%names(curdatindex)
                    data_type = diag_chaninfo_store%types(curdatindex)
                    
                    if (data_type == NLAYER_BYTE) then
                        if (data_vect) then
                            if (data_type_index_vi(1) <= diag_chaninfo_store%acount(7)) then
                                ! Grab the vector size, and allocate as needed.
                                curdatvecsize = diag_chaninfo_store%ci_byte_vi(data_type_index_vi(1))
                                data_type_index_vi(1) = data_type_index_vi(1) + 1
                            else
                                call error("Critical error - byte index exceeds internal count!")
                            end if
                            
                            data_type_index(1) = data_type_index(1) + curdatvecsize
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_chaninfo_store%ci_byte(data_type_index(1):(data_type_index(1) + curdatvecsize))))
                        else
                            data_type_index(1) = data_type_index(1) + 1
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_chaninfo_store%ci_byte(data_type_index(1))))
                        end if
                    else if (data_type == NLAYER_SHORT) then
                        if (data_vect) then
                            if (data_type_index_vi(2) <= diag_chaninfo_store%acount(8)) then
                                ! Grab the vector size, and allocate as needed.
                                curdatvecsize = diag_chaninfo_store%ci_short_vi(data_type_index_vi(2))
                                data_type_index_vi(2) = data_type_index_vi(2) + 1
                            else
                                call error("Critical error - short index exceeds internal count!")
                            end if
                            
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_chaninfo_store%ci_short(data_type_index(2):(data_type_index(2) + curdatvecsize))))
                            data_type_index(2) = data_type_index(2) + curdatvecsize
                        else
                            data_type_index(2) = data_type_index(2) + 1
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_chaninfo_store%ci_short(data_type_index(2))))
                        end if
                    else if (data_type == NLAYER_LONG) then
                        if (data_vect) then
                            if (data_type_index_vi(3) <= diag_chaninfo_store%acount(9)) then
                                ! Grab the vector size...
                                curdatvecsize = diag_chaninfo_store%ci_long_vi(data_type_index_vi(3))
                                data_type_index_vi(3) = data_type_index_vi(3) + 1
                            else
                                call error("Critical error - long index exceeds internal count!")
                            end if
                            
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_chaninfo_store%ci_long(data_type_index(3):(data_type_index(3) + curdatvecsize))))
                            data_type_index(3) = data_type_index(3) + curdatvecsize
                        else
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_chaninfo_store%ci_long(data_type_index(3))))
                            data_type_index(3) = data_type_index(3) + 1
                        end if
                    else if (data_type == NLAYER_FLOAT) then
                        if (data_vect) then
                            if (data_type_index_vi(4) <= diag_chaninfo_store%acount(10)) then
                                ! Grab the vector size, and allocate as needed.
                                curdatvecsize = diag_chaninfo_store%ci_rsingle_vi(data_type_index_vi(4))
                                data_type_index_vi(4) = data_type_index_vi(4) + 1
                            else
                                call error("Critical error - rsingle index exceeds internal count!")
                            end if
                            
                            data_type_index(4) = data_type_index(4) + curdatvecsize
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_chaninfo_store%ci_rsingle(data_type_index(4):(data_type_index(4) + curdatvecsize))))
                        else
                            data_type_index(4) = data_type_index(4) + 1
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_chaninfo_store%ci_rsingle(data_type_index(4))))
                        end if
                    else if (data_type == NLAYER_DOUBLE) then
                        if (data_vect) then
                            if (data_type_index_vi(5) <= diag_chaninfo_store%acount(11)) then
                                ! Grab the vector size, and allocate as needed.
                                curdatvecsize = diag_chaninfo_store%ci_rdouble_vi(data_type_index_vi(5))
                                data_type_index_vi(5) = data_type_index_vi(5) + 1
                            else
                                call error("Critical error - rdouble index exceeds internal count!")
                            end if
                            
                            data_type_index(5) = data_type_index(5) + curdatvecsize
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_chaninfo_store%ci_rdouble(data_type_index(5):(data_type_index(5) + curdatvecsize))))
                        else
                            data_type_index(5) = data_type_index(5) + 1
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_chaninfo_store%ci_rdouble(data_type_index(5))))
                        end if
                    else if (data_type == NLAYER_STRING) then
                        ! String array not available with NF90 attributes
                        if (data_vect) then
                            if (data_type_index_vi(6) <= diag_chaninfo_store%acount(12)) then
                                ! Grab the vector size, and allocate as needed.
                                curdatvecsize = diag_chaninfo_store%ci_string_vi(data_type_index_vi(6))
                                data_type_index_vi(6) = data_type_index_vi(6) + 1
                            else
                                call error("Critical error - string index exceeds internal count!")
                            end if
                            
                            data_type_index(6) = data_type_index(6) + curdatvecsize
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_chaninfo_store%ci_string(data_type_index(6):(data_type_index(6) + curdatvecsize))))
                        else
                            data_type_index(6) = data_type_index(6) + 1
                            call check(nf90_put_att(ncid, NF90_GLOBAL, data_name, diag_chaninfo_store%ci_string(data_type_index(6))))
                        end if
                    else
                        call error("Critical error - unknown variable type!")
                    end if
                    
                end do
            else
                call error("No nc_diag initialized yet!")
            end if
            
        end subroutine nc_diag_chaninfo_write
        
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
            
            call nc_diag_chaninfo_expand
            
            diag_chaninfo_store%total = diag_chaninfo_store%total + 1
            
            diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_BYTE
                        
            ! We just need to add one entry...
            call nc_diag_chaninfo_resize_byte(1)
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_byte(diag_chaninfo_store%acount(1)) = chaninfo_value
        end subroutine nc_diag_chaninfo_byte
        
        ! nc_diag_chaninfo - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_chaninfo_short(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)    :: chaninfo_name
            integer(i_short), intent(in)    :: chaninfo_value
            
            call nc_diag_chaninfo_expand
            
            diag_chaninfo_store%total = diag_chaninfo_store%total + 1
            
            diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_SHORT
                        
            ! We just need to add one entry...
            call nc_diag_chaninfo_resize_short(1)
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_short(diag_chaninfo_store%acount(2)) = chaninfo_value
        end subroutine nc_diag_chaninfo_short
        
        ! nc_diag_chaninfo - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_chaninfo_long(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)    :: chaninfo_name
            integer(i_long), intent(in)     :: chaninfo_value
            
            call nc_diag_chaninfo_expand
            
            diag_chaninfo_store%total = diag_chaninfo_store%total + 1
            
            diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_LONG
                        
            ! We just need to add one entry...
            call nc_diag_chaninfo_resize_long(1)
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_long(diag_chaninfo_store%acount(3)) = chaninfo_value
        end subroutine nc_diag_chaninfo_long
        
        ! nc_diag_chaninfo - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_chaninfo_rsingle(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)    :: chaninfo_name
            real(r_single), intent(in)      :: chaninfo_value
            
            call nc_diag_chaninfo_expand
            
            diag_chaninfo_store%total = diag_chaninfo_store%total + 1
            
            diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_FLOAT
                        
            ! We just need to add one entry...
            call nc_diag_chaninfo_resize_rsingle(1)
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_rsingle(diag_chaninfo_store%acount(4)) = chaninfo_value
        end subroutine nc_diag_chaninfo_rsingle
        
        ! nc_diag_chaninfo - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_chaninfo_rdouble(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)    :: chaninfo_name
            real(r_double), intent(in)      :: chaninfo_value
            
            call nc_diag_chaninfo_expand
            
            diag_chaninfo_store%total = diag_chaninfo_store%total + 1
            
            diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_DOUBLE
            
            ! We just need to add one entry...
            call nc_diag_chaninfo_resize_rdouble(1)
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_rdouble(diag_chaninfo_store%acount(5)) = chaninfo_value
        end subroutine nc_diag_chaninfo_rdouble

        ! nc_diag_chaninfo - input character(len=*)
        ! Corresponding NetCDF4 type: string? char?
        subroutine nc_diag_chaninfo_string(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)    :: chaninfo_name
            character(len=*), intent(in)    :: chaninfo_value
            
            call nc_diag_chaninfo_expand
            
            diag_chaninfo_store%total = diag_chaninfo_store%total + 1
            
            diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_STRING
            
            ! We just need to add one entry...
            ! Strings can't be vectored (at least for attributes), so no 2nd argument
            ! here.
            call nc_diag_chaninfo_resize_string(1)
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_string(diag_chaninfo_store%acount(6)) = chaninfo_value
        end subroutine nc_diag_chaninfo_string
        
        !=============================================================
        ! VECTOR TYPES
        !=============================================================
        
        ! nc_diag_chaninfo - input integer(i_byte), dimension(:)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_chaninfo_byte_v(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)               :: chaninfo_name
            integer(i_byte), dimension(:), intent(in)  :: chaninfo_value
            
            integer(i_long)                            :: endpos
            integer(i_long)                            :: value_size
            
            value_size = size(chaninfo_value)
            
            call nc_diag_chaninfo_expand
            
            diag_chaninfo_store%total = diag_chaninfo_store%total + 1
            
            diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_BYTE
            
            ! Resize to add value_size
            call nc_diag_chaninfo_resize_byte(value_size)
            
            endpos = diag_chaninfo_store%acount(1)
            
            ! Add the size of the array in:
            diag_chaninfo_store%ci_byte_vi(diag_chaninfo_store%acount(6+1)) = value_size
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_byte(endpos - size(chaninfo_value) + 1:endpos) = chaninfo_value
        end subroutine nc_diag_chaninfo_byte_v
        
        ! nc_diag_chaninfo - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_chaninfo_short_v(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)               :: chaninfo_name
            integer(i_short), dimension(:), intent(in) :: chaninfo_value
            
            integer(i_long)                            :: endpos
            integer(i_long)                            :: value_size
            
            value_size = size(chaninfo_value)
            
            call nc_diag_chaninfo_expand
            
            diag_chaninfo_store%total = diag_chaninfo_store%total + 1
            
            diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_SHORT
            
            ! Resize to add value_size
            call nc_diag_chaninfo_resize_short(value_size)
            
            endpos = diag_chaninfo_store%acount(2)
            
            ! Add the size of the array in:
            diag_chaninfo_store%ci_short_vi(diag_chaninfo_store%acount(6+2)) = value_size
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_short(endpos - size(chaninfo_value) + 1:endpos) = chaninfo_value
        end subroutine nc_diag_chaninfo_short_v
        
        ! nc_diag_chaninfo - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_chaninfo_long_v(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)               :: chaninfo_name
            integer(i_long), dimension(:), intent(in)  :: chaninfo_value
            
            integer(i_long)                            :: endpos
            integer(i_long)                            :: value_size
            
            value_size = size(chaninfo_value)
            
            call nc_diag_chaninfo_expand
            
            diag_chaninfo_store%total = diag_chaninfo_store%total + 1
            
            diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_LONG
            
            ! Resize to add value_size
            call nc_diag_chaninfo_resize_long(value_size)
            
            endpos = diag_chaninfo_store%acount(3)
            
            ! Add the size of the array in:
            diag_chaninfo_store%ci_long_vi(diag_chaninfo_store%acount(6+3)) = value_size
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_long(endpos - size(chaninfo_value) + 1:endpos) = chaninfo_value
        end subroutine nc_diag_chaninfo_long_v
        
        ! nc_diag_chaninfo - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_chaninfo_rsingle_v(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)               :: chaninfo_name
            real(r_single), dimension(:), intent(in)   :: chaninfo_value
            
            integer(i_long)                            :: endpos
            integer(i_long)                            :: value_size
            
            value_size = size(chaninfo_value)
            
            call nc_diag_chaninfo_expand
            
            diag_chaninfo_store%total = diag_chaninfo_store%total + 1
            
            diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_FLOAT
            
            ! Resize to add value_size
            call nc_diag_chaninfo_resize_rsingle(value_size)
            
            endpos = diag_chaninfo_store%acount(4)
            
            ! Add the size of the array in:
            diag_chaninfo_store%ci_rsingle_vi(diag_chaninfo_store%acount(6+4)) = value_size
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_rsingle(endpos - size(chaninfo_value) + 1:endpos) = chaninfo_value
        end subroutine nc_diag_chaninfo_rsingle_v
        
        ! nc_diag_chaninfo - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_chaninfo_rdouble_v(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)               :: chaninfo_name
            real(r_double), dimension(:), intent(in)   :: chaninfo_value
            
            integer(i_long)                            :: endpos
            integer(i_long)                            :: value_size
            
            value_size = size(chaninfo_value)
            
            call nc_diag_chaninfo_expand
            
            diag_chaninfo_store%total = diag_chaninfo_store%total + 1
            
            diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_DOUBLE
            
            ! Resize to add value_size
            call nc_diag_chaninfo_resize_rdouble(value_size)
            
            endpos = diag_chaninfo_store%acount(5)
            
            ! Add the size of the array in:
            diag_chaninfo_store%ci_rdouble_vi(diag_chaninfo_store%acount(6+5)) = value_size
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_rdouble(endpos - value_size:endpos) = chaninfo_value
        end subroutine nc_diag_chaninfo_rdouble_v
        
        ! String array not available with NF90 attributes
        
        ! nc_diag_chaninfo - input character(len=1000)
        ! Corresponding NetCDF4 type: string
        subroutine nc_diag_chaninfo_string_v(chaninfo_name, chaninfo_value)
            character(len=*),    intent(in)               :: chaninfo_name
            character(len=1000), dimension(:), intent(in) :: chaninfo_value
            
            integer(i_long)                               :: endpos
            integer(i_long)                               :: value_size
            
            value_size = size(chaninfo_value)
            
            call nc_diag_chaninfo_expand
            
            diag_chaninfo_store%total = diag_chaninfo_store%total + 1
            
            diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_STRING
            
            ! Resize to add value_size
            call nc_diag_chaninfo_resize_string(value_size)
            
            endpos = diag_chaninfo_store%acount(6)
            
            ! Add the size of the array in:
            diag_chaninfo_store%ci_string_vi(diag_chaninfo_store%acount(6+6)) = value_size
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_string(endpos - value_size:endpos) = chaninfo_value
        end subroutine nc_diag_chaninfo_string_v
