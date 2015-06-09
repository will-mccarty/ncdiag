        ! ---------------------------
        ! nc_diag_header definitions
        ! ---------------------------
        ! DO NOT COMPILE THIS DIRECTLY! THIS IS MEANT TO BE INCLUDED
        ! INSIDE A LARGER F90 SOURCE!
        ! If you compile this directly, you WILL face the WRATH of your
        ! compiler!
        
        ! Depends on: netcdf_realloc_header.f90
        ! Technically, order shouldn't matter... but just in case,
        ! include netcdf_realloc_header.f90 FIRST!
        
        ! nc_diag_header subroutines correspond to the global atributes,
        ! set by NF90_PUT_ATT()
        
        ! This file provides the actual subroutines, referred to by the
        ! interface.
        
        subroutine nc_diag_header_expand
            if (init_done .AND. allocated(diag_header_store)) then
                if (allocated(diag_header_store%names)) then
                    if (diag_header_store%total == size(diag_header_store%names)) then
                        call debug("Reallocating diag_header_store%names...")
                        call nc_diag_realloc(diag_header_store%names, 1)
                    end if
                else
                    call debug("Allocating diag_header_store%names for first time...")
                    print *, NLAYER_DEFAULT_ENT
                    allocate(diag_header_store%names(NLAYER_DEFAULT_ENT))
                    call debug("Allocated diag_header_store%names. Size:")
                    print *, size(diag_header_store%names)
                end if
                
                if (allocated(diag_header_store%types)) then
                    if (diag_header_store%total == size(diag_header_store%types)) then
                        call nc_diag_realloc(diag_header_store%types, 1)
                    end if
                else
                    allocate(diag_header_store%types(NLAYER_DEFAULT_ENT))
                end if
                
                if (allocated(diag_header_store%vectored)) then
                    if (diag_header_store%total == size(diag_header_store%vectored)) then
                        call nc_diag_realloc(diag_header_store%vectored, 1)
                    end if
                else
                    allocate(diag_header_store%vectored(NLAYER_DEFAULT_ENT))
                end if
            else
                call error("NetCDF4 layer not initialized yet!")
            endif
        end subroutine nc_diag_header_expand
        
        ! nc_diag_header - input integer(i_byte)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_header_byte(header_name, header_value)
            character(len=*), intent(in)    :: header_name
            integer(i_byte), intent(in)     :: header_value
            
            call nc_diag_header_expand
            
            diag_header_store%total = diag_header_store%total + 1
            
            diag_header_store%names(diag_header_store%total) = header_name
            diag_header_store%types(diag_header_store%total) = NLAYER_BYTE
            diag_header_store%vectored(diag_header_store%total) = .FALSE.
            
            ! We just need to add one entry...
            call nc_diag_header_resize_byte(1, .FALSE.)
            
            ! Now add the actual entry!
            diag_header_store%h_byte(diag_header_store%acount(1)) = header_value
        end subroutine nc_diag_header_byte
        
        ! nc_diag_header - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_header_short(header_name, header_value)
            character(len=*), intent(in)    :: header_name
            integer(i_short), intent(in)    :: header_value
            
            call nc_diag_header_expand
            
            diag_header_store%total = diag_header_store%total + 1
            
            diag_header_store%names(diag_header_store%total) = header_name
            diag_header_store%types(diag_header_store%total) = NLAYER_SHORT
            diag_header_store%vectored(diag_header_store%total) = .FALSE.
            
            ! We just need to add one entry...
            call nc_diag_header_resize_short(1, .FALSE.)
            
            ! Now add the actual entry!
            diag_header_store%h_short(diag_header_store%acount(2)) = header_value
        end subroutine nc_diag_header_short
        
        ! nc_diag_header - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_header_long(header_name, header_value)
            character(len=*), intent(in)    :: header_name
            integer(i_long), intent(in)     :: header_value
            
            call nc_diag_header_expand
            
            diag_header_store%total = diag_header_store%total + 1
            
            call debug("Current total:")
            print *, diag_header_store%total
            
            diag_header_store%names(diag_header_store%total) = header_name
            diag_header_store%types(diag_header_store%total) = NLAYER_LONG
            diag_header_store%vectored(diag_header_store%total) = .FALSE.
            
            ! We just need to add one entry...
            call nc_diag_header_resize_long(1, .FALSE.)
            
            ! Now add the actual entry!
            diag_header_store%h_long(diag_header_store%acount(3)) = header_value
        end subroutine nc_diag_header_long
        
        ! nc_diag_header - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_header_rsingle(header_name, header_value)
            character(len=*), intent(in)    :: header_name
            real(r_single), intent(in)      :: header_value
            
            call nc_diag_header_expand
            
            diag_header_store%total = diag_header_store%total + 1
            
            diag_header_store%names(diag_header_store%total) = header_name
            diag_header_store%types(diag_header_store%total) = NLAYER_FLOAT
            diag_header_store%vectored(diag_header_store%total) = .FALSE.
            
            ! We just need to add one entry...
            call nc_diag_header_resize_rsingle(1, .FALSE.)
            
            ! Now add the actual entry!
            diag_header_store%h_rsingle(diag_header_store%acount(4)) = header_value
        end subroutine nc_diag_header_rsingle
        
        ! nc_diag_header - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_header_rdouble(header_name, header_value)
            character(len=*), intent(in)    :: header_name
            real(r_double), intent(in)      :: header_value
            
            call nc_diag_header_expand
            
            diag_header_store%total = diag_header_store%total + 1
            
            diag_header_store%names(diag_header_store%total) = header_name
            diag_header_store%types(diag_header_store%total) = NLAYER_DOUBLE
            diag_header_store%vectored(diag_header_store%total) = .FALSE.
            
            ! We just need to add one entry...
            call nc_diag_header_resize_rdouble(1, .FALSE.)
            
            ! Now add the actual entry!
            diag_header_store%h_rdouble(diag_header_store%acount(5)) = header_value
        end subroutine nc_diag_header_rdouble

        ! nc_diag_header - input character(len=*)
        ! Corresponding NetCDF4 type: string? char?
        subroutine nc_diag_header_string(header_name, header_value)
            character(len=*), intent(in)    :: header_name
            character(len=*), intent(in)    :: header_value
            
            call nc_diag_header_expand
            
            diag_header_store%total = diag_header_store%total + 1
            
            diag_header_store%names(diag_header_store%total) = header_name
            diag_header_store%types(diag_header_store%total) = NLAYER_STRING
            diag_header_store%vectored(diag_header_store%total) = .FALSE.
            
            ! We just need to add one entry...
            call nc_diag_header_resize_string(1, .FALSE.)
            
            ! Now add the actual entry!
            diag_header_store%h_string(diag_header_store%acount(6)) = header_value
        end subroutine nc_diag_header_string
        
        !=============================================================
        ! VECTOR TYPES
        !=============================================================
        
        ! nc_diag_header - input integer(i_byte), dimension(:)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_header_byte_v(header_name, header_value)
            character(len=*), intent(in)               :: header_name
            integer(i_byte), dimension(:), intent(in)  :: header_value
            
            integer(i_long)                            :: endpos
            integer(i_long)                            :: value_size
            
            value_size = size(header_value)
            
            call nc_diag_header_expand
            
            diag_header_store%total = diag_header_store%total + 1
            
            diag_header_store%names(diag_header_store%total) = header_name
            diag_header_store%types(diag_header_store%total) = NLAYER_BYTE
            diag_header_store%vectored(diag_header_store%total) = .TRUE.
            
            ! We just need to add one entry...
            call nc_diag_header_resize_byte(size(header_value), .TRUE.)
            
            endpos = diag_header_store%acount(1)
            
            ! Add the size of the array in:
            diag_header_store%h_byte_vi(diag_header_store%acount(6+1)) = value_size
            
            ! Now add the actual entry!
            diag_header_store%h_byte(endpos - size(header_value):endpos) = header_value
        end subroutine nc_diag_header_byte_v
        
        ! nc_diag_header - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_header_short_v(header_name, header_value)
            character(len=*), intent(in)               :: header_name
            integer(i_short), dimension(:), intent(in) :: header_value
            
            integer(i_long)                            :: endpos
            integer(i_long)                            :: value_size
            
            value_size = size(header_value)
            
            call nc_diag_header_expand
            
            diag_header_store%total = diag_header_store%total + 1
            
            diag_header_store%names(diag_header_store%total) = header_name
            diag_header_store%types(diag_header_store%total) = NLAYER_SHORT
            diag_header_store%vectored(diag_header_store%total) = .TRUE.
            
            ! We just need to add one entry...
            call nc_diag_header_resize_short(size(header_value), .TRUE.)
            
            endpos = diag_header_store%acount(2)
            
            ! Add the size of the array in:
            diag_header_store%h_short_vi(diag_header_store%acount(6+2)) = value_size
            
            ! Now add the actual entry!
            diag_header_store%h_short(endpos - size(header_value):endpos) = header_value
        end subroutine nc_diag_header_short_v
        
        ! nc_diag_header - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_header_long_v(header_name, header_value)
            character(len=*), intent(in)               :: header_name
            integer(i_long), dimension(:), intent(in)  :: header_value
            
            integer(i_long)                            :: endpos
            integer(i_long)                            :: value_size
            
            value_size = size(header_value)
            
            call nc_diag_header_expand
            
            diag_header_store%total = diag_header_store%total + 1
            
            diag_header_store%names(diag_header_store%total) = header_name
            diag_header_store%types(diag_header_store%total) = NLAYER_LONG
            diag_header_store%vectored(diag_header_store%total) = .TRUE.
            
            ! We just need to add one entry...
            call nc_diag_header_resize_long(size(header_value), .TRUE.)
            
            endpos = diag_header_store%acount(3)
            
            ! Add the size of the array in:
            diag_header_store%h_long_vi(diag_header_store%acount(6+3)) = value_size
            
            ! Now add the actual entry!
            diag_header_store%h_long(endpos - size(header_value):endpos) = header_value
        end subroutine nc_diag_header_long_v
        
        ! nc_diag_header - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_header_rsingle_v(header_name, header_value)
            character(len=*), intent(in)               :: header_name
            real(r_single), dimension(:), intent(in)   :: header_value
            
            integer(i_long)                            :: endpos
            integer(i_long)                            :: value_size
            
            value_size = size(header_value)
            
            call nc_diag_header_expand
            
            diag_header_store%total = diag_header_store%total + 1
            
            diag_header_store%names(diag_header_store%total) = header_name
            diag_header_store%types(diag_header_store%total) = NLAYER_FLOAT
            diag_header_store%vectored(diag_header_store%total) = .TRUE.
            
            ! We just need to add one entry...
            call nc_diag_header_resize_rsingle(size(header_value), .TRUE.)
            
            endpos = diag_header_store%acount(4)
            
            ! Add the size of the array in:
            diag_header_store%h_rsingle_vi(diag_header_store%acount(6+4)) = value_size
            
            ! Now add the actual entry!
            diag_header_store%h_rsingle(endpos - size(header_value):endpos) = header_value
        end subroutine nc_diag_header_rsingle_v
        
        ! nc_diag_header - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_header_rdouble_v(header_name, header_value)
            character(len=*), intent(in)               :: header_name
            real(r_double), dimension(:), intent(in)   :: header_value
            
            integer(i_long)                            :: endpos
            integer(i_long)                            :: value_size
            
            value_size = size(header_value)
            
            call nc_diag_header_expand
            
            diag_header_store%total = diag_header_store%total + 1
            
            diag_header_store%names(diag_header_store%total) = header_name
            diag_header_store%types(diag_header_store%total) = NLAYER_DOUBLE
            diag_header_store%vectored(diag_header_store%total) = .TRUE.
            
            ! We just need to add one entry...
            call nc_diag_header_resize_rdouble(value_size, .TRUE.)
            
            endpos = diag_header_store%acount(5)
            
            ! Add the size of the array in:
            diag_header_store%h_rdouble_vi(diag_header_store%acount(6+5)) = value_size
            
            ! Now add the actual entry!
            diag_header_store%h_rdouble(endpos - value_size:endpos) = header_value
        end subroutine nc_diag_header_rdouble_v

