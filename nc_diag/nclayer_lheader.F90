module nclayer_lheader
    use kinds
    use nclayer_state
    use nclayer_climsg
    use netcdf
    
    implicit none
    
    !===============================================================
    ! nc_diag_header - header handling (declaration)
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
    ! nc_diag_header stores header data as NetCDF4 global
    ! attributes. The nc_diag_header subroutines temporarily cache
    ! any header data until write, where it will be set by
    ! NF90_PUT_ATT().
    !---------------------------------------------------------------
    ! This file provides the interface wrapper for the specific
    ! subroutines.
    
    interface nc_diag_header
        module procedure nc_diag_header_byte, &
            nc_diag_header_short, nc_diag_header_long, &
            nc_diag_header_rsingle, nc_diag_header_rdouble, &
            nc_diag_header_string, nc_diag_header_byte_v, &
            nc_diag_header_short_v, nc_diag_header_long_v, &
            nc_diag_header_rsingle_v, nc_diag_header_rdouble_v
    end interface nc_diag_header
    
    contains
        !===============================================================
        ! nc_diag_header - header handling (implementation)
        !===============================================================
        ! DO NOT COMPILE THIS DIRECTLY! THIS IS MEANT TO BE INCLUDED
        ! INSIDE A LARGER F90 SOURCE!
        ! If you compile this directly, you WILL face the WRATH of your
        ! compiler!
        !---------------------------------------------------------------
        ! Depends on:
        !   netcdf_realloc_header.f90
        !---------------------------------------------------------------
        ! nc_diag_header stores header data as NetCDF4 global
        ! attributes. The nc_diag_header simply wraps NF90_PUT_ATT(),
        ! with NO data caching provided.
        !---------------------------------------------------------------
        ! This file provides the actual header data adding subroutines,
        ! referred to by the interface.
        
        ! nc_diag_header - input integer(i_byte)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_header_byte(header_name, header_value)
            character(len=*), intent(in)    :: header_name
            integer(i_byte), intent(in)     :: header_value
            
            call check(nf90_put_att(ncid, NF90_GLOBAL, header_name, header_value))
        end subroutine nc_diag_header_byte
        
        ! nc_diag_header - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_header_short(header_name, header_value)
            character(len=*), intent(in)    :: header_name
            integer(i_short), intent(in)    :: header_value
            
            call check(nf90_put_att(ncid, NF90_GLOBAL, header_name, header_value))
        end subroutine nc_diag_header_short
        
        ! nc_diag_header - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_header_long(header_name, header_value)
            character(len=*), intent(in)    :: header_name
            integer(i_long), intent(in)     :: header_value
            
            call check(nf90_put_att(ncid, NF90_GLOBAL, header_name, header_value))
        end subroutine nc_diag_header_long
        
        ! nc_diag_header - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_header_rsingle(header_name, header_value)
            character(len=*), intent(in)    :: header_name
            real(r_single), intent(in)      :: header_value
            
            call check(nf90_put_att(ncid, NF90_GLOBAL, header_name, header_value))
        end subroutine nc_diag_header_rsingle
        
        ! nc_diag_header - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_header_rdouble(header_name, header_value)
            character(len=*), intent(in)    :: header_name
            real(r_double), intent(in)      :: header_value
            
            call check(nf90_put_att(ncid, NF90_GLOBAL, header_name, header_value))
        end subroutine nc_diag_header_rdouble

        ! nc_diag_header - input character(len=*)
        ! Corresponding NetCDF4 type: string? char?
        subroutine nc_diag_header_string(header_name, header_value)
            character(len=*), intent(in)    :: header_name
            character(len=*), intent(in)    :: header_value
            
            ! Note: using F95 trim here!
            call check(nf90_put_att(ncid, NF90_GLOBAL, header_name, trim(header_value)))
        end subroutine nc_diag_header_string
        
        !=============================================================
        ! VECTOR TYPES
        !=============================================================
        
        ! nc_diag_header - input integer(i_byte), dimension(:)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_header_byte_v(header_name, header_value)
            character(len=*), intent(in)               :: header_name
            integer(i_byte), dimension(:), intent(in)  :: header_value
            
            call check(nf90_put_att(ncid, NF90_GLOBAL, header_name, header_value))
        end subroutine nc_diag_header_byte_v
        
        ! nc_diag_header - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_header_short_v(header_name, header_value)
            character(len=*), intent(in)               :: header_name
            integer(i_short), dimension(:), intent(in) :: header_value
            
            call check(nf90_put_att(ncid, NF90_GLOBAL, header_name, header_value))
        end subroutine nc_diag_header_short_v
        
        ! nc_diag_header - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_header_long_v(header_name, header_value)
            character(len=*), intent(in)               :: header_name
            integer(i_long), dimension(:), intent(in)  :: header_value
            
            call check(nf90_put_att(ncid, NF90_GLOBAL, header_name, header_value))
        end subroutine nc_diag_header_long_v
        
        ! nc_diag_header - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_header_rsingle_v(header_name, header_value)
            character(len=*), intent(in)               :: header_name
            real(r_single), dimension(:), intent(in)   :: header_value
            
            call check(nf90_put_att(ncid, NF90_GLOBAL, header_name, header_value))
        end subroutine nc_diag_header_rsingle_v
        
        ! nc_diag_header - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_header_rdouble_v(header_name, header_value)
            character(len=*), intent(in)               :: header_name
            real(r_double), dimension(:), intent(in)   :: header_value
            
            call check(nf90_put_att(ncid, NF90_GLOBAL, header_name, header_value))
        end subroutine nc_diag_header_rdouble_v
        
        ! String array not available with NF90 attributes
        
        ! nc_diag_header - input character(len=1000)
        ! Corresponding NetCDF4 type: string
        !subroutine nc_diag_header_string_v(header_name, header_value)
        !    character(len=*),    intent(in)               :: header_name
        !    character(len=1000), dimension(:), intent(in) :: header_value
        !    
        !    integer(i_long)                               :: endpos
        !    integer(i_long)                               :: value_size
        !    
        !    value_size = size(header_value)
        !    
        !    call nc_diag_header_expand
        !    
        !    diag_header_store%total = diag_header_store%total + 1
        !    
        !    diag_header_store%names(diag_header_store%total) = header_name
        !    diag_header_store%types(diag_header_store%total) = NLAYER_STRING
        !    diag_header_store%vectored(diag_header_store%total) = .TRUE.
        !    
        !    ! Resize to add value_size
        !    call nc_diag_header_resize_string(value_size, .TRUE.)
        !    
        !    endpos = diag_header_store%acount(6)
        !    
        !    ! Add the size of the array in:
        !    diag_header_store%h_string_vi(diag_header_store%acount(6+6)) = value_size
        !    
        !    ! Now add the actual entry!
        !    diag_header_store%h_string(endpos - value_size:endpos) = header_value
        !end subroutine nc_diag_header_string_v
end module nclayer_lheader
