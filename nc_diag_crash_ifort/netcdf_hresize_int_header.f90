        ! ----------------------------------
        ! nc_diag_header_resize_resize definitions
        ! ----------------------------------
        ! DO NOT COMPILE THIS DIRECTLY! THIS IS MEANT TO BE INCLUDED
        ! INSIDE A LARGER F90 SOURCE!
        ! If you compile this directly, you WILL face the WRATH of your
        ! compiler!
        
        ! Depends on: netcdf_realloc_header.f90
        ! Technically, order shouldn't matter... but just in case,
        ! include netcdf_realloc_header.f90 FIRST!
        
        ! nc_diag_header_resize subroutines correspond to the global atributes,
        ! set by NF90_PUT_ATT()
        
        ! This file provides the interface wrapper for the specific
        ! subroutines.
        
        interface nc_diag_header_resize
            module procedure nc_diag_header_resize_byte, &
                nc_diag_header_resize_short, nc_diag_header_resize_long, &
                nc_diag_header_resize_rsingle, nc_diag_header_resize_rdouble, &
                nc_diag_header_resize_string, nc_diag_header_resize_byte_v, &
                nc_diag_header_resize_short_v, nc_diag_header_resize_long_v, &
                nc_diag_header_resize_rsingle_v, nc_diag_header_resize_rdouble_v
        end interface nc_diag_header_resize
