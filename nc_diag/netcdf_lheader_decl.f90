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
