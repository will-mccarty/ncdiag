        ! ---------------------------
        ! nc_diag_realloc definitions
        ! ---------------------------
        ! DO NOT COMPILE THIS DIRECTLY! THIS IS MEANT TO BE INCLUDED
        ! INSIDE A LARGER F90 SOURCE!
        ! If you compile this directly, you WILL face the WRATH of your
        ! compiler!
        
        ! nc_diag_realloc subroutines provide reallocation functionality
        ! for various inputs.
        
        ! This file provides the interface wrapper for the specific
        ! subroutines.
        
        interface nc_diag_realloc
            module procedure nc_diag_realloc_byte, &
                nc_diag_realloc_short, nc_diag_realloc_long, &
                nc_diag_realloc_rsingle, nc_diag_realloc_rdouble, &
                nc_diag_realloc_string, nc_diag_realloc_logical
        end interface nc_diag_realloc
