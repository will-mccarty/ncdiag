        !===============================================================
        ! nc_diag_realloc - reallocation support (declaration)
        !===============================================================
        ! DO NOT COMPILE THIS DIRECTLY! THIS IS MEANT TO BE INCLUDED
        ! INSIDE A LARGER F90 SOURCE!
        ! If you compile this directly, you WILL face the WRATH of your
        ! compiler!
        !---------------------------------------------------------------
        ! Depends on: nothing
        !---------------------------------------------------------------
        ! nc_diag_realloc subroutines provide reallocation functionality
        ! for various inputs.
        !---------------------------------------------------------------
        ! This file provides the interface wrapper for the array
        ! reallocation subroutines. This is so that others can simply
        ! call nc_diag_realloc with the necessary arguments, instead of
        ! having to call the specific nc_diag_realloc_* subroutines.
        
        interface nc_diag_realloc
            module procedure nc_diag_realloc_byte, &
                nc_diag_realloc_short, nc_diag_realloc_long, &
                nc_diag_realloc_rsingle, nc_diag_realloc_rdouble, &
                nc_diag_realloc_string, nc_diag_realloc_logical
        end interface nc_diag_realloc
