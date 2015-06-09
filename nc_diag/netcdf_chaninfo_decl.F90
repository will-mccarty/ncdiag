        !===============================================================
        ! nc_diag_chaninfo - channel info handling (declaration)
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
        ! nc_diag_chaninfo stuff stuff
        !---------------------------------------------------------------
        ! This file provides the actual subroutines, referred to by the
        ! interface.
        
        type diag_chaninfo
            integer(i_long)                               :: nchans = -1
            
            ! # of times we needed to realloc chaninfo
            ! also the multiplier factor for allocation (2^x)
            integer(i_byte)                               :: alloc_multi
            
            character(len=100),  dimension(:),allocatable :: names
            integer(i_byte),     dimension(:),allocatable :: types
            integer(i_long)                               :: total
            integer(i_long),     dimension(6)             :: asize
            integer(i_long),     dimension(6)             :: acount
            
            integer(i_byte),     dimension(:),allocatable :: ci_byte
            integer(i_short),    dimension(:),allocatable :: ci_short
            integer(i_long),     dimension(:),allocatable :: ci_long
            real(r_single),      dimension(:),allocatable :: ci_rsingle
            real(r_double),      dimension(:),allocatable :: ci_rdouble
            character(len=1000), dimension(:),allocatable :: ci_string
        end type diag_chaninfo
        
        type(diag_header), allocatable :: diag_chaninfo_store
