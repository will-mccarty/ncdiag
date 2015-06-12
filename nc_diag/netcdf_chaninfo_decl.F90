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
            ! Number of channels to store
            integer(i_long)                               :: nchans = -1
            
            ! # of times we needed to realloc chaninfo
            ! also the multiplier factor for allocation (2^x)
            integer(i_byte)                               :: alloc_multi
            
            !-----------------------------------------------------------
            ! Variable storage
            !-----------------------------------------------------------
            
            ! Name array for each variable
            character(len=100),  dimension(:),allocatable :: names
            
            ! Type constants array for each variable
            integer(i_byte),     dimension(:),allocatable :: types
            
            ! Relative positioning for each variable - relative position
            ! in groups of nchan within each type array. For instance,
            ! if "asdf" has a relative value of 2, type of BYTE, and nchan
            ! of 10, then variable "asdf" will start at 1 + [(2 - 1) * 10] = 11
            ! within the byte storage array. Eqn: 1 + [(REL_VAL - 1) * nchan]
            integer(i_byte),     dimension(:),allocatable :: var_rel_pos
            
            ! Current variable usage (which, for each element,
            ! should be <= nchans)
            integer(i_byte),     dimension(:),allocatable :: var_usage
            
            ! Variable IDs (for use with NetCDF API)
            integer(i_kind),     dimension(:),allocatable :: var_ids
            
            !-----------------------------------------------------------
            ! Type metadata storage
            !-----------------------------------------------------------
            
            ! Type array variable usage count - number of variables
            ! in each type array. For instance, if element 1 (ci_byte) has
            ! a value of 3 here, it means it has 3 variables stored already.
            ! (Hypothetically, if nchan = 10, then it has 30 stored variables.
            ! That means I can start creating vars at 1 + [(4-1) * 10] = 31.)
            !  1  2  3  4  5  6  7  8  9 10
            ! 11 12 13 14 15 16 17 18 19 20
            ! 21 22 23 24 25 26 27 28 29 30
            ! 31
            integer(i_byte),     dimension(6)             :: acount_v
            
            ! Total variables stored
            integer(i_long)                               :: total
            
            ! Array size for each type array
            integer(i_long),     dimension(6)             :: asize
            
            ! Array count for each type array - used with the internal
            ! resizing tool
            integer(i_long),     dimension(6)             :: acount
            
            ! Storage arrays for specific types
            integer(i_byte),     dimension(:),allocatable :: ci_byte
            integer(i_short),    dimension(:),allocatable :: ci_short
            integer(i_long),     dimension(:),allocatable :: ci_long
            real(r_single),      dimension(:),allocatable :: ci_rsingle
            real(r_double),      dimension(:),allocatable :: ci_rdouble
            character(len=1000), dimension(:),allocatable :: ci_string
        end type diag_chaninfo
        
        type(diag_chaninfo), allocatable :: diag_chaninfo_store
        
        interface nc_diag_chaninfo
            module procedure nc_diag_chaninfo_byte, &
                nc_diag_chaninfo_short, nc_diag_chaninfo_long, &
                nc_diag_chaninfo_rsingle, nc_diag_chaninfo_rdouble, &
                nc_diag_chaninfo_string!, nc_diag_chaninfo_byte_v, &
                !nc_diag_chaninfo_short_v, nc_diag_chaninfo_long_v, &
                !nc_diag_chaninfo_rsingle_v, nc_diag_chaninfo_rdouble_v
        end interface nc_diag_chaninfo
