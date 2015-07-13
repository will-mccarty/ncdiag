        !===============================================================
        ! nc_diag_varattr - varattr handling (declaration)
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
        ! This file provides the interface wrapper for the specific
        ! subroutines.
        
        ! Variable type - this stores and handles all of the variables,
        ! and includes the variable storage type.
        type diag_varattr
            character(len=100), dimension(:), allocatable :: names
            integer(i_byte),    dimension(:), allocatable :: types
            integer(i_long),    dimension(:), allocatable :: var_ids
            
            integer(i_llong)                              :: total
        end type diag_varattr
        
        type(diag_varattr), allocatable :: diag_varattr_store
        
        interface nc_diag_varattr
            module procedure nc_diag_varattr_byte, &
                nc_diag_varattr_short, nc_diag_varattr_long, &
                nc_diag_varattr_rsingle, nc_diag_varattr_rdouble, &
                nc_diag_varattr_string, &
                nc_diag_varattr_byte_v, nc_diag_varattr_short_v, &
                nc_diag_varattr_long_v, nc_diag_varattr_rsingle_v, &
                nc_diag_varattr_rdouble_v
        end interface nc_diag_varattr
        
#ifdef NO_NETCDF
        integer(i_byte), parameter :: NLAYER_FILL_BYTE   = -254
        integer(i_short),parameter :: NLAYER_FILL_SHORT  = -254
        integer(i_long), parameter :: NLAYER_FILL_LONG   = -254
        real(r_single),  parameter :: NLAYER_FILL_FLOAT  = -254.567
        real(r_double),  parameter :: NLAYER_FILL_DOUBLE = -254.56789
        character,       parameter :: NLAYER_FILL_CHAR   = CHAR(0)
#else
        integer(i_byte), parameter :: NLAYER_FILL_BYTE   = NF90_FILL_BYTE
        integer(i_short),parameter :: NLAYER_FILL_SHORT  = NF90_FILL_SHORT
        integer(i_long), parameter :: NLAYER_FILL_LONG   = NF90_FILL_INT
        real(r_single),  parameter :: NLAYER_FILL_FLOAT  = NF90_FILL_FLOAT
        real(r_double),  parameter :: NLAYER_FILL_DOUBLE = NF90_FILL_DOUBLE
        character,       parameter :: NLAYER_FILL_CHAR   = NF90_FILL_CHAR
#endif
