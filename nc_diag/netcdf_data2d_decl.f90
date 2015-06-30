        !===============================================================
        ! nc_diag_data2d - data2d handling (declaration)
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
        ! nc_diag_data2d stores data2d data2d as NetCDF4 global
        ! attributes. The nc_diag_data2d subroutines temporarily cache
        ! any data2d data2d until write, where it will be set by
        ! NF90_PUT_ATT().
        !---------------------------------------------------------------
        ! This file provides the interface wrapper for the specific
        ! subroutines.
        
        ! Actual data2d storage type
        type diag_d_storage
            ! Recursive storage for multidimensional data2d.
            ! This used to be dimension(:), allocatable, but ifort
            ! (at least the version used currently) doesn't support
            ! allocated derived type matryoshkas yet!
            ! So we're using pointers instead and allocating that.
            ! NOTE - "=> null()" is Fortran 95!
            type(diag_d_storage),dimension(:), pointer    :: storage => null()
            
            ! Type of data2d represented. This is a NCLAYER constant.
            integer(i_byte)                               :: data_type
            
            ! Boolean to indicate whether this has actual data2d or is
            ! just another array of this type, leading to the data2d.
            logical                                       :: is_data
            logical                                       :: filled
            
            ! Type data2d storage - since this is just for a single
            ! variable, only one of these will actually be used.
            integer(i_byte),     dimension(:),allocatable :: byte
            integer(i_short),    dimension(:),allocatable :: short
            integer(i_long),     dimension(:),allocatable :: long
            real(r_single),      dimension(:),allocatable :: rsingle
            real(r_double),      dimension(:),allocatable :: rdouble
            character(len=1000), dimension(:),allocatable :: string
            
            ! data2d array size - again, since we're only using one,
            ! this will represent one of the type data2d storage variables
            ! above.
            integer(i_long)                               :: asize
            integer(i_long)                               :: acount
        end type diag_d_storage
        
        ! Variable type - this stores and handles all of the variables,
        ! and includes the variable storage type.
        type diag_data2d
            character(len=100), dimension(:), allocatable :: names
            integer(i_byte),    dimension(:), allocatable :: types
            type(diag_d_storage),dimension(:),allocatable :: stores
            integer(i_long),    dimension(:), allocatable :: dims
            
            ! Total variables
            integer(i_long)                               :: total
            
            ! Did we write anything out yet?
            logical                                       :: def_lock
            logical                                       :: data_lock
            
            integer(i_long)                               :: nobs_dim_id
            integer(i_long),    dimension(:), allocatable :: var_dim_ids
            integer(i_long),    dimension(:), allocatable :: var_ids
        end type diag_data2d
        
        type(diag_data2d), allocatable :: diag_data2d_store
        
        interface nc_diag_data2d
            module procedure nc_diag_data2d_byte, &
                nc_diag_data2d_short, nc_diag_data2d_long, &
                nc_diag_data2d_rsingle, nc_diag_data2d_rdouble, &
                nc_diag_data2d_string
        end interface nc_diag_data2d
