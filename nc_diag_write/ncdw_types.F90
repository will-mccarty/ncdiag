module ncdw_types
    use kinds, only: i_byte, i_short, i_long, i_llong, &
        r_single, r_double
    use netcdf, only: NF90_FILL_BYTE, NF90_FILL_SHORT, NF90_FILL_INT, &
        NF90_FILL_FLOAT, NF90_FILL_DOUBLE, NF90_FILL_CHAR
    
    implicit none
    
    ! NetCDF4 type struct constants
    integer(i_byte), parameter              :: NLAYER_BYTE   = 1
    integer(i_byte), parameter              :: NLAYER_SHORT  = 2
    integer(i_byte), parameter              :: NLAYER_LONG   = 3
    integer(i_byte), parameter              :: NLAYER_FLOAT  = 4
    integer(i_byte), parameter              :: NLAYER_DOUBLE = 5
    integer(i_byte), parameter              :: NLAYER_STRING = 6
    
    ! Default number of starting entries
    integer(i_short), parameter             :: NLAYER_DEFAULT_ENT = 1024
    
    ! NetCDF zlib (/gzip) compression level
    integer(i_byte), parameter              :: NLAYER_COMPRESSION = 9
    
    ! NetCDF chunking size
    integer(i_long), parameter              :: NLAYER_CHUNKING = 16384
    
    ! Base used when exponentiated.
    integer(i_long), parameter              :: NLAYER_MULTI_BASE = 2
    
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
    
    type diag_chaninfo
        ! Number of channels to store
        integer(i_long)                               :: nchans = -1
        integer(i_long)                               :: nchans_dimid
        
        ! # of times we needed to realloc chaninfo
        ! also the multiplier factor for allocation (2^x)
        integer(i_byte)                               :: alloc_multi
        
        ! Did we write anything out yet?
        logical                                       :: def_lock
        logical                                       :: data_lock
        
        ! Strict checking for bounds?
        ! (Making sure that the sizes are consistent!)
        logical                                       :: strict_check
        
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
        integer(i_long),     dimension(:),allocatable :: var_rel_pos
        
        ! Current variable usage (which, for each element,
        ! should be <= nchans)
        integer(i_long),     dimension(:),allocatable :: var_usage
        
        ! Variable IDs (for use with NetCDF API)
        integer(i_long),     dimension(:),allocatable :: var_ids
        
        ! Maximum string length - only used when the variable
        ! definitions are locked.
        integer(i_long),    dimension(:), allocatable :: max_str_lens
        
        ! Relative indexes - for buffer flushing, keep track of the
        ! relative indexes when we flush data. We store the
        ! variable count here so that we can reference it when we
        ! reset our variable counter to zero, allowing us to reuse
        ! memory while still preserving data order!
        integer(i_long),    dimension(:), allocatable :: rel_indexes
        
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
        integer(i_long),     dimension(6)             :: acount_v
        
        ! Total variables stored
        integer(i_long)                               :: total = 0
        
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
    
    !===============================================================
    ! nc_diag_metadata - metadata handling (declaration)
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
    ! nc_diag_metadata stores metadata data as NetCDF4 global
    ! attributes. The nc_diag_metadata subroutines temporarily cache
    ! any metadata data until write, where it will be set by
    ! NF90_PUT_ATT().
    !---------------------------------------------------------------
    ! This file provides the interface wrapper for the specific
    ! subroutines.
    
    ! diag_metadata struct
    !   This is a super type to store information for the diag metadata,
    !   to be stored in the NetCDF4 file.
    !   
    !   Storage works as follows:
    !     = Add elements to the metadata structure through the subroutine
    !       nc_diag_metadata().
    !         -> The element name is first added to the names variable
    !            within diag_metadata. Allocation (and/or reallocation)
    !            occurs if necessary.
    !         -> The type of the element is stored into the types
    !            variable within diag_metadata, using the constants
    !            available above. Allocation (and/or reallocation)
    !            occurs if necessary.
    !         -> If the type of the element is a vector, the vectored
    !            logical is set to true. Otherwise, it's left as false.
    !         -> If the type of the element is a vector, the
    !            corresponding index vector is set to the number of
    !            elements in the vector.
    !         -> Then the array size and count are validated for the
    !            specific type. Allocation (and/or reallocation) for the
    !            specific type array occurs if necessary.
    !         -> Once ready, we add the actual data into diag_metadata.
    !            If the type of the element is a vector, we simply
    !            append the elements to the vector, since we can now
    !            keep track.
    !         -> Finally, we increment any counters as necessary, and
    !            we call it a day!
    !     = When everything is done, nc_diag_write() is called. This
    !       will trigger nc_diag_metadata_write(), which will do the 
    !       following:
    !         -> Fetch the total number of attributes.
    !         -> Iterate through the names, types, and logical vectored
    !            variables, using the total number of attributes.
    !         -> Based on the types and logical vectored variable,
    !            fetch the actual data from the right spot.
    !         -> Write said data using the NetCDF4 subroutines.
    !         -> Increment our own counters to keep track of how much
    !            we read, especially for the individual variables and
    !            types.
    !         -> Not as tedious as queueing the data!
    !   
    !   Variables:
    !     names    - names of metadata information (attributes) to store.
    !                This is a 1-D array of names - dimensions based on
    !                the number of attributes stored.
    !     types    - types (specified as an integer constants located
    !                above) for each attribute. This is a 1-D array of
    !                integers - dimensions based on the number of
    !                attributes stored.
    !     vectored - whether the attribute stores a 1D vector or not.
    !                This is a 1-D array of integers - dimensions based
    !                on the number of attributes stored.
    !     total    - the total number of attributes in diag_metadata
    !     asize    - array size for each type. This is a 1-D array of
    !                integers - dimensions based on the number of TYPES
    !                available, including vectored types. In this case,
    !                we have 6 single types, plus 5 "hidden" vectored
    !                types (via m_***_vi), so the dimension is 11.
    !     acount   - array count for each type - this is the number of
    !                elements already stored for each type, including
    !                vectored types. The dimensions are the same as
    !                asize - in this case, it's 11.
    !     m_***    - data storage variables, single element array,
    !                dimensions based on the number and type of
    !                attributes stored. If I store one short, one float,
    !                and one double from scratch, then m_short, m_float,
    !                and m_double will have a length of 1 each. The rest
    !                of the m_*** will be empty.
    !     m_***_vi - length index storage for vectored data, dimensions
    !                based on the number and type of vectored attributes
    !                stored. If I store one short vector, one float
    !                vector, and one double vector from scratch, then
    !                m_short_vi, m_float_vi, and m_double_vi will have
    !                a length of 1 vector each. The rest of the m_***_vi
    !                will be empty. These are only populated when a
    !                vector of data is added.
    type diag_md_iarr
        integer(i_long),    dimension(:), allocatable :: index_arr
        integer(i_long)                               :: icount
        integer(i_long)                               :: isize
    end type diag_md_iarr
    
    type diag_metadata
        character(len=100), dimension(:), allocatable :: names
        integer(i_byte),    dimension(:), allocatable :: types
        type(diag_md_iarr), dimension(:), allocatable :: stor_i_arr
        integer(i_byte),    dimension(:), allocatable :: alloc_sia_multi
        
        ! Maximum string length - only used when the variable
        ! definitions are locked.
        integer(i_long),    dimension(:), allocatable :: max_str_lens
        
        ! Relative indexes - for buffer flushing, keep track of the
        ! relative indexes when we flush data. We store the
        ! variable count here so that we can reference it when we
        ! reset our variable counter to zero, allowing us to reuse
        ! memory while still preserving data order!
        integer(i_long),    dimension(:), allocatable :: rel_indexes
        
        ! Total variables
        integer(i_long)                               :: total = 0
        integer(i_long)                               :: prealloc_total = 0
        
        ! Array sizes
        integer(i_long),  dimension(6)                :: asize
        integer(i_long),  dimension(6)                :: acount
        
        ! # of times we needed to realloc simple metadata
        ! also the multiplier factor for allocation (2^x)
        integer(i_byte)                               :: alloc_s_multi
        
        ! # of times we needed to realloc metadata data storage
        ! also the multiplier factor for allocation (2^x)
        integer(i_byte),  dimension(6)                :: alloc_m_multi
        
        ! # of times we needed to realloc metadata INDEX data storage
        ! also the multiplier factor for allocation (2^x)
        integer(i_byte),  dimension(6)                :: alloc_mi_multi
        
        ! Did we write anything out yet?
        logical                                       :: def_lock
        logical                                       :: data_lock
        
        ! Strict checking for bounds?
        ! (Making sure that the sizes are consistent!)
        logical                                       :: strict_check
        
        integer(i_byte),     dimension(:),allocatable :: m_byte
        integer(i_short),    dimension(:),allocatable :: m_short
        integer(i_long),     dimension(:),allocatable :: m_long
        real(r_single),      dimension(:),allocatable :: m_rsingle
        real(r_double),      dimension(:),allocatable :: m_rdouble
        character(len=1000), dimension(:),allocatable :: m_string
        
        integer(i_long),    dimension(:), allocatable :: var_ids
    end type diag_metadata
    
    ! diag_data2d struct
    !   This is a super type to store information for the diag data2d,
    !   to be stored in the NetCDF4 file.
    !   
    !   Storage works as follows:
    !     = Add elements to the data2d structure through the subroutine
    !       nc_diag_data2d().
    !         -> The element name is first added to the names variable
    !            within diag_data2d. Allocation (and/or reallocation)
    !            occurs if necessary.
    !         -> The type of the element is stored into the types
    !            variable within diag_data2d, using the constants
    !            available above. Allocation (and/or reallocation)
    !            occurs if necessary.
    !         -> If the type of the element is a vector, the vectored
    !            logical is set to true. Otherwise, it's left as false.
    !         -> If the type of the element is a vector, the
    !            corresponding index vector is set to the number of
    !            elements in the vector.
    !         -> Then the array size and count are validated for the
    !            specific type. Allocation (and/or reallocation) for the
    !            specific type array occurs if necessary.
    !         -> Once ready, we add the actual data into diag_data2d.
    !            If the type of the element is a vector, we simply
    !            append the elements to the vector, since we can now
    !            keep track.
    !         -> Finally, we increment any counters as necessary, and
    !            we call it a day!
    !     = When everything is done, nc_diag_write() is called. This
    !       will trigger nc_diag_data2d_write(), which will do the 
    !       following:
    !         -> Fetch the total number of attributes.
    !         -> Iterate through the names, types, and logical vectored
    !            variables, using the total number of attributes.
    !         -> Based on the types and logical vectored variable,
    !            fetch the actual data from the right spot.
    !         -> Write said data using the NetCDF4 subroutines.
    !         -> Increment our own counters to keep track of how much
    !            we read, especially for the individual variables and
    !            types.
    !         -> Not as tedious as queueing the data!
    !   
    !   Variables:
    !     names    - names of data2d information (attributes) to store.
    !                This is a 1-D array of names - dimensions based on
    !                the number of attributes stored.
    !     types    - types (specified as an integer constants located
    !                above) for each attribute. This is a 1-D array of
    !                integers - dimensions based on the number of
    !                attributes stored.
    !     vectored - whether the attribute stores a 1D vector or not.
    !                This is a 1-D array of integers - dimensions based
    !                on the number of attributes stored.
    !     total    - the total number of attributes in diag_data2d
    !     asize    - array size for each type. This is a 1-D array of
    !                integers - dimensions based on the number of TYPES
    !                available, including vectored types. In this case,
    !                we have 6 single types, plus 5 "hidden" vectored
    !                types (via m_***_vi), so the dimension is 11.
    !     acount   - array count for each type - this is the number of
    !                elements already stored for each type, including
    !                vectored types. The dimensions are the same as
    !                asize - in this case, it's 11.
    !     m_***    - data storage variables, single element array,
    !                dimensions based on the number and type of
    !                attributes stored. If I store one short, one float,
    !                and one double from scratch, then m_short, m_float,
    !                and m_double will have a length of 1 each. The rest
    !                of the m_*** will be empty.
    !     m_***_vi - length index storage for vectored data, dimensions
    !                based on the number and type of vectored attributes
    !                stored. If I store one short vector, one float
    !                vector, and one double vector from scratch, then
    !                m_short_vi, m_float_vi, and m_double_vi will have
    !                a length of 1 vector each. The rest of the m_***_vi
    !                will be empty. These are only populated when a
    !                vector of data is added.
    type diag_d2d_iarr
        integer(i_long),    dimension(:), allocatable :: index_arr
        integer(i_long),    dimension(:), allocatable :: length_arr
        integer(i_long)                               :: icount
        integer(i_long)                               :: isize
    end type diag_d2d_iarr
    
    type diag_data2d
        character(len=100), dimension(:), allocatable :: names
        integer(i_byte),    dimension(:), allocatable :: types
        type(diag_d2d_iarr),dimension(:), allocatable :: stor_i_arr
        integer(i_byte),    dimension(:), allocatable :: alloc_sia_multi
        
        ! Maximum string length - only used when the variable
        ! definitions are locked.
        integer(i_long),    dimension(:), allocatable :: max_str_lens
        
        ! Maximum variable length - only used when the variable
        ! definitions are locked.
        integer(i_long),    dimension(:), allocatable :: max_lens
        
        ! Relative indexes - for buffer flushing, keep track of the
        ! relative indexes when we flush data. We store the
        ! variable count here so that we can reference it when we
        ! reset our variable counter to zero, allowing us to reuse
        ! memory while still preserving data order!
        integer(i_long),    dimension(:), allocatable :: rel_indexes
        
        ! Total variables
        integer(i_long)                               :: total = 0
        integer(i_long)                               :: prealloc_total = 0
        
        ! Array sizes
        integer(i_long),  dimension(6)                :: asize
        integer(i_long),  dimension(6)                :: acount
        
        ! # of times we needed to realloc simple data2d
        ! also the multiplier factor for allocation (2^x)
        integer(i_byte)                               :: alloc_s_multi
        
        ! # of times we needed to realloc data2d data storage
        ! also the multiplier factor for allocation (2^x)
        integer(i_byte),  dimension(6)                :: alloc_m_multi
        
        ! # of times we needed to realloc data2d INDEX data storage
        ! also the multiplier factor for allocation (2^x)
        integer(i_byte),  dimension(6)                :: alloc_mi_multi
        
        ! Did we write anything out yet?
        logical                                       :: def_lock
        logical                                       :: data_lock
        
        ! Strict checking for bounds?
        ! (Making sure that the sizes are consistent!)
        logical                                       :: strict_check
        
        integer(i_byte),     dimension(:),allocatable :: m_byte
        integer(i_short),    dimension(:),allocatable :: m_short
        integer(i_long),     dimension(:),allocatable :: m_long
        real(r_single),      dimension(:),allocatable :: m_rsingle
        real(r_double),      dimension(:),allocatable :: m_rdouble
        character(len=1000), dimension(:),allocatable :: m_string
        
        integer(i_long),    dimension(:), allocatable :: var_dim_ids
        integer(i_long),    dimension(:), allocatable :: var_ids
    end type diag_data2d
    
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
        
        ! Global nobs dimension ID
        integer(i_long)                               :: nobs_dim_id = -1
    end type diag_varattr
end module ncdw_types
