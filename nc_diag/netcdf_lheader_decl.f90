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
        
        ! diag_header struct
        !   This is a super type to store information for the diag header,
        !   to be stored in the NetCDF4 file.
        !   
        !   Storage works as follows:
        !     = Add elements to the header structure through the subroutine
        !       nc_diag_header().
        !         -> The element name is first added to the names variable
        !            within diag_header. Allocation (and/or reallocation)
        !            occurs if necessary.
        !         -> The type of the element is stored into the types
        !            variable within diag_header, using the constants
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
        !         -> Once ready, we add the actual data into diag_header.
        !            If the type of the element is a vector, we simply
        !            append the elements to the vector, since we can now
        !            keep track.
        !         -> Finally, we increment any counters as necessary, and
        !            we call it a day!
        !     = When everything is done, nc_diag_write() is called. This
        !       will trigger nc_diag_header_write(), which will do the 
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
        !     names    - names of header information (attributes) to store.
        !                This is a 1-D array of names - dimensions based on
        !                the number of attributes stored.
        !     types    - types (specified as an integer constants located
        !                above) for each attribute. This is a 1-D array of
        !                integers - dimensions based on the number of
        !                attributes stored.
        !     vectored - whether the attribute stores a 1D vector or not.
        !                This is a 1-D array of integers - dimensions based
        !                on the number of attributes stored.
        !     total    - the total number of attributes in diag_header
        !     asize    - array size for each type. This is a 1-D array of
        !                integers - dimensions based on the number of TYPES
        !                available, including vectored types. In this case,
        !                we have 6 single types, plus 5 "hidden" vectored
        !                types (via h_***_vi), so the dimension is 11.
        !     acount   - array count for each type - this is the number of
        !                elements already stored for each type, including
        !                vectored types. The dimensions are the same as
        !                asize - in this case, it's 11.
        !     h_***    - data storage variables, single element array,
        !                dimensions based on the number and type of
        !                attributes stored. If I store one short, one float,
        !                and one double from scratch, then h_short, h_float,
        !                and h_double will have a length of 1 each. The rest
        !                of the h_*** will be empty.
        !     h_***_vi - length index storage for vectored data, dimensions
        !                based on the number and type of vectored attributes
        !                stored. If I store one short vector, one float
        !                vector, and one double vector from scratch, then
        !                h_short_vi, h_float_vi, and h_double_vi will have
        !                a length of 1 vector each. The rest of the h_***_vi
        !                will be empty. These are only populated when a
        !                vector of data is added.
        type diag_header
            character(len=100), dimension(:), allocatable :: names
            integer(i_byte),  dimension(:),   allocatable :: types
            logical,          dimension(:),   allocatable :: vectored
            integer(i_long)                               :: total
            integer(i_long),  dimension(12)               :: asize
            integer(i_long),  dimension(12)               :: acount
            
            ! # of times we needed to realloc simple metadata
            ! also the multiplier factor for allocation (2^x)
            integer(i_byte)                               :: alloc_s_multi
            
            ! # of times we needed to realloc header data storage
            ! also the multiplier factor for allocation (2^x)
            integer(i_byte),  dimension(6)                :: alloc_h_multi
            
            ! # of times we needed to realloc header INDEX data storage
            ! also the multiplier factor for allocation (2^x)
            integer(i_byte),  dimension(6)                :: alloc_hi_multi
            
            integer(i_byte),     dimension(:),allocatable :: h_byte
            integer(i_short),    dimension(:),allocatable :: h_short
            integer(i_long),     dimension(:),allocatable :: h_long
            real(r_single),      dimension(:),allocatable :: h_rsingle
            real(r_double),      dimension(:),allocatable :: h_rdouble
            character(len=1000), dimension(:),allocatable :: h_string
            
            ! Index information
            integer(i_long),  dimension(:),   allocatable :: h_byte_vi
            integer(i_long),  dimension(:),   allocatable :: h_short_vi
            integer(i_long),  dimension(:),   allocatable :: h_long_vi
            integer(i_long),  dimension(:),   allocatable :: h_rsingle_vi
            integer(i_long),  dimension(:),   allocatable :: h_rdouble_vi
            integer(i_long),  dimension(:),   allocatable :: h_string_vi
        end type diag_header
        
        type(diag_header), allocatable :: diag_header_store
        
        interface nc_diag_header
            module procedure nc_diag_header_byte, &
                nc_diag_header_short, nc_diag_header_long, &
                nc_diag_header_rsingle, nc_diag_header_rdouble, &
                nc_diag_header_string, nc_diag_header_byte_v, &
                nc_diag_header_short_v, nc_diag_header_long_v, &
                nc_diag_header_rsingle_v, nc_diag_header_rdouble_v
        end interface nc_diag_header
