module netcdf_layer
    use netcdf
    use fnv32mod
    use kinds
    implicit none
    
    logical,dimension(:),allocatable        :: def_locked
    real(r_single),dimension(:),allocatable :: bifix
    !real(r_single),dimension(:),allocatable    :: bifix
    
    ! diag_header struct constants
    integer(i_byte), parameter              :: NLAYER_BYTE   = 1
    integer(i_byte), parameter              :: NLAYER_SHORT  = 2
    integer(i_byte), parameter              :: NLAYER_LONG   = 3
    integer(i_byte), parameter              :: NLAYER_FLOAT  = 4
    integer(i_byte), parameter              :: NLAYER_DOUBLE = 5
    integer(i_byte), parameter              :: NLAYER_STRING = 6
    
    integer(i_byte), parameter              :: NLAYER_DEFAULT_ENT = 127
    
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
        !character(len=*), dimension(:),   allocatable   :: names
        character(len=100), dimension(:), allocatable :: names
        integer(i_byte),  dimension(:),   allocatable :: types
        logical,          dimension(:),   allocatable :: vectored
        integer(i_long)                               :: total
        !integer(i_long),  dimension(12),  allocatable   :: asize
        !integer(i_long),  dimension(12),  allocatable   :: acount
        integer(i_long),  dimension(11)                :: asize
        integer(i_long),  dimension(11)                :: acount
        
        integer(i_byte),  dimension(:),   allocatable :: h_byte
        integer(i_short), dimension(:),   allocatable :: h_short
        integer(i_long),  dimension(:),   allocatable :: h_long
        real(r_single),   dimension(:),   allocatable :: h_rsingle
        real(r_double),   dimension(:),   allocatable :: h_rdouble
        character(len=1000), dimension(:),allocatable :: h_string
        
        ! Index information
        integer(i_long),  dimension(:),   allocatable :: h_byte_vi
        integer(i_long),  dimension(:),   allocatable :: h_short_vi
        integer(i_long),  dimension(:),   allocatable :: h_long_vi
        integer(i_long),  dimension(:),   allocatable :: h_rsingle_vi
        integer(i_long),  dimension(:),   allocatable :: h_rdouble_vi
    end type diag_header
    
    type(diag_header), allocatable :: diag_header_store
    
    integer :: ncid
    logical :: init_done = .FALSE.
    logical :: header_locked = .FALSE.
    
    include "netcdf_realloc_int_header.f90"
    !include "netcdf_hresize_int_header.f90"
    include "netcdf_layer_int_header.f90"
    
    contains
        include "netcdf_realloc_sub_header.f90"
        include "netcdf_hresize_sub_header.f90"
        include "netcdf_layer_sub_header.f90"
        
        subroutine nc_diag_init(filename)
            character(len=*),intent(in)    :: filename
            print *,'Initializing netcdf layer library...'
            ! nf90_create creates the NetCDF file, and initializes
            ! everything needed to write a NetCDF file.
            ! 
            ! NF90_CLOBBER forces overwriting the file, even if it already
            ! exists.
            ! 
            ! ncid is a special ID that the NetCDF library uses to keep
            ! track of what file you're working on. We're returning that
            ! here.
            if (.NOT. init_done) then
                call check( nf90_create(filename, OR(NF90_NETCDF4, NF90_CLOBBER), ncid) )
                if (allocated(diag_header_store)) then
                    call error("BUG! diag_header_store is allocated, but init_done is set!")
                end if
                allocate(diag_header_store)
                init_done = .TRUE.
            else
                call error("Attempted to initialize without closing previous nc_diag file!")
            end if
        end subroutine nc_diag_init
        
        !subroutine nc_dimension_chaninfo_set
        !    
        !end subroutine nc_dimension_chaninfo_set
        !
        !subroutine nc_diag_chaninfo(chaninfo_name, chaninfo_)
        !    
        !end subroutine nc_diag_chaninfo
        !
        !subroutine nc_diag_metadata
        !    
        !end subroutine nc_diag_metadata
        !
        !subroutine nc_diag_data(data_name, data_index, data)
        !    
        !end subroutine nc_diag_data
        
        subroutine check(status)
          integer, intent ( in) :: status
          
          if(status /= nf90_noerr) then 
            call error(trim(nf90_strerror(status)))
          end if
        end subroutine check
        
        subroutine error(err)
            character(len=*), intent(in) :: err
            write(*, "(A, A)") "ERROR: ", err
            stop "Failed to process data/write NetCDF4."
        end subroutine error
        
        subroutine debug(dbg)
            character(len=*), intent(in) :: dbg
            write(*, "(A, A)") "D: ", dbg
        end subroutine debug
    
end module netcdf_layer
