    type data_blob
        integer(i_byte),    dimension(:), allocatable :: byte_buffer
        integer(i_short),   dimension(:), allocatable :: short_buffer
        integer(i_long),    dimension(:), allocatable :: long_buffer
        
        real(r_single),     dimension(:), allocatable :: rsingle_buffer
        real(r_double),     dimension(:), allocatable :: rdouble_buffer
        
        character(1)     ,dimension(:,:), allocatable :: string_buffer
        
        integer(i_byte),  dimension(:,:), allocatable :: byte_2d_buffer
        integer(i_short), dimension(:,:), allocatable :: short_2d_buffer
        integer(i_long),  dimension(:,:), allocatable :: long_2d_buffer
        
        real(r_single),   dimension(:,:), allocatable :: rsingle_2d_buffer
        real(r_double),   dimension(:,:), allocatable :: rdouble_2d_buffer
        
        character(1),   dimension(:,:,:), allocatable :: string_2d_buffer
        
        integer(i_long)                               :: cur_pos = 1
        integer(i_long),  dimension(3)                :: alloc_size
    end type data_blob
    
    ! Data blob stores entire variable's data!
    ! Indexing uses the metadata indexing system.
    type(data_blob),    dimension(:), allocatable     :: data_blobs
    
