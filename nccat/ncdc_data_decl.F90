    integer(i_byte),    dimension(:), allocatable     :: byte_buffer
    integer(i_short),   dimension(:), allocatable     :: short_buffer
    integer(i_long),    dimension(:), allocatable     :: long_buffer
    
    real(r_single),     dimension(:), allocatable     :: rsingle_buffer
    real(r_double),     dimension(:), allocatable     :: rdouble_buffer
    
    !character(len=1000),dimension(:), allocatable     :: string_buffer
    character(1)   ,dimension(:,:), allocatable     :: string_buffer
    
    integer(i_byte),  dimension(:,:), allocatable     :: byte_2d_buffer
    integer(i_short), dimension(:,:), allocatable     :: short_2d_buffer
    integer(i_long),  dimension(:,:), allocatable     :: long_2d_buffer
    
    real(r_single),   dimension(:,:), allocatable     :: rsingle_2d_buffer
    real(r_double),   dimension(:,:), allocatable     :: rdouble_2d_buffer
    
    character(1),   dimension(:,:,:), allocatable     :: string_2d_buffer
