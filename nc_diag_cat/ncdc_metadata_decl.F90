    
    integer(i_long)                            :: input_ndims, cached_ndims = -1
    integer(i_long)                            :: input_nvars, cached_nvars = -1
    integer(i_long)                            :: input_nattrs
    
    integer(i_long)                            :: tmp_dim_index
    
    character(len=NF90_MAX_NAME)               :: tmp_dim_name
    integer(i_long)                            :: tmp_dim_size
    
    character(len=NF90_MAX_NAME)               :: tmp_var_name
    integer(i_long)                            :: tmp_var_type, tmp_var_ndims
    integer(i_long), dimension(:), allocatable :: tmp_var_dimids
    character(len=NF90_MAX_NAME) , allocatable :: tmp_var_dim_names(:)
    
    integer(i_long), dimension(:), allocatable :: tmp_input_dimids, tmp_input_varids
    
    logical                                    :: is_unlim = .FALSE.
    
    integer(i_long), parameter                 :: DIM_START_SIZE = 256
    integer(i_long), parameter                 :: VAR_START_SIZE = 1024
    
    ! Dimension storage
    character(len=100), dimension(:), allocatable     :: dim_names
    integer(i_long),    dimension(:), allocatable     :: dim_sizes
    integer(i_long),    dimension(:), allocatable     :: dim_output_ids
    integer(i_long),    dimension(:), allocatable     :: dim_counters
    integer(i_long),    dimension(:), allocatable     :: dim_unlim_sizes
    
    ! Array storage info for dimension storage
    integer(i_long)                                   :: dim_arr_total = 0
    integer(i_long)                                   :: dim_arr_size = 0
    
    integer(i_long)                                   :: num_unlims, i
    integer(i_long), dimension(:), allocatable        :: unlim_dims
    
    ! dim_sizes(i) of -1 designates an unlimited dimension
    
    ! Variable dimensions storage
    ! See ncdc_realloc for nc_diag_cat_dim_names derived type def
    
    ! Variable storage
    character(len=100),    dimension(:), allocatable  :: var_names
    integer(i_long),       dimension(:), allocatable  :: var_types
    type(nc_diag_cat_dim_names), dimension(:), allocatable  :: var_dim_names
    integer(i_long),       dimension(:), allocatable  :: var_output_ids
    integer(i_long),       dimension(:), allocatable  :: var_counters
    logical,               dimension(:), allocatable  :: var_hasunlim
    
    ! Array storage info for variable storage
    integer(i_long)                                   :: var_arr_total = 0
    integer(i_long)                                   :: var_arr_size = 0
