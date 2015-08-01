module ncdr_state
    use kinds
    use ncdr_types
    implicit none
    
    integer(i_long) :: current_ncid = -1, current_ind = -1
    integer(i_long), dimension(:), allocatable :: ncid_stack, ind_stack
    integer(i_long) :: ncid_stack_size = 0, ncid_stack_count = 0
    logical :: init_done = .FALSE.
    
    character(len=200) :: cur_nc_file
    
    type(ncdr_file), dimension(:), allocatable :: ncdr_files
    integer(i_long)                            :: ncdr_file_count = 0
    integer(i_long)                            :: ncdr_file_total = 0
    integer(i_long)                            :: ncdr_file_highest = 0
    
    ! Default number of starting entries
    integer(i_short), parameter             :: NCDR_DEFAULT_ENT = 1024
    
    ! NetCDF chunking size
    integer(i_long), parameter              :: NCDR_CHUNKING = 16384
end module ncdr_state
