program test_index
    type data_arr
        integer, dimension(:), allocatable :: int_arr_dummy1
        integer, dimension(:), allocatable :: int_arr_dummy2
        integer, dimension(:), allocatable :: int_arr_dummy3
        integer, dimension(:), allocatable :: int_arr_dummy4
        integer, dimension(:), allocatable :: int_arr_dummy5
        integer, dimension(:), allocatable :: int_arr
        integer                            :: dcount
        integer                            :: dsize
    end type data_arr
    
    type data
        character(len=100), dimension(:), allocatable :: names
        type(data_arr),     dimension(:), allocatable :: stor_data_arr
        
        integer                                       :: total = 0
    end type data
    
    type(data), allocatable    :: data_store
    
    integer, parameter :: INITIAL_SIZE = 1024
    
    integer            :: i
    
    real(8) :: t1,t2
    
    call init_data
    
    call cpu_time(t1)
    do i = 1, 100000
        call add_data("test", i)
        call add_data("test2", i+1)
    end do
    call cpu_time(t2)
    print *, "Data adding time:", real(t2-t1)
        
    call cpu_time(t1)
    call write_data
    call cpu_time(t2)
    print *, "Data writing time:", real(t2-t1)
    
    contains
        subroutine init_data
            allocate(data_store)
        end subroutine init_data
        
        subroutine expand_fields
            if (allocated(data_store%names)) then
                if (data_store%total >= size(data_store%names)) &
                    call nc_diag_realloc_string(data_store%names, size(data_store%names))
            else
                allocate(data_store%names(INITIAL_SIZE))
            end if
            
            if (allocated(data_store%stor_data_arr)) then
                if (data_store%total >= size(data_store%stor_data_arr)) &
                    call nc_diag_realloc_darr(size(data_store%stor_data_arr))
            else
                allocate(data_store%stor_data_arr(INITIAL_SIZE))
            end if
        end subroutine expand_fields
        
        subroutine add_data(var_name, var_value)
            character(len=*)   :: var_name
            integer            :: var_value
            
            integer            :: var_index
            
            var_index = lookup_var(var_name)
            
            if (var_index == -1) then
                data_store%total = data_store%total + 1
                call expand_fields
                data_store%names(data_store%total) = var_name
                var_index = data_store%total
            end if
            
            call nc_diag_resize_darr(var_index, 1)
            
            data_store%stor_data_arr(var_index)%int_arr(data_store%stor_data_arr(var_index)%dcount) = var_value
        end subroutine add_data
        
        subroutine write_data
            integer :: i, j
            integer, dimension(:), allocatable :: tmp
            
            open(unit=4, FILE='indvarr.out', form='unformatted', access='stream')

            do i = 1, data_store%total
                allocate(tmp(data_store%stor_data_arr(i)%dcount))
                tmp = data_store%stor_data_arr(i)%int_arr
                write (unit=4) tmp
                deallocate(tmp)
            end do
            close (unit=4)
        end subroutine write_data
        
        function lookup_var(var_name)
            character(len=100) :: var_name
            integer            :: lookup_var
            
            lookup_var = -1
            
            do i = 1, data_store%total
                if (data_store%names(i) == var_name) then
                    lookup_var = i
                    exit
                end if
            end do
        end function lookup_var
        
        subroutine nc_diag_realloc_string(arr, addl_num_entries)
            character(len=*), dimension(:), allocatable, intent(inout) :: arr
            integer,intent(in) :: addl_num_entries
            
            character(len=len(arr(1))), dimension(:), allocatable   :: tmp
            integer            :: new_size
            
            integer                              :: alloc_err
            character(len=100)                           :: err_msg
            
            new_size = size(arr) + addl_num_entries
            
            allocate(tmp(new_size), STAT=alloc_err)
            if (alloc_err /= 0) then
                write(*, "(A, I0)") "Reallocator was unable to reallocate memory! Error code: ", alloc_err
                stop
            end if
            tmp(1:size(arr)) = arr
            deallocate(arr)
            allocate(arr(new_size))
            arr = tmp
        end subroutine nc_diag_realloc_string
        subroutine nc_diag_realloc_darr(addl_num_entries)
            integer, intent(in)    :: addl_num_entries
            
            type(data_arr), dimension(:), allocatable   :: tmp_stor_data_arr
            
            ! We need to realloc ourselves here...
            allocate(tmp_stor_data_arr(size(data_store%stor_data_arr) + addl_num_entries))
            tmp_stor_data_arr(1:size(data_store%stor_data_arr)) = data_store%stor_data_arr
            deallocate(data_store%stor_data_arr)
            allocate(data_store%stor_data_arr(size(tmp_stor_data_arr)))
            data_store%stor_data_arr = tmp_stor_data_arr
            deallocate(tmp_stor_data_arr)
        end subroutine nc_diag_realloc_darr
        subroutine nc_diag_realloc_long(arr, addl_num_entries)
            integer, dimension(:), allocatable, intent(inout) :: arr
            integer,intent(in) :: addl_num_entries
            
            integer, dimension(:), allocatable   :: tmp
            integer                             :: new_size
            
            integer                              :: alloc_err
            character(len=100)                           :: err_msg
            
            new_size = size(arr) + addl_num_entries
            
            allocate(tmp(new_size), STAT=alloc_err)
            if (alloc_err /= 0) then
                write(*, "(A, I0)") "Reallocator was unable to reallocate memory! Error code: ", alloc_err
                stop
            end if
            
            tmp(1:size(arr)) = arr
            deallocate(arr)
            allocate(arr(new_size))
            arr = tmp
        end subroutine nc_diag_realloc_long
        subroutine nc_diag_resize_darr(darr_index, addl_num_entries)
            integer, intent(in)     :: darr_index
            integer, intent(in)    :: addl_num_entries
            
            integer                :: addl_num_entries_r
            
            if (allocated(data_store%stor_data_arr(darr_index)%int_arr)) then
                data_store%stor_data_arr(darr_index)%dcount = &
                    data_store%stor_data_arr(darr_index)%dcount + addl_num_entries
                if (data_store%stor_data_arr(darr_index)%dcount >= data_store%stor_data_arr(darr_index)%dsize) then
                    addl_num_entries_r = addl_num_entries + data_store%stor_data_arr(darr_index)%dsize
                    call nc_diag_realloc_long(data_store%stor_data_arr(darr_index)%int_arr, addl_num_entries_r)
                    
                    data_store%stor_data_arr(darr_index)%dsize = size(data_store%stor_data_arr(darr_index)%int_arr)
                end if
            else
                data_store%stor_data_arr(darr_index)%dcount = addl_num_entries
                allocate(data_store%stor_data_arr(darr_index)%int_arr(addl_num_entries + INITIAL_SIZE))
                data_store%stor_data_arr(darr_index)%dsize = addl_num_entries + INITIAL_SIZE
            end if
        end subroutine nc_diag_resize_darr
end program test_index
