        ! ---------------------------
        ! nc_diag_realloc definitions
        ! ---------------------------
        ! DO NOT COMPILE THIS DIRECTLY! THIS IS MEANT TO BE INCLUDED
        ! INSIDE A LARGER F90 SOURCE!
        ! If you compile this directly, you WILL face the WRATH of your
        ! compiler!
        
        ! nc_diag_realloc subroutines provide reallocation functionality
        ! for various inputs.
        
        ! This file provides the actual subroutines, referred to by the
        ! interface.
        
        ! nc_diag_realloc(arr, addl_num_entries)
        !    input: integer(i_byte), dimension(:) (1D)
        !    integer(i_long), intent(in) :: addl_num_entries
        subroutine nc_diag_realloc_byte(arr, addl_num_entries)
            integer(i_byte), dimension(:), allocatable, intent(inout) :: arr
            integer(i_long), intent(in) :: addl_num_entries
            
            integer(i_byte), dimension(:), allocatable   :: tmp
            
            allocate(tmp(size(arr) + addl_num_entries))
            tmp(1:size(arr)) = arr
            deallocate(arr)
            allocate(arr(size(tmp)))
            arr = tmp
        end subroutine nc_diag_realloc_byte
        
        ! nc_diag_realloc(arr, addl_num_entries)
        !    input: integer(i_short), dimension(:) (1D)
        !    integer(i_long), intent(in) :: addl_num_entries
        subroutine nc_diag_realloc_short(arr, addl_num_entries)
            integer(i_short), dimension(:), allocatable, intent(inout) :: arr
            integer(i_long), intent(in) :: addl_num_entries
            
            integer(i_short), dimension(:), allocatable   :: tmp
            
            allocate(tmp(size(arr) + addl_num_entries))
            tmp(1:size(arr)) = arr
            deallocate(arr)
            allocate(arr(size(tmp)))
            arr = tmp
        end subroutine nc_diag_realloc_short
        
        ! nc_diag_realloc(arr, addl_num_entries)
        !    input: integer(i_long), dimension(:) (1D)
        !    integer(i_long), intent(in) :: addl_num_entries
        subroutine nc_diag_realloc_long(arr, addl_num_entries)
            integer(i_long), dimension(:), allocatable, intent(inout) :: arr
            integer(i_long), intent(in) :: addl_num_entries
            
            integer(i_long), dimension(:), allocatable   :: tmp
            
            call debug("Reallocating long array...")
            
            allocate(tmp(size(arr) + addl_num_entries))
            tmp(1:size(arr)) = arr
            deallocate(arr)
            allocate(arr(size(tmp)))
            arr = tmp
            call debug("Realloc finished for long")
        end subroutine nc_diag_realloc_long
        
        ! nc_diag_realloc(arr, addl_num_entries)
        !    input: real(r_single), dimension(:) (1D)
        !    integer(i_long), intent(in) :: addl_num_entries
        subroutine nc_diag_realloc_rsingle(arr, addl_num_entries)
            real(r_single), dimension(:), allocatable, intent(inout) :: arr
            integer(i_long), intent(in) :: addl_num_entries
            
            real(r_single), dimension(:), allocatable   :: tmp
            
            allocate(tmp(size(arr) + addl_num_entries))
            tmp(1:size(arr)) = arr
            deallocate(arr)
            allocate(arr(size(tmp)))
            arr = tmp
        end subroutine nc_diag_realloc_rsingle
        
        ! nc_diag_realloc(arr, addl_num_entries)
        !    input: real(r_double), dimension(:) (1D)
        !    integer(i_long), intent(in) :: addl_num_entries
        subroutine nc_diag_realloc_rdouble(arr, addl_num_entries)
            real(r_double), dimension(:), allocatable, intent(inout) :: arr
            integer(i_long), intent(in) :: addl_num_entries
            
            real(r_double), dimension(:), allocatable   :: tmp
            
            allocate(tmp(size(arr) + addl_num_entries))
            tmp(1:size(arr)) = arr
            deallocate(arr)
            allocate(arr(size(tmp)))
            arr = tmp
        end subroutine nc_diag_realloc_rdouble
        
        ! nc_diag_realloc(arr, addl_num_entries)
        !    input: character(len=*), dimension(:) (1D)
        !    integer(i_long), intent(in) :: addl_num_entries
        subroutine nc_diag_realloc_string(arr, addl_num_entries)
            character(len=*), dimension(:), allocatable, intent(inout) :: arr
            integer(i_long), intent(in) :: addl_num_entries
            
            character(len=len(arr(1))), dimension(:), allocatable   :: tmp
            integer :: string_len, string_arr_size
            
            string_len = len(arr(1))
            string_arr_size = size(arr)
            
            call debug("Length of string to allocate to:")
            print *, string_len
            
            call debug("Allocating from...")
            print *, string_arr_size
            
            call debug("...to size...")
            print *, (string_arr_size + addl_num_entries)
            
            allocate(tmp(string_arr_size + addl_num_entries))
            tmp(1:string_arr_size) = arr
            deallocate(arr)
            allocate(arr(size(tmp)))
            arr = tmp
        end subroutine nc_diag_realloc_string
        
        ! nc_diag_realloc(arr, addl_num_entries)
        !    input: logical, dimension(:) (1D)
        !    integer(i_long), intent(in) :: addl_num_entries
        subroutine nc_diag_realloc_logical(arr, addl_num_entries)
            logical, dimension(:), allocatable, intent(inout) :: arr
            integer(i_long), intent(in) :: addl_num_entries
            
            logical, dimension(:), allocatable   :: tmp
            
            allocate(tmp(size(arr) + addl_num_entries))
            tmp(1:size(arr)) = arr
            deallocate(arr)
            allocate(arr(size(tmp)))
            arr = tmp
        end subroutine nc_diag_realloc_logical
