module ncdr_alloc_assert
    ! Allocate if things aren't allocated, or assert that things are
    ! all good to go.
    ! 
    ! Other parts include just assertion functions (e.g. asserting
    ! that a variable exists).
    use kinds
    use ncdr_check
    implicit none
    
    interface nc_diag_read_assert_var
        module procedure nc_diag_read_id_assert_var, &
            nc_diag_read_noid_assert_var
    end interface nc_diag_read_assert_var
    
    interface nc_diag_read_assert_var_dims
        module procedure &
            nc_diag_read_assert_var_dims_1d_byte, &
            nc_diag_read_assert_var_dims_1d_short, &
            nc_diag_read_assert_var_dims_1d_long, &
            nc_diag_read_assert_var_dims_1d_float, &
            nc_diag_read_assert_var_dims_1d_double, &
            nc_diag_read_assert_var_dims_1d_string, &
            nc_diag_read_assert_var_dims_2d_byte, &
            nc_diag_read_assert_var_dims_2d_short, &
            nc_diag_read_assert_var_dims_2d_long, &
            nc_diag_read_assert_var_dims_2d_float, &
            nc_diag_read_assert_var_dims_2d_double, &
            nc_diag_read_assert_var_dims_2d_string
    end interface nc_diag_read_assert_var_dims
    
    contains
        function nc_diag_read_id_assert_var(file_ncid, var_name) result(var_index)
            integer, intent(in)            :: file_ncid
            character(len=*), intent(in)   :: var_name
            
            integer                        :: var_index, file_ind
            
            call ncdr_check_ncid(file_ncid)
            
            file_ind = nc_diag_read_get_index_from_ncid(file_ncid)
            
            do var_index = 1, ncdr_files(file_ind)%nvars
                if (ncdr_files(file_ind)%vars(var_index)%var_name == var_name) &
                    return
            end do
            
            ! If we didn't find anything, show an error!
            call error("The specified variable '" // var_name // "' does not exist!")
        end function nc_diag_read_id_assert_var
        
        function nc_diag_read_noid_assert_var(var_name) result(var_index)
            character(len=*), intent(in)   :: var_name
            
            integer                        :: var_index
            
            call ncdr_check_current_ncid
            
            var_index = nc_diag_read_id_assert_var(current_ncid, var_name)
        end function nc_diag_read_noid_assert_var
        
        subroutine nc_diag_read_assert_var_type(var_type, correct_var_type)
            integer(i_long)                            :: var_type
            integer(i_long)                            :: correct_var_type
            
            if (var_type /= correct_var_type) &
                call error("Mismatched type for variable! Got " // &
                    nc_diag_read_get_type_str(var_type) // &
                    " when " // &
                    nc_diag_read_get_type_str(correct_var_type) // &
                    " was expected for the variable!")
        end subroutine nc_diag_read_assert_var_type
        
        function nc_diag_read_get_type_str(var_type) result(type_str)
            integer(i_long)                            :: var_type
            character(len=:), allocatable              :: type_str
            
            if (var_type == NF90_BYTE) then
                type_str = "NF90_BYTE"
            else if (var_type == NF90_SHORT) then
                type_str = "NF90_SHORT"
            else if (var_type == NF90_INT) then
                type_str = "NF90_INT"
            else if (var_type == NF90_FLOAT) then
                type_str = "NF90_FLOAT"
            else if (var_type == NF90_DOUBLE) then
                type_str = "NF90_DOUBLE"
            else if (var_type == NF90_CHAR) then
                type_str = "NF90_CHAR"
            else if (var_type == NF90_STRING) then
                type_str = "NF90_STRING (not supported)"
            else
                type_str = "(unknown type)"
            end if
        end function nc_diag_read_get_type_str
        
        subroutine nc_diag_read_assert_var_ndims(var_ndims, correct_var_ndims)
            integer(i_long)                            :: var_ndims
            integer(i_long)                            :: correct_var_ndims
            
            if (var_ndims /= correct_var_ndims) &
                call error("Mismatched dimensions for variable!")
        end subroutine nc_diag_read_assert_var_ndims
        
        !-------------------------------------------------------------
        ! Variable allocation and assertion subroutines
        !-------------------------------------------------------------
        
        subroutine nc_diag_read_assert_var_dims_1d_byte(var_stor, correct_dims)
            integer(i_byte),dimension(:),allocatable,intent(inout) :: var_stor
            integer(i_long), dimension(:)                           :: correct_dims
            integer(i_long), parameter :: correct_ndims = 1
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call error("Invalid number of dimensions for variable!")
                if (any(shape(var_stor) /= correct_dims)) &
                    call error("Mismatched dimensions for variable storage!")
            else
                allocate(var_stor(correct_dims(1)))
            end if
        end subroutine nc_diag_read_assert_var_dims_1d_byte
        
        subroutine nc_diag_read_assert_var_dims_1d_short(var_stor, correct_dims)
            integer(i_short),dimension(:),allocatable,intent(inout) :: var_stor
            integer(i_long), dimension(:)                           :: correct_dims
            integer(i_long), parameter :: correct_ndims = 1
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call error("Invalid number of dimensions for variable!")
                if (any(shape(var_stor) /= correct_dims)) &
                    call error("Mismatched dimensions for variable storage!")
            else
                allocate(var_stor(correct_dims(1)))
            end if
        end subroutine nc_diag_read_assert_var_dims_1d_short
        
        subroutine nc_diag_read_assert_var_dims_1d_long(var_stor, correct_dims)
            integer(i_long),dimension(:),allocatable,intent(inout) :: var_stor
            integer(i_long), dimension(:)                           :: correct_dims
            integer(i_long), parameter :: correct_ndims = 1
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call error("Invalid number of dimensions for variable!")
                if (any(shape(var_stor) /= correct_dims)) &
                    call error("Mismatched dimensions for variable storage!")
            else
                allocate(var_stor(correct_dims(1)))
            end if
        end subroutine nc_diag_read_assert_var_dims_1d_long
        
        subroutine nc_diag_read_assert_var_dims_1d_float(var_stor, correct_dims)
            real(r_single),dimension(:),allocatable,intent(inout) :: var_stor
            integer(i_long), dimension(:)                           :: correct_dims
            integer(i_long), parameter :: correct_ndims = 1
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call error("Invalid number of dimensions for variable!")
                if (any(shape(var_stor) /= correct_dims)) &
                    call error("Mismatched dimensions for variable storage!")
            else
                allocate(var_stor(correct_dims(1)))
            end if
        end subroutine nc_diag_read_assert_var_dims_1d_float
        
        subroutine nc_diag_read_assert_var_dims_1d_double(var_stor, correct_dims)
            real(r_double),dimension(:),allocatable,intent(inout) :: var_stor
            integer(i_long), dimension(:)                           :: correct_dims
            integer(i_long), parameter :: correct_ndims = 1
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call error("Invalid number of dimensions for variable!")
                if (any(shape(var_stor) /= correct_dims)) &
                    call error("Mismatched dimensions for variable storage!")
            else
                allocate(var_stor(correct_dims(1)))
            end if
        end subroutine nc_diag_read_assert_var_dims_1d_double
        
        subroutine nc_diag_read_assert_var_dims_1d_string(var_stor, correct_dims)
            character(len=:),dimension(:),allocatable,intent(inout) :: var_stor
            integer(i_long), dimension(:)                           :: correct_dims
            integer(i_long), parameter :: correct_ndims = 2
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call error("Invalid number of dimensions for variable!")
                if (any(shape(var_stor) /= correct_dims)) &
                    call error("Mismatched dimensions for variable storage!")
            else
                allocate(character(len=correct_dims(1)) :: var_stor(correct_dims(2)))
            end if
        end subroutine nc_diag_read_assert_var_dims_1d_string
        
        subroutine nc_diag_read_assert_var_dims_2d_byte(var_stor, correct_dims)
            integer(i_byte),dimension(:,:),allocatable,intent(inout):: var_stor
            integer(i_long), dimension(:)                           :: correct_dims
            integer(i_long), parameter :: correct_ndims = 2
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call error("Invalid number of dimensions for variable!")
                if (any(shape(var_stor) /= correct_dims)) &
                    call error("Mismatched dimensions for variable storage!")
            else
                allocate(var_stor(correct_dims(1), correct_dims(2)))
            end if
        end subroutine nc_diag_read_assert_var_dims_2d_byte
        
        subroutine nc_diag_read_assert_var_dims_2d_short(var_stor, correct_dims)
            integer(i_short),dimension(:,:),allocatable,intent(inout):: var_stor
            integer(i_long), dimension(:)                           :: correct_dims
            integer(i_long), parameter :: correct_ndims = 2
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call error("Invalid number of dimensions for variable!")
                if (any(shape(var_stor) /= correct_dims)) &
                    call error("Mismatched dimensions for variable storage!")
            else
                allocate(var_stor(correct_dims(1), correct_dims(2)))
            end if
        end subroutine nc_diag_read_assert_var_dims_2d_short
        
        subroutine nc_diag_read_assert_var_dims_2d_long(var_stor, correct_dims)
            integer(i_long),dimension(:,:),allocatable,intent(inout):: var_stor
            integer(i_long), dimension(:)                           :: correct_dims
            integer(i_long), parameter :: correct_ndims = 2
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call error("Invalid number of dimensions for variable!")
                if (any(shape(var_stor) /= correct_dims)) &
                    call error("Mismatched dimensions for variable storage!")
            else
                allocate(var_stor(correct_dims(1), correct_dims(2)))
            end if
        end subroutine nc_diag_read_assert_var_dims_2d_long
        
        subroutine nc_diag_read_assert_var_dims_2d_float(var_stor, correct_dims)
            real(r_single),dimension(:,:),allocatable,intent(inout):: var_stor
            integer(i_long), dimension(:)                           :: correct_dims
            integer(i_long), parameter :: correct_ndims = 2
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call error("Invalid number of dimensions for variable!")
                if (any(shape(var_stor) /= correct_dims)) &
                    call error("Mismatched dimensions for variable storage!")
            else
                allocate(var_stor(correct_dims(1), correct_dims(2)))
            end if
        end subroutine nc_diag_read_assert_var_dims_2d_float
        
        subroutine nc_diag_read_assert_var_dims_2d_double(var_stor, correct_dims)
            real(r_double),dimension(:,:),allocatable,intent(inout):: var_stor
            integer(i_long), dimension(:)                           :: correct_dims
            integer(i_long), parameter :: correct_ndims = 2
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call error("Invalid number of dimensions for variable!")
                if (any(shape(var_stor) /= correct_dims)) &
                    call error("Mismatched dimensions for variable storage!")
            else
                allocate(var_stor(correct_dims(1), correct_dims(2)))
            end if
        end subroutine nc_diag_read_assert_var_dims_2d_double
        
        subroutine nc_diag_read_assert_var_dims_2d_string(var_stor, correct_dims)
            character(len=:),dimension(:,:),allocatable,intent(inout):: var_stor
            integer(i_long), dimension(:)                           :: correct_dims
            integer(i_long), parameter :: correct_ndims = 3
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call error("Invalid number of dimensions for variable!")
                if (any(shape(var_stor) /= correct_dims)) &
                    call error("Mismatched dimensions for variable storage!")
            else
                allocate(character(len=correct_dims(1)) :: var_stor(correct_dims(2), correct_dims(3)))
            end if
        end subroutine nc_diag_read_assert_var_dims_2d_string
end module ncdr_alloc_assert
