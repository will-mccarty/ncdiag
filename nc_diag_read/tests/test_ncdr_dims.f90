program test_ncdr_get
    use nc_diag_read
    use netcdf
    
    integer(i_long) :: ndims, ndims_len
    character(len=:), dimension(:), allocatable :: dim_names
    
    integer(i_long) :: i, tmp_ncid, tmp_ncid_2, ind
    character(len=:), allocatable :: dim_name
    
    ! nc_diag_read_lookup_dim
    ! nc_diag_read_assert_dim
    ! nc_diag_read_check_dim
    ! nc_diag_read_get_dim
    ! nc_diag_read_check_dim_unlim
    ! nc_diag_read_get_dim_names
    ! 
    ! All functions except last one!
    
    !------------------------------------------------------------------
    ! Subroutine allocation method testing
    !------------------------------------------------------------------
    
    !------------------------------------------------------------------
    ! Make sure if we close with NCID via caching, we actually clear
    ! the cache!
    !------------------------------------------------------------------
    call nc_diag_read_init("test.nc", tmp_ncid)
    
    write (*, "(A)") " ** File: test.nc (using cached NCID)"
    
    call nc_diag_read_get_dim_names(ndims, ndims_len, dim_names)
    write (*, "(A, I0, A, I0)") " ** Number of dimensions in test.nc: ", ndims, &
        " | Maximum length of dimension names: ", ndims_len
    print *, "** All dimensions: **"
    print *, dim_names
    
    print *, "** Dimensions details: **"
    
    do i = 1, ndims
        dim_name = trim(dim_names(i))
        ind = nc_diag_read_lookup_dim(dim_name)
        ind = nc_diag_read_assert_dim(dim_name)
        if (nc_diag_read_check_dim(dim_name) == .FALSE.) &
            call error("Can't find dim with check(), even when it's listed!")
        write (*, "(A, I0, A, L)") "    -> Dimension: " // dim_name // " | Size : ", &
            nc_diag_read_get_dim(dim_name), " | Unlimited? ", &
            nc_diag_read_check_dim_unlim(dim_name)
    end do
    
    tmp_ncid_2 = nc_diag_read_id_init("test_fixed.nc")
    
    deallocate(dim_names)
    
    write (*, "(A)") " ** File: test_fixed.nc (using NCID)"
    
    call nc_diag_read_get_dim_names(tmp_ncid_2, ndims, ndims_len, dim_names)
    write (*, "(A, I0, A, I0)") " ** Number of dimensions in test_fixed.nc: ", ndims, &
        " | Maximum length of dimension names: ", ndims_len
    print *, "** All dimensions: **"
    print *, dim_names
    
    print *, "** Dimensions details: **"
    
    do i = 1, ndims
        dim_name = trim(dim_names(i))
        ind = nc_diag_read_lookup_dim(tmp_ncid_2, dim_name)
        ind = nc_diag_read_assert_dim(tmp_ncid_2, dim_name)
        if (nc_diag_read_check_dim(tmp_ncid_2, dim_name) == .FALSE.) &
            call error("Can't find dim with check(), even when it's listed!")
        write (*, "(A, I0, A, L)") "    -> Dimension: " // dim_name // " | Size : ", &
            nc_diag_read_get_dim(tmp_ncid_2, dim_name), " | Unlimited? ", &
            nc_diag_read_check_dim_unlim(tmp_ncid_2, dim_name)
    end do
    
    call nc_diag_read_close(file_ncid = tmp_ncid)
    
    call nc_diag_read_close(file_ncid = tmp_ncid_2)
    
    contains
        subroutine display_current_state
            character(len=100)                         :: file_name
            integer(i_long)                            :: file_ncid
            
            call nc_diag_read_get_current(file_name, file_ncid)
            
            write (*, "(A, I0, A)") " ** Current file: " // trim(file_name) // &
                " (NCID: ", file_ncid, ")"
            
            call nc_diag_read_get_current_queue(file_name, file_ncid)
            
            write (*, "(A, I0, A)") " ** Current file in queue: " // trim(file_name) // &
                " (NCID: ", file_ncid, ")"
        end subroutine display_current_state
        
        subroutine display_1d_var_byte(var_name)
            character(len=*)                           :: var_name
            integer(i_byte), dimension(:), allocatable :: var_stor
            
            integer(i_long) :: i
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (1D): " // var_name // " (Elements: ", size(var_stor), ")"
            
            !print *, var_stor
            
            !do i = 1, size(var_stor)
            !    if (var_stor(i) == NF90_FILL_INT) then
            !        write (*, "(A5)", advance = "no") "(em) "
            !    else
            !        write (*, "(I4, A)", advance = "no") var_stor(i), " "
            !    end if
            !end do
            
            do i = 1, size(var_stor)
                if (var_stor(i) == NF90_FILL_INT) then
                    write (*, "(A4)") "(em)"
                else
                    write (*, "(I4)") var_stor(i)
                end if
            end do
            
            write (*, "(A)") ""
        end subroutine display_1d_var_byte
        
        subroutine display_1d_var_short(var_name)
            character(len=*)                           :: var_name
            integer(i_short), dimension(:), allocatable :: var_stor
            
            integer(i_long) :: i
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (1D): " // var_name // " (Elements: ", size(var_stor), ")"
            
            !print *, var_stor
            
            !do i = 1, size(var_stor)
            !    if (var_stor(i) == NF90_FILL_INT) then
            !        write (*, "(A7)", advance = "no") "(emp) "
            !    else
            !        write (*, "(I6, A)", advance = "no") var_stor(i), " "
            !    end if
            !end do
            
            do i = 1, size(var_stor)
                if (var_stor(i) == NF90_FILL_INT) then
                    write (*, "(A6)") "(emp)"
                else
                    write (*, "(I6)") var_stor(i)
                end if
            end do
            
            write (*, "(A)") ""
        end subroutine display_1d_var_short
        
        subroutine display_1d_var_long(var_name)
            character(len=*)                           :: var_name
            integer(i_long), dimension(:), allocatable :: var_stor
            
            integer(i_long) :: i
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (1D): " // var_name // " (Elements: ", size(var_stor), ")"
            !print *, var_stor
            
            !do i = 1, size(var_stor)
            !    if (var_stor(i) == NF90_FILL_INT) then
            !        write (*, "(A13)", advance = "no") "(empty) "
            !    else
            !        write (*, "(I12, A)", advance = "no") var_stor(i), " "
            !    end if
            !end do
            
            do i = 1, size(var_stor)
                if (var_stor(i) == NF90_FILL_INT) then
                    write (*, "(A12)") "(empty)"
                else
                    write (*, "(I12)") var_stor(i)
                end if
            end do
            
            write (*, "(A)") ""
            
        end subroutine display_1d_var_long
        
        subroutine display_1d_var_float(var_name)
            character(len=*)                           :: var_name
            real(r_single), dimension(:), allocatable  :: var_stor
            
            integer(i_long) :: i
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (1D): " // var_name // " (Elements: ", size(var_stor), ")"
            !print *, var_stor
            
            !do i = 1, size(var_stor)
            !    if (var_stor(i) == NF90_FILL_FLOAT) then
            !        write (*, "(A19)", advance = "no") "(empty) "
            !    else
            !        write (*, "(F18.10, A)", advance = "no") var_stor(j, i), " "
            !    end if
            !end do
            
            do i = 1, size(var_stor)
                if (var_stor(i) == NF90_FILL_FLOAT) then
                    write (*, "(A18)") "(empty)"
                else
                    write (*, "(F18.10)") var_stor(i)
                end if
            end do
        end subroutine display_1d_var_float
        
        subroutine display_1d_var_double(var_name)
            character(len=*)                           :: var_name
            real(r_double), dimension(:), allocatable  :: var_stor
            
            integer(i_long) :: i
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (1D): " // var_name // " (Elements: ", size(var_stor), ")"
            !print *, var_stor
            
            !do i = 1, size(var_stor)
            !    if (var_stor(i) == NF90_FILL_FLOAT) then
            !        write (*, "(A17)", advance = "no") "(empty) "
            !    else
            !        write (*, "(F16.13, A)", advance = "no") var_stor(j, i), " "
            !    end if
            !end do
            
            do i = 1, size(var_stor)
                if (var_stor(i) == NF90_FILL_DOUBLE) then
                    write (*, "(A16)") "(empty)"
                else
                    write (*, "(F16.13)") var_stor(i)
                end if
            end do
        end subroutine display_1d_var_double
        
        subroutine display_1d_var_string(var_name)
            character(len=*)                           :: var_name
            character(len=:), dimension(:), allocatable:: var_stor
            
            integer(i_long) :: i
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (1D): " // var_name // " (Elements: ", size(var_stor), ")"
            
            !do i = 1, size(var_stor)
            !    if ((i > 1) .AND. (mod(i - 1, 5) == 0)) write (*, "(A)") ""
            !    if ((var_stor(i)(1:1) == NF90_FILL_CHAR) .OR. (len(var_stor(i)) == 0)) then
            !        write (*, "(A20)", advance = "no") "(empty) "
            !    else
            !        write (*, "(A20)", advance = "no") '"' // var_stor(i) // '" '
            !    end if
            !end do
            
            do i = 1, size(var_stor)
                if ((var_stor(i)(1:1) == NF90_FILL_CHAR) .OR. (len(var_stor(i)) == 0)) then
                    write (*, "(A20)") "(empty)"
                else
                    write (*, "(A20)") '"' // var_stor(i) // '"'
                end if
            end do
            
            write (*, "(A)") ""
            
        end subroutine display_1d_var_string
        
        subroutine display_2d_var_byte(var_name)
            character(len=*)                           :: var_name
            integer(i_byte),dimension(:,:),allocatable :: var_stor
            
            integer(i_long) :: i, j
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (2D): " // var_name // " (Elements: ", size(var_stor), ")"
            
            do i = 1, size(var_stor, 2)
                do j = 1, size(var_stor, 1)
                    if ((j > 1) .AND. (mod(j - 1, 5) == 0)) write (*, "(A)") "..."
                    if (var_stor(j, i) == NF90_FILL_BYTE) then
                        write (*, "(A5)", advance = "no") "(e) "
                    else
                        write (*, "(I4, A)", advance = "no") var_stor(j, i), " "
                    end if
                end do
                write (*, "(A)") ""
            end do
        end subroutine display_2d_var_byte
        
        subroutine display_2d_var_short(var_name)
            character(len=*)                           :: var_name
            integer(i_short),dimension(:,:),allocatable :: var_stor
            
            integer(i_long) :: i, j
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (2D): " // var_name // " (Elements: ", size(var_stor), ")"
            
            do i = 1, size(var_stor, 2)
                do j = 1, size(var_stor, 1)
                    if ((j > 1) .AND. (mod(j - 1, 5) == 0)) write (*, "(A)") "..."
                    if (var_stor(j, i) == NF90_FILL_SHORT) then
                        write (*, "(A7)", advance = "no") "(emp) "
                    else
                        write (*, "(I6, A)", advance = "no") var_stor(j, i), " "
                    end if
                end do
                write (*, "(A)") ""
            end do
        end subroutine display_2d_var_short
        
        subroutine display_2d_var_long(var_name)
            character(len=*)                           :: var_name
            integer(i_long),dimension(:,:),allocatable :: var_stor
            
            integer(i_long) :: i, j
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (2D): " // var_name // " (Elements: ", size(var_stor), ")"
            
            do i = 1, size(var_stor, 2)
                do j = 1, size(var_stor, 1)
                    if ((j > 1) .AND. (mod(j - 1, 5) == 0)) write (*, "(A)") "..."
                    if (var_stor(j, i) == NF90_FILL_INT) then
                        write (*, "(A13)", advance = "no") "(empty) "
                    else
                        write (*, "(I12, A)", advance = "no") var_stor(j, i), " "
                    end if
                end do
                write (*, "(A)") ""
            end do
        end subroutine display_2d_var_long
        
        subroutine display_2d_var_float(var_name)
            character(len=*)                           :: var_name
            real(r_single), dimension(:,:), allocatable:: var_stor
            
            integer(i_long) :: i, j
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (2D): " // var_name // " (Elements: ", size(var_stor), ")"
            
            do i = 1, size(var_stor, 2)
                do j = 1, size(var_stor, 1)
                    if ((j > 1) .AND. (mod(j - 1, 5) == 0)) write (*, "(A)") "..."
                    if (var_stor(j, i) == NF90_FILL_FLOAT) then
                        write (*, "(A19)", advance = "no") "(empty) "
                    else
                        write (*, "(F18.10, A)", advance = "no") var_stor(j, i), " "
                    end if
                end do
                write (*, "(A)") ""
            end do
        end subroutine display_2d_var_float
        
        subroutine display_2d_var_double(var_name)
            character(len=*)                           :: var_name
            real(r_double), dimension(:,:), allocatable:: var_stor
            
            integer(i_long) :: i, j
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (2D): " // var_name // " (Elements: ", size(var_stor), ")"
            
            do i = 1, size(var_stor, 2)
                do j = 1, size(var_stor, 1)
                    if ((j > 1) .AND. (mod(j - 1, 5) == 0)) write (*, "(A)") "..."
                    if (var_stor(j, i) == NF90_FILL_DOUBLE) then
                        write (*, "(A17)", advance = "no") "(empty) "
                    else
                        write (*, "(F16.13, A)", advance = "no") var_stor(j, i), " "
                    end if
                end do
                write (*, "(A)") ""
            end do
            
        end subroutine display_2d_var_double
        
        ! NOTE - dimensions have to be flipped
        subroutine display_2d_var_string(var_name)
            character(len=*)                           :: var_name
            character(len=:),dimension(:,:),allocatable:: var_stor
            
            integer(i_long) :: i, j
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (2D): " // var_name // " (Elements: ", size(var_stor), ")"
            
            print *, shape(var_stor)
            
            do i = 1, size(var_stor, 2)
                do j = 1, size(var_stor, 1)
                    if ((j > 1) .AND. (mod(j - 1, 5) == 0)) write (*, "(A)") "..."
                    if ((var_stor(j, i)(1:1) == NF90_FILL_CHAR) .OR. (len(var_stor(j, i)) == 0)) then
                        write (*, "(A20)", advance = "no") "(empty) "
                    else
                        write (*, "(A20)", advance = "no") '"' // var_stor(j, i) // '" '
                    end if
                end do
                write (*, "(A)") ""
            end do
            
            write (*, "(A)") ""
            
        end subroutine display_2d_var_string
end program test_ncdr_get
