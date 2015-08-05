program test_ncdr_get
    use nc_diag_read
    use netcdf
    
    integer(i_long) :: nvars, nvars_len
    character(len=:), dimension(:), allocatable :: var_names
    
    integer(i_long) :: ndims, ndims_len
    character(len=:), dimension(:), allocatable :: dim_names
    
    !------------------------------------------------------------------
    ! Subroutine allocation method testing
    !------------------------------------------------------------------
    
    call nc_diag_read_init("test.nc")
    
    ! Using an invalid NCDR ID should fail:
    !print *, nc_diag_read_get_dim(1234567, "asdf")
    
    call nc_diag_read_get_dim_names(ndims, ndims_len, dim_names)
    write (*, "(A, I0, A, I0)") " ** Number of dimensions: ", ndims, &
        " | Maximum length of dimension names: ", ndims_len
    print *, "** All dimensions: **"
    print *, dim_names
    
    call nc_diag_read_get_var_names(nvars, nvars_len, var_names)
    write (*, "(A, I0, A, I0)") " ** Number of variables: ", nvars, &
        " | Maximum length of variable names: ", nvars_len
    print *, "** All variables: **"
    print *, var_names
    
    !call nc_diag_read_init("test_fixed.nc")
    call display_1d_var_long("chaninfosimple1")
    call display_1d_var_long("chaninfosimple2")
    
    ! This won't work due to mismatched types:
    !call display_1d_var_byte("chaninfosimple2")
    
    call display_1d_var_float("chaninfosimple4_float")
    
    ! Mismatched types:
    !call display_1d_var_long("chaninfosimple4_float")
    !call display_1d_var_double("chaninfosimple4_float")
    
    call display_1d_var_double("chaninfosimple5_double")
    
    call display_1d_var_string("chaninfosimple6_str")
    call display_1d_var_string("chaninfosimple7_str")
    
    call display_1d_var_long("chaninfosimple3_notcomplete")
    
    call display_1d_var_string("chaninfosimple8_str")
    
    call display_1d_var_long("chaninfosimple9_buf")
    call display_1d_var_long("chaninfosimple10_notcomplete")
    
    call display_1d_var_long("metadatasimple1")
    
    call display_1d_var_string("metadatasimple6_str")
    call display_1d_var_string("metadatasimple8_str")
    
    call display_1d_var_long("metadata_notcomplete")
    
    call display_1d_var_string("metadata_str_notcomplete")
    
    call display_2d_var_long("data2dsimple1")
    call display_2d_var_long("data2dsimple2")
    
    call display_2d_var_float("data2dsimple4_float")
    call display_2d_var_float("data2dsimple4_float2")
    
    call display_2d_var_double("data2dsimple5_double")
    
    call display_2d_var_long("data2dsimple99")
    
    call display_2d_var_string("data2dsimple6_str")
    
    call display_2d_var_long("data2dsimple7")
    call display_2d_var_long("data2d_notcomplete")
    
    call nc_diag_read_close
    
    contains
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
