program test_nc_diag_read
    use nc_diag_read
    
    call nc_diag_read_init("test.nc")
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
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (1D): " // var_name // " (Elements: ", size(var_stor), ")"
            print *, var_stor
        end subroutine display_1d_var_byte
        
        subroutine display_1d_var_short(var_name)
            character(len=*)                           :: var_name
            integer(i_short), dimension(:), allocatable :: var_stor
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (1D): " // var_name // " (Elements: ", size(var_stor), ")"
            print *, var_stor
        end subroutine display_1d_var_short
        
        subroutine display_1d_var_long(var_name)
            character(len=*)                           :: var_name
            integer(i_long), dimension(:), allocatable :: var_stor
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (1D): " // var_name // " (Elements: ", size(var_stor), ")"
            print *, var_stor
        end subroutine display_1d_var_long
        
        subroutine display_1d_var_float(var_name)
            character(len=*)                           :: var_name
            real(r_single), dimension(:), allocatable  :: var_stor
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (1D): " // var_name // " (Elements: ", size(var_stor), ")"
            print *, var_stor
        end subroutine display_1d_var_float
        
        subroutine display_1d_var_double(var_name)
            character(len=*)                           :: var_name
            real(r_double), dimension(:), allocatable  :: var_stor
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (1D): " // var_name // " (Elements: ", size(var_stor), ")"
            print *, var_stor
        end subroutine display_1d_var_double
        
        subroutine display_1d_var_string(var_name)
            character(len=*)                           :: var_name
            character(len=:), dimension(:), allocatable:: var_stor
            
            integer(i_long) :: i
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (1D): " // var_name // " (Elements: ", size(var_stor), ")"
            
            do i = 1, size(var_stor)
                if ((i > 1) .AND. (mod(i - 1, 5) == 0)) write (*, "(A)") ""
                write (*, "(A20)", advance = "no") '"' // var_stor(i) // '" '
            end do
            
            write (*, "(A)") ""
            
        end subroutine display_1d_var_string
        
        subroutine display_2d_var_byte(var_name)
            character(len=*)                           :: var_name
            integer(i_byte),dimension(:,:),allocatable :: var_stor
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (2D): " // var_name // " (Elements: ", size(var_stor), ")"
            print *, var_stor
        end subroutine display_2d_var_byte
        
        subroutine display_2d_var_short(var_name)
            character(len=*)                           :: var_name
            integer(i_short),dimension(:,:),allocatable :: var_stor
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (2D): " // var_name // " (Elements: ", size(var_stor), ")"
            print *, var_stor
        end subroutine display_2d_var_short
        
        subroutine display_2d_var_long(var_name)
            character(len=*)                           :: var_name
            integer(i_long),dimension(:,:),allocatable :: var_stor
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            print *, shape(var_stor)
            write (*, "(A, I0, A)") " ** Variable (2D): " // var_name // " (Elements: ", size(var_stor), ")"
            print *, var_stor
        end subroutine display_2d_var_long
        
        subroutine display_2d_var_float(var_name)
            character(len=*)                           :: var_name
            real(r_single), dimension(:,:), allocatable:: var_stor
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (2D): " // var_name // " (Elements: ", size(var_stor), ")"
            print *, var_stor
        end subroutine display_2d_var_float
        
        subroutine display_2d_var_double(var_name)
            character(len=*)                           :: var_name
            real(r_double), dimension(:,:), allocatable:: var_stor
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (2D): " // var_name // " (Elements: ", size(var_stor), ")"
            print *, var_stor
        end subroutine display_2d_var_double
        
        subroutine display_2d_var_string(var_name)
            character(len=*)                           :: var_name
            character(len=:),dimension(:,:),allocatable:: var_stor
            
            integer(i_long) :: i, j
            
            call nc_diag_read_get_var(var_name, var_stor)
            
            write (*, "(A, I0, A)") " ** Variable (2D): " // var_name // " (Elements: ", size(var_stor), ")"
            
            print *, shape(var_stor)
            
            do i = 1, size(var_stor, 2)
                do j = 1, size(var_stor, 1)
                    if ((j > 1) .AND. (mod(j - 1, 5) == 0)) write (*, "(A)") ""
                    write (*, "(A30)", advance = "no") '"' // var_stor(j, i) // '" '
                end do
                write (*, "(A)") ""
                write (*, "(A)") "== END ROW =="
            end do
            
            write (*, "(A)") ""
            
        end subroutine display_2d_var_string
end program test_nc_diag_read
