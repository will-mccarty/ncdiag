        function nc_diag_varattr_check_var(var_name) result(found)
            character(len=*), intent(in)    :: var_name
            integer :: i
            logical :: found
            found = .FALSE.
            
            if (init_done .AND. allocated(diag_varattr_store)) then
                do i = 1, diag_varattr_store%total
                    if (diag_varattr_store%names(i) == var_name) then
                        found = .TRUE.
                        exit
                    end if
                end do
            end if
        end function nc_diag_varattr_check_var
        
        function nc_diag_varattr_lookup_var(var_name) result(ind)
            character(len=*), intent(in)    :: var_name
            integer :: i, ind
            
            ind = -1
            
            if (init_done .AND. allocated(diag_varattr_store)) then
                do i = 1, diag_varattr_store%total
                    if (diag_varattr_store%names(i) == var_name) then
                        ind = i
                        exit
                    end if
                end do
            end if
        end function nc_diag_varattr_lookup_var
        
        subroutine nc_diag_varattr_expand(addl_fields)
            integer(i_llong), intent(in) :: addl_fields
            integer(i_llong)             :: size_add
            
            if (init_done .AND. allocated(diag_varattr_store)) then
                if (allocated(diag_varattr_store%names)) then
                    if (diag_varattr_store%total >= size(diag_varattr_store%names)) then
                        size_add = (size(diag_varattr_store%names) * 0.5) + addl_fields
                        call nc_diag_realloc(diag_varattr_store%names, addl_fields)
                    end if
                else
                    allocate(diag_varattr_store%names(NLAYER_DEFAULT_ENT))
                end if
                
                if (allocated(diag_varattr_store%var_ids)) then
                    if (diag_varattr_store%total >= size(diag_varattr_store%var_ids)) then
                        size_add = (size(diag_varattr_store%var_ids) * 0.5) + addl_fields
                        call nc_diag_realloc(diag_varattr_store%var_ids, size_add)
                    end if
                else
                    allocate(diag_varattr_store%var_ids(NLAYER_DEFAULT_ENT))
                    diag_varattr_store%var_ids = -1
                end if
                
            else
                call error("NetCDF4 layer not initialized yet!")
            endif
            
        end subroutine nc_diag_varattr_expand
        
        subroutine nc_diag_varattr_add_var(var_name, var_id)
            character(len=*), intent(in)    :: var_name
            integer(i_long)                 :: var_id
            
            if (nc_diag_varattr_check_var(var_name)) then
                call error("Variable already exists for variable attributes!")
            else
                print *, "adding var!"
                call nc_diag_varattr_expand(1)
                diag_varattr_store%total = diag_varattr_store%total + 1
                diag_varattr_store%names(diag_varattr_store%total) = var_name
                diag_varattr_store%var_ids(diag_varattr_store%total) = var_id
                print *, "done adding var!"
            end if
        end subroutine nc_diag_varattr_add_var
        
        ! nc_diag_varattr - input integer(i_byte)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_varattr_byte(var_name, attr_name, attr_value)
            character(len=*), intent(in)    :: var_name
            character(len=*), intent(in)    :: attr_name
            integer(i_byte), intent(in)     :: attr_value
            
            integer(i_long)                 :: var_index
            
            if (nc_diag_varattr_check_var(var_name)) then
                var_index = nc_diag_varattr_lookup_var(var_name)
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index for attr!")
                call check(nf90_put_att(ncid, diag_varattr_store%var_ids(var_index), attr_name, attr_value))
            else
                call error("Can't set attribute for a non-existent variable!" &
                    // char(10) &
                    // "             (If you did add the variable, make sure you lock" &
                    // char(10) &
                    // "             the definitions before calling varattr!) ")
            end if
        end subroutine nc_diag_varattr_byte
        
        ! nc_diag_varattr - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_varattr_short(var_name, attr_name, attr_value)
            character(len=*), intent(in)    :: var_name
            character(len=*), intent(in)    :: attr_name
            integer(i_short), intent(in)    :: attr_value
            
            integer(i_long)                 :: var_index
            
            if (nc_diag_varattr_check_var(var_name)) then
                var_index = nc_diag_varattr_lookup_var(var_name)
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index for attr!")
                call check(nf90_put_att(ncid, diag_varattr_store%var_ids(var_index), attr_name, attr_value))
            else
                call error("Can't set attribute for a non-existent variable!" &
                    // char(10) &
                    // "             (If you did add the variable, make sure you lock" &
                    // char(10) &
                    // "             the definitions before calling varattr!) ")
            end if
        end subroutine nc_diag_varattr_short
        
        ! nc_diag_varattr - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_varattr_long(var_name, attr_name, attr_value)
            character(len=*), intent(in)    :: var_name
            character(len=*), intent(in)    :: attr_name
            integer(i_long), intent(in)     :: attr_value
            
            integer(i_long)                 :: var_index
            
            if (nc_diag_varattr_check_var(var_name)) then
                var_index = nc_diag_varattr_lookup_var(var_name)
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index for attr!")
                call check(nf90_put_att(ncid, diag_varattr_store%var_ids(var_index), attr_name, attr_value))
            else
                call error("Can't set attribute for a non-existent variable!" &
                    // char(10) &
                    // "             (If you did add the variable, make sure you lock" &
                    // char(10) &
                    // "             the definitions before calling varattr!) ")
            end if
        end subroutine nc_diag_varattr_long
        
        ! nc_diag_varattr - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_varattr_rsingle(var_name, attr_name, attr_value)
            character(len=*), intent(in)    :: var_name
            character(len=*), intent(in)    :: attr_name
            real(r_single), intent(in)      :: attr_value
            
            integer(i_long)                 :: var_index
            
            if (nc_diag_varattr_check_var(var_name)) then
                var_index = nc_diag_varattr_lookup_var(var_name)
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index for attr!")
                call check(nf90_put_att(ncid, diag_varattr_store%var_ids(var_index), attr_name, attr_value))
            else
                call error("Can't set attribute for a non-existent variable!" &
                    // char(10) &
                    // "             (If you did add the variable, make sure you lock" &
                    // char(10) &
                    // "             the definitions before calling varattr!) ")
            end if
        end subroutine nc_diag_varattr_rsingle
        
        ! nc_diag_varattr - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_varattr_rdouble(var_name, attr_name, attr_value)
            character(len=*), intent(in)    :: var_name
            character(len=*), intent(in)    :: attr_name
            real(r_double), intent(in)      :: attr_value
            
            integer(i_long)                 :: var_index
            
            if (nc_diag_varattr_check_var(var_name)) then
                var_index = nc_diag_varattr_lookup_var(var_name)
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index for attr!")
                call check(nf90_put_att(ncid, diag_varattr_store%var_ids(var_index), attr_name, attr_value))
            else
                call error("Can't set attribute for a non-existent variable!" &
                    // char(10) &
                    // "             (If you did add the variable, make sure you lock" &
                    // char(10) &
                    // "             the definitions before calling varattr!) ")
            end if
        end subroutine nc_diag_varattr_rdouble

        ! nc_diag_varattr - input character(len=*)
        ! Corresponding NetCDF4 type: string? char?
        subroutine nc_diag_varattr_string(var_name, attr_name, attr_value)
            character(len=*), intent(in)    :: var_name
            character(len=*), intent(in)    :: attr_name
            character(len=*), intent(in)    :: attr_value
            
            integer(i_long)                 :: var_index
            
            if (nc_diag_varattr_check_var(var_name)) then
                var_index = nc_diag_varattr_lookup_var(var_name)
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index for attr!")
                call check(nf90_put_att(ncid, diag_varattr_store%var_ids(var_index), attr_name, attr_value))
            else
                call error("Can't set attribute for a non-existent variable!" &
                    // char(10) &
                    // "             (If you did add the variable, make sure you lock" &
                    // char(10) &
                    // "             the definitions before calling varattr!) ")
            end if
        end subroutine nc_diag_varattr_string
        
        !=============================================================
        ! VECTOR TYPES
        !=============================================================
        
        ! nc_diag_varattr - input integer(i_byte), dimension(:)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_varattr_byte_v(var_name, attr_name, attr_value)
            character(len=*), intent(in)               :: var_name
            character(len=*), intent(in)               :: attr_name
            integer(i_byte), dimension(:), intent(in)  :: attr_value
            
            integer(i_long)                 :: var_index
            
            if (nc_diag_varattr_check_var(var_name)) then
                var_index = nc_diag_varattr_lookup_var(var_name)
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index for attr!")
                call check(nf90_put_att(ncid, diag_varattr_store%var_ids(var_index), attr_name, attr_value))
            else
                call error("Can't set attribute for a non-existent variable!" &
                    // char(10) &
                    // "             (If you did add the variable, make sure you lock" &
                    // char(10) &
                    // "             the definitions before calling varattr!) ")
            end if
        end subroutine nc_diag_varattr_byte_v
        
        ! nc_diag_varattr - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_varattr_short_v(var_name, attr_name, attr_value)
            character(len=*), intent(in)               :: var_name
            character(len=*), intent(in)               :: attr_name
            integer(i_short), dimension(:), intent(in) :: attr_value
            
            integer(i_long)                 :: var_index
            
            if (nc_diag_varattr_check_var(var_name)) then
                var_index = nc_diag_varattr_lookup_var(var_name)
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index for attr!")
                call check(nf90_put_att(ncid, diag_varattr_store%var_ids(var_index), attr_name, attr_value))
            else
                call error("Can't set attribute for a non-existent variable!" &
                    // char(10) &
                    // "             (If you did add the variable, make sure you lock" &
                    // char(10) &
                    // "             the definitions before calling varattr!) ")
            end if
        end subroutine nc_diag_varattr_short_v
        
        ! nc_diag_varattr - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_varattr_long_v(var_name, attr_name, attr_value)
            character(len=*), intent(in)               :: var_name
            character(len=*), intent(in)               :: attr_name
            integer(i_long), dimension(:), intent(in)  :: attr_value
            
            integer(i_long)                 :: var_index
            
            if (nc_diag_varattr_check_var(var_name)) then
                var_index = nc_diag_varattr_lookup_var(var_name)
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index for attr!")
                call check(nf90_put_att(ncid, diag_varattr_store%var_ids(var_index), attr_name, attr_value))
            else
                call error("Can't set attribute for a non-existent variable!" &
                    // char(10) &
                    // "             (If you did add the variable, make sure you lock" &
                    // char(10) &
                    // "             the definitions before calling varattr!) ")
            end if
        end subroutine nc_diag_varattr_long_v
        
        ! nc_diag_varattr - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_varattr_rsingle_v(var_name, attr_name, attr_value)
            character(len=*), intent(in)               :: var_name
            character(len=*), intent(in)               :: attr_name
            real(r_single), dimension(:), intent(in)   :: attr_value
            
            integer(i_long)                 :: var_index
            
            if (nc_diag_varattr_check_var(var_name)) then
                var_index = nc_diag_varattr_lookup_var(var_name)
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index for attr!")
                call check(nf90_put_att(ncid, diag_varattr_store%var_ids(var_index), attr_name, attr_value))
            else
                call error("Can't set attribute for a non-existent variable!" &
                    // char(10) &
                    // "             (If you did add the variable, make sure you lock" &
                    // char(10) &
                    // "             the definitions before calling varattr!) ")
            end if
        end subroutine nc_diag_varattr_rsingle_v
        
        ! nc_diag_varattr - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_varattr_rdouble_v(var_name, attr_name, attr_value)
            character(len=*), intent(in)               :: var_name
            character(len=*), intent(in)               :: attr_name
            real(r_double), dimension(:), intent(in)   :: attr_value
            
            integer(i_long)                 :: var_index
            
            if (nc_diag_varattr_check_var(var_name)) then
                var_index = nc_diag_varattr_lookup_var(var_name)
                if (var_index == -1) call error("Bug! Variable exists but could not lookup index for attr!")
                call check(nf90_put_att(ncid, diag_varattr_store%var_ids(var_index), attr_name, attr_value))
            else
                call error("Can't set attribute for a non-existent variable!" &
                    // char(10) &
                    // "             (If you did add the variable, make sure you lock" &
                    // char(10) &
                    // "             the definitions before calling varattr!) ")
            end if
        end subroutine nc_diag_varattr_rdouble_v
