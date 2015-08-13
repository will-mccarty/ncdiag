module nc_diag_res
    use kinds
    use ncdres_climsg
    use fson
    
    type(fson_value), pointer :: nc_diag_json => null()
    
    contains
        ! Opens a given resource file for reading.
        ! 
        ! Given the resource file name, open the file and set everything
        ! up for reading the file.
        ! 
        ! Args:
        !     filename (character(len=*): resource file name to load.
        ! 
        ! Returns:
        !     file_ncdr_id (integer(i_long)): internal nc_diag_read ID
        !         for use in other subroutines and functions. 
        ! 
        subroutine nc_diag_load_resource_file(filename)
            character(len=*), intent(in) :: filename
            
            if (associated(nc_diag_json)) &
                call error("Resource file already open!")
            
            nc_diag_json => fson_parse(filename)
        end subroutine nc_diag_load_resource_file
        
        ! Lookup a variable and check its status.
        ! 
        ! Given the variable name, lookup its status within the JSON
        ! resource file. If the variable is present in the JSON file,
        ! and if it is enabled, this will return true. Otherwise, if
        ! the variable doesn't exist in the resource file, or it is
        ! disabled, return false.
        ! 
        ! Args:
        !     filename (character(len=*): resource file name to load.
        ! 
        ! Returns:
        !     file_ncdr_id (integer(i_long)): internal nc_diag_read ID
        !         for use in other subroutines and functions. 
        ! 
        function nc_diag_load_check_variable(var_name) result(var_enabled)
            character(len=*), intent(in)  :: var_name
            logical                       :: var_enabled
            
            character(len=1024)           :: var_str
            
            write (var_str, "(A)") "variables." // var_name
            
            var_enabled = .FALSE.
            
            call fson_get(nc_diag_json, trim(var_str), var_enabled)
        end function nc_diag_load_check_variable
        
        ! Opens a given resource file for reading.
        ! 
        ! Given the resource file name, open the file and set everything
        ! up for reading the file.
        ! 
        ! Args:
        !     filename (character(len=*): resource file name to load.
        ! 
        ! Returns:
        !     file_ncdr_id (integer(i_long)): internal nc_diag_read ID
        !         for use in other subroutines and functions. 
        ! 
        subroutine nc_diag_close_resource_file
            if (associated(nc_diag_json)) then
                call fson_destroy(nc_diag_json)
                nullify(nc_diag_json)
            else
                call error("No resource file open!")
            end if
        end subroutine nc_diag_close_resource_file
end module nc_diag_res
