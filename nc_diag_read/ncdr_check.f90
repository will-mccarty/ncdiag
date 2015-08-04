module ncdr_check
    use kinds
    use ncdr_climsg
    use ncdr_types
    use ncdr_state
    use netcdf
    
    contains
        subroutine ncdr_check_ncid(file_ncid)
            integer(i_long), intent(in) :: file_ncid
            
            if (nc_diag_read_get_index_from_ncid(file_ncid) == -1) &
                call error("The specified NCID does not exist or is already closed!")
            
            if (.NOT. ncdr_files(&
                nc_diag_read_get_index_from_ncid(file_ncid) &
                )%file_open) &
                call error("The specified NCID does not exist or is already closed! (Still in DB, but closed!)")
        end subroutine ncdr_check_ncid
        
        subroutine ncdr_check_current_ncid
            if (current_ind == -1) &
                call error("No NetCDF files have been opened yet (or none are open at the moment). Note that there may be other files open, but they are not tracked here.")
            
            if (.NOT. ncdr_files(current_ind)%file_open) &
                call error("The current NCID has already been closed!")
        end subroutine ncdr_check_current_ncid
        
        function nc_diag_read_get_index_from_ncid(file_ncid) result(file_ind)
            integer(i_long), intent(in)                :: file_ncid
            integer(i_long)                            :: i, file_ind
            
            if (ncdr_file_count == 0) then
                file_ind = -1
                return
            end if
            
            do i = 1, ncdr_file_count
                !write (*, "(A, I0, A, I0)") "File: " // ncdr_files(i)%filename // &
                !    " | Current NCID: ", ncdr_files(i)%ncid, &
                !    " | Target NCID: ", file_ncid
                
                if ((file_ncid == ncdr_files(i)%ncid) .AND. (ncdr_files(i)%file_open)) then
                    file_ind = i
                    return
                end if
            end do
            
            file_ind = -1
        end function nc_diag_read_get_index_from_ncid
        
        subroutine check(status)
          integer, intent ( in) :: status
          
          if(status /= nf90_noerr) then 
            call error(trim(nf90_strerror(status)))
          end if
        end subroutine check
end module ncdr_check
