module ncdr_check
    use kinds
    use ncdr_climsg
    use ncdr_types
    use ncdr_state
    use netcdf
    
    contains
        subroutine ncdr_check_ncdr_id(file_ncdr_id)
            integer(i_long), intent(in) :: file_ncdr_id
            
            if (file_ncdr_id > ncdr_file_count) &
                call error("The specified NCDR ID does not exist and/or is already closed!")
            
            if (.NOT. ncdr_files(file_ncdr_id)%file_open) &
                call error("The specified NCDR ID does not exist or is already closed! (Still in DB, but closed!)")
        end subroutine ncdr_check_ncdr_id
        
        subroutine ncdr_check_current_ncdr_id
            if (current_ncdr_id == -1) &
                call error("Current NCDR ID indicates that no files are open.")
            call ncdr_check_ncdr_id(current_ncdr_id)
        end subroutine ncdr_check_current_ncdr_id
        
        subroutine ncdr_check_ncid(file_ncid)
            integer(i_long), intent(in) :: file_ncid
            integer(i_long)             :: nc_err
            
            nc_err = nf90_inquire(file_ncid)
            
            if (nc_err == NF90_EBADID) &
                call error("The specified NCID does not exist and/or is already closed!")
            
            ! General error - something we can't handle!
            if (nc_err /= NF90_NOERR) &
                call check(nc_err)
        end subroutine ncdr_check_ncid
        
        subroutine ncdr_check_current_ncid
            call ncdr_check_current_ncdr_id
            call ncdr_check_ncid(ncdr_files(current_ncdr_id)%ncid)
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
        
        function nc_diag_read_get_index_from_filename(file_name) result(file_ind)
            character(len=*), intent(in)               :: file_name
            integer(i_long)                            :: i, file_ind
            
            if (ncdr_file_count == 0) then
                file_ind = -1
                return
            end if
            
            do i = 1, ncdr_file_count
                !write (*, "(A, I0, A, I0)") "File: " // ncdr_files(i)%filename // &
                !    " | Current NCID: ", ncdr_files(i)%ncid, &
                !    " | Target NCID: ", file_ncid
                
                if ((file_name == ncdr_files(i)%filename) .AND. (ncdr_files(i)%file_open)) then
                    file_ind = i
                    return
                end if
            end do
            
            file_ind = -1
        end function nc_diag_read_get_index_from_filename
        
        subroutine check(status)
          integer, intent ( in) :: status
          
          if(status /= nf90_noerr) then 
            call error(trim(nf90_strerror(status)))
          end if
        end subroutine check
end module ncdr_check
