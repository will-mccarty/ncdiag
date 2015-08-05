module nc_diag_read
    use netcdf
    use kinds
    use ncdr_climsg
    use ncdr_types
    use ncdr_realloc_mod
    use netcdf_unlimdims
    use ncdr_state
    use ncdr_check
    
    use ncdr_dims
    use ncdr_vars
    
    !use utils
    implicit none
    
#define INITIAL_SIZE 1024
#define NCDR_MULTI_BASE 1
    
    contains
        subroutine nc_diag_read_parse_file(filename, file_ncid)
            character(len=*),intent(in)                :: filename
            integer(i_long), intent(in)                :: file_ncid
            
            integer(i_long)                            :: input_ndims
            integer(i_long)                            :: input_nvars
            integer(i_long)                            :: input_nattrs
            
            ncdr_file_count = ncdr_file_count + 1
            
            if (allocated(ncdr_files)) then
                if (ncdr_file_count > ncdr_file_total) then
                    call ncdr_realloc(ncdr_files, ncdr_file_total * NCDR_MULTI_BASE)
                end if
            else
                allocate(ncdr_files(NCDR_DEFAULT_ENT))
            end if
            
            ncdr_files(ncdr_file_count)%filename = filename
            ncdr_files(ncdr_file_count)%ncid     = file_ncid
            
            ! Get top level info about the file!
            call check(nf90_inquire(file_ncid, nDimensions = input_ndims, &
                nVariables = input_nvars, nAttributes = input_nattrs))
            
            call nc_diag_read_parse_file_dims(file_ncid, ncdr_file_count, input_ndims)
            call nc_diag_read_parse_file_vars(file_ncid, ncdr_file_count, input_nvars)
            
            ! Make sure file is now open!
            ncdr_files(ncdr_file_count)%file_open = .TRUE.
            
            ! Update highest record - this will let us keep track and
            ! help us clear memory when we can!
            if (ncdr_file_count > ncdr_file_highest) then
                ncdr_file_highest = ncdr_file_count
            end if
        end subroutine nc_diag_read_parse_file
        
        function nc_diag_read_id_init(filename) result(file_ncid)
            character(len=*),intent(in)    :: filename
            
            integer                        :: file_ncid
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)            :: action_str
            
            if (enable_action) then
                write(action_str, "(A)") "nc_diag_init(filename = " // trim(filename) // &
                    ", append = (not specified))"
                call actionm(trim(action_str))
            end if
#endif
            
            if (nc_diag_read_get_index_from_filename(filename) /= -1) &
                call error("Can't open the same file more than once! (Opening, closing, and then opening again is allowed.)")
            
            write (*,"(A, A, A)") 'Initializing netcdf layer library, version ', trim(nf90_inq_libvers()), '...'
            
            call check( nf90_open(filename, NF90_NOWRITE, file_ncid) )
            
            call nc_diag_read_parse_file(filename, file_ncid)
        end function nc_diag_read_id_init
        
        subroutine nc_diag_read_init(filename, file_ncid, from_push)
            character(len=*),intent(in)            :: filename
            integer(i_long), intent(out), optional :: file_ncid
            logical,         intent(in),  optional :: from_push
            integer(i_long)                        :: f_ncid
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                    :: action_str
            
            if (enable_action) then
                write(action_str, "(A)") "nc_diag_read_init(filename = " // trim(filename) // &
                    ", append = (not specified))"
                call actionm(trim(action_str))
            end if
#endif
            
            if (ncid_stack_count > 0) then
                if (.NOT. (present(from_push) .AND. (from_push))) &
                    call error("Can not initialize due to push/pop queue use! If you want to init without the stack, you must use nc_diag_read_id_init or clear the queue first!")
            end if
            
            f_ncid = nc_diag_read_id_init(filename)
            
            if (present(file_ncid)) &
                file_ncid = f_ncid
            
            ! Set current ncid
            current_ncid = f_ncid
            current_ind = nc_diag_read_get_index_from_ncid(f_ncid)
        end subroutine nc_diag_read_init
        
        subroutine nc_diag_read_push(filename, file_ncid)
            character(len=*),intent(in)            :: filename
            integer(i_long), intent(out), optional :: file_ncid
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                    :: action_str
            
            if (enable_action) then
                write(action_str, "(A)") "nc_diag_read_push(filename = " // trim(filename) // &
                    ", append = (not specified))"
                call actionm(trim(action_str))
            end if
#endif
            
            if ((ncid_stack_count == 0) .AND. (current_ncid /= -1)) &
                call error("Can not initialize due to normal caching use! If you want to init with the stack, you must close the cached file first, then use nc_diag_read_push()!")
            
            ncid_stack_count = ncid_stack_count + 1
            
            if (allocated(ncid_stack)) then
                if (ncid_stack_count >= ncid_stack_size) then
                    call ncdr_realloc(ncid_stack, size(ncid_stack))
                    call ncdr_realloc(ind_stack, size(ind_stack))
                    ncid_stack_size = size(ncid_stack)
                end if
            else
                allocate(ncid_stack(INITIAL_SIZE))
                allocate(ind_stack(INITIAL_SIZE))
                ncid_stack_size = size(ncid_stack)
            end if
            
            if (present(file_ncid)) then
                call nc_diag_read_init(filename, file_ncid, .TRUE.)
            else
                call nc_diag_read_init(filename, from_push = .TRUE.)
            end if
            
            ! Push new NCID to stack
            ncid_stack(ncid_stack_count) = current_ncid
            ind_stack(ncid_stack_count) = nc_diag_read_get_index_from_ncid(current_ncid)
        end subroutine nc_diag_read_push
        
        subroutine nc_diag_read_close(filename, file_ncid, from_pop)
            character(len=*),intent(in), optional  :: filename
            integer(i_long), intent(in), optional  :: file_ncid
            logical,         intent(in), optional  :: from_pop
            
            integer(i_long)                        :: f_ncid, f_ind, i
            logical                                :: range_closed
            
            f_ncid = -1
            
            if (ncdr_file_count == 0) &
                call error("No files are currently open!")
            
            if (ncid_stack_count > 0) then
                if ((any(ncid_stack == file_ncid)) .AND. (.NOT. (present(from_pop) .AND. (from_pop)))) &
                    call error("Can not close due to push/pop queue use! If you want to use this without the stack, you must use nc_diag_read_id_init or clear the queue first!")
            end if
            
            if (present(filename)) then
                f_ind = nc_diag_read_get_index_from_filename(filename)
                
                if (f_ind == -1) &
                    call error("The NetCDF file specified, " // filename // ", is not open and can't be closed.")
                
                f_ncid = ncdr_files(f_ind)%ncid
            else if (present(file_ncid)) then
                ! Do... nothing. Just store the ncid.
                f_ncid = file_ncid
            else
                ! Try to see if current_ncid is defined
                if (current_ncid == -1) &
                    call error("No arguments specified for closing a file! (Also, no current NCIDs were found!)")
                f_ncid = current_ncid
            end if
            
            ! Sanity check
            call ncdr_check_ncid(f_ncid)
            
            ! Close it!
            call check(nf90_close(f_ncid))
            
            ! Deactivate entry...
            f_ind = nc_diag_read_get_index_from_ncid(f_ncid)
            ncdr_files(f_ind)%file_open = .FALSE.
            
            ! Deallocate as much as possible!
            deallocate(ncdr_files(f_ind)%dims)
            deallocate(ncdr_files(f_ind)%vars)
            
            ! Set current_ncid to -1, as necessary:
            if (current_ncid == f_ncid) then
                current_ncid = -1
                current_ind = -1
            end if
            
            ! Update highest record - this will let us keep track and
            ! help us clear memory when we can!
            range_closed = .TRUE.
            
            !print *, "UPDATE:      f_ind, ncdr_file_count, ncdr_file_highest"
            !print *, "PREUPDATE:", f_ind, ncdr_file_count, ncdr_file_highest
            
            if (f_ind < ncdr_file_highest) then
                do i = f_ind, ncdr_file_highest
                    if (ncdr_files(i)%file_open) then
                        range_closed = .FALSE.
                        exit
                    end if
                end do
                
                if (range_closed) then
                    ncdr_file_highest = f_ind
                    ncdr_file_count = f_ind
                end if
            else if (f_ind == ncdr_file_highest) then
                ncdr_file_highest = f_ind - 1
                ncdr_file_count = f_ind - 1
                
                do i = 1, ncdr_file_highest
                    if (ncdr_files(i)%file_open) then
                        range_closed = .FALSE.
                        exit
                    end if
                end do
                
                if (range_closed) then
                    ncdr_file_highest = 0
                    ncdr_file_count = 0
                end if
            end if
            
            !print *, "POSTUPDATE:", f_ind, ncdr_file_count, ncdr_file_highest
        end subroutine nc_diag_read_close
        
        ! Pop - we return the thing we just deleted, and push things up!
        subroutine nc_diag_read_pop(filename, file_ncid)
            character(len=*),intent(out), optional :: filename
            integer(i_long), intent(out), optional :: file_ncid
            
            if (ncid_stack_count == 0) &
                call error("No NetCDF files to pop!")
            
            if (current_ncid /= ncid_stack(ncid_stack_count)) &
                call error("BUG - current NCID differs from the current queued NCID!")
            
            if (present(filename)) then
                filename = ncdr_files(ncid_stack(ncid_stack_count))%filename
            end if
            
            if (present(file_ncid)) then
                file_ncid = ncid_stack(ncid_stack_count)
            end if
            
            ! Close the file
            call nc_diag_read_close(file_ncid = ncid_stack(ncid_stack_count), from_pop = .TRUE.)
            
            ! Set the stack spot to -1...
            ncid_stack(ncid_stack_count) = -1
            
            ! ...and decrease the count, effectively "popping" it!
            ncid_stack_count = ncid_stack_count - 1
            
            ! If everything is gone, set current to -1.
            if (ncid_stack_count /= 0) then
                current_ind  = ind_stack(ncid_stack_count)
                current_ncid = ncid_stack(ncid_stack_count)
            else
                current_ind = -1
                current_ncid = -1
            end if
        end subroutine nc_diag_read_pop
end module nc_diag_read
