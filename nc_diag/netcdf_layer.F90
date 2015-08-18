module netcdf_layer
    use nclayer_state, only: init_done, append_only, ncid, &
        enable_trim, cur_nc_file, &
        diag_chaninfo_store, diag_metadata_store, diag_data2d_store, &
        diag_varattr_store
    use netcdf, only: nf90_inq_libvers, nf90_open, nf90_create, &
        nf90_enddef, nf90_close, nf90_sync, &
        NF90_WRITE, NF90_NETCDF4, NF90_CLOBBER
    
    !------------------------------------------------------------------
    ! API imports to expose API from this module
    !------------------------------------------------------------------
    use nclayer_climsg, only: &
#ifdef ENABLE_ACTION_MSGS
        nclayer_enable_action, nclayer_actionm, &
#endif
        nclayer_error, nclayer_warning, nclayer_info, nclayer_check, &
        nc_set_info_display, nc_set_action_display
    
    use nclayer_types, only: NLAYER_BYTE, NLAYER_SHORT, NLAYER_LONG, &
        NLAYER_FLOAT, NLAYER_DOUBLE, NLAYER_STRING
    
    use nclayer_lheader, only: nc_diag_header
    
    use nclayer_chaninfo, only: nc_diag_chaninfo_dim_set, &
        nc_diag_chaninfo, &
        nc_diag_chaninfo_load_def, nc_diag_chaninfo_write_def, &
        nc_diag_chaninfo_write_data, &
        nc_diag_chaninfo_set_strict, &
        nc_diag_chaninfo_allocmulti, nc_diag_chaninfo_prealloc_vars, &
        nc_diag_chaninfo_prealloc_vars_storage
    
    use nclayer_metadata, only: nc_diag_metadata, &
        nc_diag_metadata_load_def, nc_diag_metadata_write_def, &
        nc_diag_metadata_write_data, &
        nc_diag_metadata_set_strict, &
        nc_diag_metadata_allocmulti, &
        nc_diag_metadata_prealloc_vars, &
        nc_diag_metadata_prealloc_vars_storage, &
        nc_diag_metadata_prealloc_vars_storage_all
    
    use nclayer_data2d, only: nc_diag_data2d, &
        nc_diag_data2d_load_def, nc_diag_data2d_write_def, &
        nc_diag_data2d_write_data, &
        nc_diag_data2d_set_strict, &
        nc_diag_data2d_allocmulti, &
        nc_diag_data2d_prealloc_vars, &
        nc_diag_data2d_prealloc_vars_storage, &
        nc_diag_data2d_prealloc_vars_storage_all
    
    use nclayer_varattr, only: nc_diag_varattr
    
    implicit none
    
    contains
        subroutine nc_diag_init(filename, append)
            character(len=*),intent(in)    :: filename
            logical, intent(in), optional  :: append
            
            integer                        :: bsize = 16777216;
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                if (present(append)) then
                    write(action_str, "(A, L, A)") "nc_diag_init(filename = " // trim(filename) // &
                        ", append = ", append, ")"
                else
                    write(action_str, "(A)") "nc_diag_init(filename = " // trim(filename) // &
                        ", append = (not specified))"
                end if
                call nclayer_actionm(trim(action_str))
            end if
#endif
            
            !print *,'Initializing netcdf layer library, version ...'
            write (*,"(A, A, A)") 'Initializing netcdf layer library, version ', trim(nf90_inq_libvers()), '...'
            
            ! nf90_create creates the NetCDF file, and initializes
            ! everything needed to write a NetCDF file.
            ! 
            ! NF90_CLOBBER forces overwriting the file, even if it already
            ! exists.
            ! 
            ! ncid is a special ID that the NetCDF library uses to keep
            ! track of what file you're working on. We're returning that
            ! here.
            if (.NOT. init_done) then
                ! Special append mode - that means that we need to
                ! assume that all definitions are set and locked.
                if (present(append) .AND. (append == .TRUE.)) then
                    call nclayer_check( nf90_open(filename, NF90_WRITE, ncid, &
                        bsize, cache_nelems = 16777216) ) ! Optimization settings
                    append_only = .TRUE.
                    call nclayer_warning("NetCDF file opened in append mode - definitions will be locked.")
                else
                    call nclayer_check( nf90_create(filename, OR(NF90_NETCDF4, NF90_CLOBBER), ncid, &
                        0, bsize, cache_nelems = 16777216) ) ! Optimization settings
                end if
                
                if (allocated(diag_chaninfo_store)) then
                    call nclayer_error("BUG! diag_chaninfo_store is allocated, but init_done is not set!")
                end if
                
                if (allocated(diag_metadata_store)) then
                    call nclayer_error("BUG! diag_metadata_store is allocated, but init_done is not set!")
                end if
                
                if (allocated(diag_data2d_store)) then
                    call nclayer_error("BUG! diag_data2d_store is allocated, but init_done is not set!")
                end if
                
                if (allocated(diag_varattr_store)) then
                    call nclayer_error("BUG! diag_data2d_store is allocated, but init_done is not set!")
                end if
                
                allocate(diag_chaninfo_store)
                allocate(diag_metadata_store)
                allocate(diag_data2d_store)
                allocate(diag_varattr_store)
                
                write (*,"(A, I0, A)") 'NetCDF will use ', bsize, ' bytes of cache.'
                
                cur_nc_file = filename
                
                init_done = .TRUE.
                
                ! "Lock" the definitions... or simply ask chaninfo/metadata/data2d
                ! to read the NetCDF files and build a cache.
                if (present(append) .AND. (append == .TRUE.)) then
                    call nclayer_info("Loading chaninfo variables/dimensions from file:")
                    call nc_diag_chaninfo_load_def
                    
                    call nclayer_info("Loading metadata variables/dimensions from file:")
                    call nc_diag_metadata_load_def
                    
                    call nclayer_info("Loading data2d variables/dimensions from file:")
                    call nc_diag_data2d_load_def
                end if
            else
                call nclayer_error("Attempted to initialize without closing previous nc_diag file!" &
                    // char(10) &
                    // "             (Previous file: " // trim(cur_nc_file) &
                    // char(10) &
                    // "              Attempted to open file: " // trim(filename) // ")")
            end if
        end subroutine nc_diag_init
        
        !subroutine nc_diag_metadata
        !    
        !end subroutine nc_diag_metadata
        !
        !subroutine nc_diag_data(data_name, data_index, data)
        !    
        !end subroutine nc_diag_data
        
        subroutine nc_diag_lock_def
#ifdef ENABLE_ACTION_MSGS
            if (nclayer_enable_action) then
                call nclayer_actionm("nc_diag_lock_def()")
            end if
#endif
            call nclayer_info("Locking all variable definitions!")
            
            call nclayer_info("Defining chaninfo:")
            call nc_diag_chaninfo_write_def
            
            call nclayer_info("Defining metadata:")
            call nc_diag_metadata_write_def
            
            call nclayer_info("Defining data2d:")
            call nc_diag_data2d_write_def
            
            call nclayer_info("All variable definitions locked!")
        end subroutine nc_diag_lock_def
        
        subroutine nc_diag_write
#ifdef ENABLE_ACTION_MSGS
            if (nclayer_enable_action) then
                call nclayer_actionm("nc_diag_write()")
            end if
#endif
            
            call nclayer_info("Defining chaninfo:")
            call nc_diag_chaninfo_write_def(.TRUE.)
            
            call nclayer_info("Defining metadata:")
            call nc_diag_metadata_write_def(.TRUE.)
            
            call nclayer_info("Defining data2d:")
            call nc_diag_data2d_write_def(.TRUE.)
            
            ! Lock definition writing!
            if ((.NOT. append_only) .AND. ((.NOT. diag_chaninfo_store%def_lock) .OR. &
                (.NOT. diag_metadata_store%def_lock) .OR. &
                (.NOT. diag_data2d_store%def_lock))) & &
                call nclayer_check(nf90_enddef(ncid))
            
            call nclayer_info("Writing chaninfo:")
            call nc_diag_chaninfo_write_data
            
            call nclayer_info("Writing metadata:")
            call nc_diag_metadata_write_data
            
            call nclayer_info("Writing data2d:")
            call nc_diag_data2d_write_data
            
            call nclayer_info("All done queuing in data, letting NetCDF take over!")
            call nclayer_check(nf90_close(ncid))
            
            call nclayer_info("All done!")
            
            call nc_diag_finish
        end subroutine nc_diag_write
        
        subroutine nc_diag_finish
#ifdef ENABLE_ACTION_MSGS
            if (nclayer_enable_action) then
                call nclayer_actionm("nc_diag_finish()")
            end if
#endif
            if (init_done) then
                call nclayer_info("Cleaning up...")
                if (.NOT. allocated(diag_chaninfo_store)) then
                    call nclayer_error("BUG! diag_chaninfo_store is not allocated, but init_done is set!")
                end if
                
                if (.NOT. allocated(diag_metadata_store)) then
                    call nclayer_error("BUG! diag_metadata_store is not allocated, but init_done is set!")
                end if
                
                if (.NOT. allocated(diag_data2d_store)) then
                    call nclayer_error("BUG! diag_data2d_store is not allocated, but init_done is set!")
                end if
                
                if (.NOT. allocated(diag_varattr_store)) then
                    call nclayer_error("BUG! diag_data2d_store is not allocated, but init_done is set!")
                end if
                
                deallocate(diag_chaninfo_store)
                deallocate(diag_metadata_store)
                deallocate(diag_data2d_store)
                deallocate(diag_varattr_store)
                
                init_done = .FALSE.
                append_only = .FALSE.
                cur_nc_file = ""
            else
                call nclayer_error("Attempted to deallocate without initializing!")
            end if
        end subroutine nc_diag_finish
        
        subroutine nc_diag_flush_buffer
#ifdef ENABLE_ACTION_MSGS
            if (nclayer_enable_action) then
                call nclayer_actionm("nc_diag_flush_buffer()")
            end if
#endif
            if ((.NOT. diag_chaninfo_store%def_lock) .OR. &
                (.NOT. diag_metadata_store%def_lock) .OR. &
                (.NOT. diag_data2d_store%def_lock)) &
                call nclayer_error("Definitions must be locked in order to flush the buffer!")
            
            ! Perform writes with the buffer flag set!
            call nclayer_info("Flushing chaninfo:")
            call nc_diag_chaninfo_write_data(.TRUE.)
            
            call nclayer_info("Flushing metadata:")
            call nc_diag_metadata_write_data(.TRUE.)
            
            call nclayer_info("Flushing data2d:")
            call nc_diag_data2d_write_data(.TRUE.)
            
            call nclayer_info("Flushing done!")
        end subroutine nc_diag_flush_buffer
        
        subroutine nc_diag_flush_to_file
#ifdef ENABLE_ACTION_MSGS
            if (nclayer_enable_action) then
                call nclayer_actionm("nc_diag_flush_to_file()")
            end if
#endif
            call nclayer_check(nf90_sync(ncid))
        end subroutine nc_diag_flush_to_file
        
        subroutine nc_diag_set_strict(enable_strict)
            logical, intent(in) :: enable_strict
            
            if (init_done) then
                call nc_diag_chaninfo_set_strict(enable_strict)
                call nc_diag_metadata_set_strict(enable_strict)
                call nc_diag_data2d_set_strict(enable_strict)
            else
                call nclayer_error("Can't set strictness level - NetCDF4 layer not initialized yet!")
            end if
        end subroutine nc_diag_set_strict
        
        subroutine nc_diag_set_trim(do_trim)
            logical, intent(in) :: do_trim
            
            enable_trim = do_trim
        end subroutine nc_diag_set_trim
end module netcdf_layer
