! nc_diag_write - NetCDF Layer Diag Writing Module
! Copyright 2015 Albert Huang - SSAI/NASA for NASA GSFC GMAO (610.1).
! 
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
! 
!   http://www.apache.org/licenses/LICENSE-2.0
! 
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
! implied. See the License for the specific language governing
! permissions and limitations under the License.
! 
! Main Module - nc_diag_write_mod
!

module nc_diag_write_mod
    ! Library that provides a high level interface for storing channel-
    ! based and observation-based data.
    ! 
    ! This library allows developers to easily store channel-based data
    ! (e.g. chaninfo) and observation-based data (metadata and data2d)
    ! to a NetCDF file via an easy to use API.
    ! 
    ! Internally, the process for storing this data looks like this:
    !   -> When the developer calls nc_diag_init, the NetCDF file is
    !      opened internally. The corresponding NCID is stored, and
    !      any memory allocation needed is done at this step.
    !      => If the file was opened in append mode, nc_diag_write will
    !         attempt to load any existing variable definitions for all
    !         types of variables - chaninfo, metadata, and data2d.
    !         Appropriate variable counters and data for each variable
    !         type will be set during init, and data writing will start
    !         at the end of the variable.
    !      
    !   -> Headers are essentially NetCDF global attributes, or
    !      attributes that describe a file. These can be added at any
    !      time during the writing session.
    !      
    !   -> varattr, or variable attributes, describe an associated
    !      variable. (This is a NetCDF4 variable attribute!) These can
    !      only be added after variable definitions have been locked.
    !      
    !   -> chaninfo variables:
    !      => nc_diag_chaninfo_dim_set must be called first to set
    !         the nchans dimension. If it isn't called, doing any
    !         chaninfo operation will result in an error.
    !      => chaninfo variables are 1D, with nchans number of elements.
    !      
    !   -> metadata and data2d variables:
    !      => metadata and data2d variables do not require any initial
    !         dimension setting - nc_diag_write will keep track of your
    !         number of observations for you!
    !      => metadata variables are 1D, with nobs number of elements.
    !         nobs can increase infinitely to fit the number of
    !         observations recorded.
    !      => data2d variables are 2D, with dimensions of nobs by
    !         another fixed dimension.
    !      
    !   -> Data calls will store the input data into memory. The
    !      implementation and design of the variable storage is
    !      dependent on the variable type being stored. chaninfo
    !      variables have a certain storage format, and metadata/data2d
    !      variables have another storage format. Note that metadata
    !      and data2d code have a few similarities in data storage
    !      since the variables themselves share common features, like
    !      the nobs dimension.
    !      
    !   -> Once data is done being queued ("stored"), nc_diag_write is
    !      called. The variables will have their data re-read from
    !      memory and actually written to the file. This is also very
    !      much variable type independent, since every variable has its
    !      own way of storing variable data. Again, metadata and data2d
    !      have similar code, with the only difference being the
    !      dimensionality. Note that this is where NetCDF calls are
    !      made to define and "put" data.
    !      
    !   -> Once all the data has been written out, it is safe to call
    !      nc_diag_finish. This will close the NetCDF file being
    !      written. Note that NetCDF also keeps a cache of data being
    !      stored in memory as well, so actual I/O writing may not
    !      be completely done until here.
    
    ! Load state variables
    use ncdw_state, only: init_done, append_only, ncid, &
        enable_trim, cur_nc_file, &
        diag_chaninfo_store, diag_metadata_store, diag_data2d_store, &
        diag_varattr_store
    
    ! Load needed NetCDF functions and constants
    use netcdf, only: nf90_inq_libvers, nf90_open, nf90_create, &
        nf90_enddef, nf90_close, nf90_sync, &
        NF90_WRITE, NF90_NETCDF4, NF90_CLOBBER
    
    !------------------------------------------------------------------
    ! API imports to expose API from this module
    ! (Plus general imports for this module as well!)
    !------------------------------------------------------------------
    use ncdw_climsg, only: &
#ifdef ENABLE_ACTION_MSGS
        nclayer_enable_action, nclayer_actionm, &
#endif
        nclayer_error, nclayer_warning, nclayer_info, nclayer_check, &
        nc_set_info_display, nc_set_action_display
    
    use ncdw_types, only: NLAYER_BYTE, NLAYER_SHORT, NLAYER_LONG, &
        NLAYER_FLOAT, NLAYER_DOUBLE, NLAYER_STRING
    
    use ncdw_lheader, only: nc_diag_header
    
    use ncdw_chaninfo, only: nc_diag_chaninfo_dim_set, &
        nc_diag_chaninfo, &
        nc_diag_chaninfo_load_def, nc_diag_chaninfo_write_def, &
        nc_diag_chaninfo_write_data, &
        nc_diag_chaninfo_set_strict, &
        nc_diag_chaninfo_allocmulti, nc_diag_chaninfo_prealloc_vars, &
        nc_diag_chaninfo_prealloc_vars_storage
    
    use ncdw_metadata, only: nc_diag_metadata, &
        nc_diag_metadata_load_def, nc_diag_metadata_write_def, &
        nc_diag_metadata_write_data, &
        nc_diag_metadata_set_strict, &
        nc_diag_metadata_allocmulti, &
        nc_diag_metadata_prealloc_vars, &
        nc_diag_metadata_prealloc_vars_storage, &
        nc_diag_metadata_prealloc_vars_storage_all
    
    use ncdw_data2d, only: nc_diag_data2d, &
        nc_diag_data2d_load_def, nc_diag_data2d_write_def, &
        nc_diag_data2d_write_data, &
        nc_diag_data2d_set_strict, &
        nc_diag_data2d_allocmulti, &
        nc_diag_data2d_prealloc_vars, &
        nc_diag_data2d_prealloc_vars_storage, &
        nc_diag_data2d_prealloc_vars_storage_all
    
    use ncdw_varattr, only: nc_diag_varattr
    
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
            
            call nclayer_info('Initializing netcdf layer library, version ' // trim(nf90_inq_libvers()) // '...')
            
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
        
        ! Flush all of the current variable data to NetCDF, and reset
        ! all of the variable storage to an initial state.
        ! 
        ! Attempt to write the currently stored variable definitions
        ! and data to the NetCDF file via NetCDF API calls.
        ! 
        ! Once done, this will effectively "flush" the data from the
        ! current variable buffers. Internally, this sets a starting
        ! counter and resets the buffer counter so that new data can
        ! be stored sequentially without requiring more memory, at least
        ! until memory runs out for the current buffer.
        ! 
        ! Definitions MUST be locked in order for flushing to work.
        ! Without definition locking, nc_diag_write is unable to make
        ! calls to NetCDF due to the lack of variable IDs.
        ! 
        ! If definitions are not locked, calling this will result in an
        ! error.
        ! 
        ! Data locking does NOT occur with flushing. As a result, this
        ! subroutine may be called multiple times, and a final
        ! nc_diag_write can be called once after this call.
        ! 
        ! (Note that calling nc_diag_write will lock the data and close
        ! the file, regardless of flushing the buffer here!)
        ! 
        ! Args:
        !     None
        ! 
        ! Raises:
        !     If definitions have not been locked, this will result in
        !     an error.
        !     
        !     The following errors will trigger indirectly from other
        !     subroutines called here:
        !     
        !     If the variable data writing has already been locked, this
        !     will result in an error.
        !     
        !     If there is no file open (or the file is already closed),
        !     this will result in an error.
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
        subroutine nc_diag_flush_buffer
#ifdef ENABLE_ACTION_MSGS
            if (nclayer_enable_action) then
                call nclayer_actionm("nc_diag_flush_buffer()")
            end if
#endif
            if (.NOT. init_done) &
                call nclayer_error("Attempted to flush nc_diag_write buffers without initializing!")
            
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
            ! Make sure we have something open + initialized
            if (.NOT. init_done) &
                call nclayer_error("Attempted to flush NetCDF buffers without initializing!")
            
            ! Call nf90_sync to try and commit the put'd data to disk
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
end module nc_diag_write_mod
