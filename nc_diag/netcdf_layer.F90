module netcdf_layer
#ifndef NO_NETCDF
    use netcdf
#else
#warning NetCDF library usage disabled. Filler routines will be used.
#endif
    use fnv32mod
    use kinds
    use utils
    implicit none
    
    logical,dimension(:),allocatable        :: def_locked
    real(r_single),dimension(:),allocatable :: bifix
    !real(r_single),dimension(:),allocatable    :: bifix
    
    ! NetCDF4 type struct constants
    integer(i_byte), parameter              :: NLAYER_BYTE   = 1
    integer(i_byte), parameter              :: NLAYER_SHORT  = 2
    integer(i_byte), parameter              :: NLAYER_LONG   = 3
    integer(i_byte), parameter              :: NLAYER_FLOAT  = 4
    integer(i_byte), parameter              :: NLAYER_DOUBLE = 5
    integer(i_byte), parameter              :: NLAYER_STRING = 6
    
    ! Default number of starting entries
    integer(i_short), parameter             :: NLAYER_DEFAULT_ENT = 1024
    
    ! NetCDF zlib (/gzip) compression level
    integer(i_byte), parameter              :: NLAYER_COMPRESSION = 9
    
    ! NetCDF chunking size
    integer(i_long), parameter              :: NLAYER_CHUNKING = 16384
    
    ! Base used when exponentiated.
#define NLAYER_MULTI_BASE 2
    
    integer :: ncid
    logical :: init_done = .FALSE.
    logical :: header_locked = .FALSE.
    
    logical :: enable_info = .FALSE.
    logical :: enable_action = .FALSE.
    
#ifndef IGNORE_VERSION
    logical :: NLAYER_STRING_BROKEN = .FALSE.
#endif
    
#ifdef NO_NETCDF
#include "netcdf_nonetcdf_decl.F90"
#endif

#include "netcdf_realloc_decl.f90"
#include "netcdf_lheader_decl.f90"
#include "netcdf_chaninfo_decl.F90"
#include "netcdf_metadata_decl.f90"
#include "netcdf_data2d_decl.f90"
#include "netcdf_varattr_decl.F90"
    
    contains
#ifdef NO_NETCDF
#include "netcdf_nonetcdf_imp.F90"
#endif
        
#include "netcdf_realloc_imp.f90"
#include "netcdf_ciresize.F90"
#include "netcdf_mresize.F90"
#include "netcdf_dresize.F90"
#include "netcdf_lheader_imp.F90"
#include "netcdf_chaninfo_imp.F90"
#include "netcdf_metadata_imp.F90"
#include "netcdf_data2d_imp.F90"
#include "netcdf_varattr_imp.F90"
        
        subroutine nc_diag_init(filename)
            character(len=*),intent(in)    :: filename
            character(len=:), allocatable  :: version_num
            
            integer                        :: bsize = 16777216;
            
#ifdef ENABLE_ACTION_MSGS
            if (enable_action) then
                call actionm("nc_diag_init(filename = " // trim(filename) // ")")
            end if
#endif
            
#ifndef NO_NETCDF
            !print *,'Initializing netcdf layer library, version ...'
            write (*,"(A, A, A)") 'Initializing netcdf layer library, version ', trim(nf90_inq_libvers()), '...'
            call string_before_delimiter(trim(nf90_inq_libvers()), " ", version_num)
            
#ifndef IGNORE_VERSION
            call nc_version_check(version_num)
#endif

#endif
            
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
#ifndef NO_NETCDF
                call check( nf90_create(filename, OR(NF90_NETCDF4, NF90_CLOBBER), ncid, &
                    0, bsize, cache_nelems = 16777216) ) ! Optimization settings
#endif
                
                if (allocated(diag_chaninfo_store)) then
                    call error("BUG! diag_chaninfo_store is allocated, but init_done is not set!")
                end if
                
                if (allocated(diag_metadata_store)) then
                    call error("BUG! diag_metadata_store is allocated, but init_done is not set!")
                end if
                
                if (allocated(diag_data2d_store)) then
                    call error("BUG! diag_data2d_store is allocated, but init_done is not set!")
                end if
                
                if (allocated(diag_varattr_store)) then
                    call error("BUG! diag_data2d_store is allocated, but init_done is not set!")
                end if
                
                allocate(diag_chaninfo_store)
                allocate(diag_metadata_store)
                allocate(diag_data2d_store)
                allocate(diag_varattr_store)
                
                write (*,"(A, I0, A)") 'NetCDF will use ', bsize, ' bytes of cache.'
                
                init_done = .TRUE.
            else
                call error("Attempted to initialize without closing previous nc_diag file!")
            end if
        end subroutine nc_diag_init
        
#ifndef IGNORE_VERSION
        subroutine nc_version_check(version_num)
            character(len=*),intent(in)    :: version_num
            character(len=:), allocatable  :: version_split_char(:)
            
            integer(i_long), dimension(4)  :: version_split
            
            version_split = 0
            
            ! There are problems with a certain version of NetCDF,
            ! particularly with string handling. When attempting to get
            ! and put string data, NC_EMAPTYPE (Mapped access for
            ! atomic types only) appears, preventing string storage from
            ! working. We can work around this, but we need to know the
            ! version in order to know whether we need the workaround
            ! or not.
            ! 
            ! Affected versions are 4.2.1.1 and lower.
            
            version_split_char = string_split_index(version_num, ".")
            
            ! Convert to integers!
            if (size(version_split_char) >= 1) read(version_split_char(1), "(I)") version_split(1)
            if (size(version_split_char) >= 2) read(version_split_char(2), "(I)") version_split(2)
            if (size(version_split_char) >= 3) read(version_split_char(3), "(I)") version_split(3)
            if (size(version_split_char) >= 4) read(version_split_char(4), "(I)") version_split(4)
            
            ! Now compare!
            if ((version_split(1) <= 4) .AND. (version_split(2) <= 2) &
                .AND. (version_split(3) <= 2) .AND. (version_split(4) <= 1)) then
                NLAYER_STRING_BROKEN = .TRUE.
                call warning("Detected buggy version (<= v4.2.1.1) of NetCDF with bad string" &
                    // char(10) &
                    // "             data handling. Any string data input will result in an error." &
                    // char(10) &
                    // "             You can use the NLAYER_STRING_BROKEN variable to determine if" &
                    // char(10) &
                    // "             strings are usable or not within this module." &
                    // char(10) &
                    // "             (To ignore version, rebuild with IGNORE_VERSION defined.)")
            end if
        end subroutine nc_version_check
#endif
        
        !subroutine nc_diag_metadata
        !    
        !end subroutine nc_diag_metadata
        !
        !subroutine nc_diag_data(data_name, data_index, data)
        !    
        !end subroutine nc_diag_data
        
        subroutine nc_diag_lock_def
#ifdef ENABLE_ACTION_MSGS
            if (enable_action) then
                call actionm("nc_diag_lock_def()")
            end if
#endif
            call info("Locking all variable definitions!")
#ifndef NO_NETCDF
            call info("Defining chaninfo:")
            call nc_diag_chaninfo_write_def
            
            call info("Defining metadata:")
            call nc_diag_metadata_write_def
            
            call info("Defining data2d:")
            call nc_diag_data2d_write_def
#else
            call warning("NetCDF support is disabled, so defintions will not be" &
                    // char(10) &
                    // "             locked. Any functions that depend on definition locking will" &
                    // char(10) &
                    // "             not work!")
#endif
            call info("All variable definitions locked!")
        end subroutine nc_diag_lock_def
        
        subroutine nc_diag_write
#ifdef ENABLE_ACTION_MSGS
            if (enable_action) then
                call actionm("nc_diag_write()")
            end if
#endif
#ifndef NO_NETCDF
            call info("Defining chaninfo:")
            call nc_diag_chaninfo_write_def(.TRUE.)
            
            call info("Defining metadata:")
            call nc_diag_metadata_write_def(.TRUE.)
            
            call info("Defining data2d:")
            call nc_diag_data2d_write_def(.TRUE.)
            
            ! Lock definition writing!
            call check(nf90_enddef(ncid))
            
            call info("Writing chaninfo:")
            call nc_diag_chaninfo_write_data
            
            call info("Writing metadata:")
            call nc_diag_metadata_write_data
            
            call info("Writing data2d:")
            call nc_diag_data2d_write_data
            
            call info("All done queuing in data, letting NetCDF take over!")
            call check(nf90_close(ncid))
#else
            call warning("NetCDF support is disabled, so no writing will occur.")
#endif
            call info("All done!")
            
            call nc_diag_finish
        end subroutine nc_diag_write
        
        subroutine nc_diag_finish
#ifdef ENABLE_ACTION_MSGS
            if (enable_action) then
                call actionm("nc_diag_finish()")
            end if
#endif
            if (init_done) then
                call info("Cleaning up...")
                if (.NOT. allocated(diag_chaninfo_store)) then
                    call error("BUG! diag_chaninfo_store is not allocated, but init_done is set!")
                end if
                
                if (.NOT. allocated(diag_metadata_store)) then
                    call error("BUG! diag_metadata_store is not allocated, but init_done is set!")
                end if
                
                if (.NOT. allocated(diag_data2d_store)) then
                    call error("BUG! diag_data2d_store is not allocated, but init_done is set!")
                end if
                
                if (.NOT. allocated(diag_varattr_store)) then
                    call error("BUG! diag_data2d_store is not allocated, but init_done is set!")
                end if
                
                deallocate(diag_chaninfo_store)
                deallocate(diag_metadata_store)
                deallocate(diag_data2d_store)
                deallocate(diag_varattr_store)
                
                init_done = .FALSE.
            else
                call error("Attempted to deallocate without initializing!")
            end if
        end subroutine nc_diag_finish
        
        subroutine nc_diag_flush_buffer
#ifndef NO_NETCDF
#ifdef ENABLE_ACTION_MSGS
            if (enable_action) then
                call actionm("nc_diag_flush_buffer()")
            end if
#endif
            if ((.NOT. diag_chaninfo_store%def_lock) .OR. &
                (.NOT. diag_metadata_store%def_lock) .OR. &
                (.NOT. diag_data2d_store%def_lock)) &
                call error("Definitions must be locked in order to flush the buffer!")
            
            ! Perform writes with the buffer flag set!
            call info("Flushing chaninfo:")
            call nc_diag_chaninfo_write_data(.TRUE.)
            
            call info("Flushing metadata:")
            call nc_diag_metadata_write_data(.TRUE.)
            
            call info("Flushing data2d:")
            call nc_diag_data2d_write_data(.TRUE.)
            
#else
            call warning("NetCDF support is disabled, so no buffer flush will occur.")
#endif
            
            call info("Flushing done!")
        end subroutine nc_diag_flush_buffer
        
        subroutine nc_diag_flush_to_file
#ifdef ENABLE_ACTION_MSGS
            if (enable_action) then
                call actionm("nc_diag_flush_to_file()")
            end if
#endif
#ifndef NO_NETCDF
            call check(nf90_sync(ncid))
#else
            call warning("NetCDF support is disabled, so no NetCDF file flush will occur.")
#endif
        end subroutine
        
        subroutine nc_diag_set_strict(enable_strict)
            logical, intent(in) :: enable_strict
            
            if (init_done) then
                call nc_diag_chaninfo_set_strict(enable_strict)
                call nc_diag_metadata_set_strict(enable_strict)
                call nc_diag_data2d_set_strict(enable_strict)
            else
                call error("Can't set strictness level - NetCDF4 layer not initialized yet!")
            end if
        end subroutine nc_diag_set_strict
        
        subroutine check(status)
          integer, intent ( in) :: status
          
          if(status /= nf90_noerr) then 
            call error(trim(nf90_strerror(status)))
          end if
        end subroutine check
        
        subroutine error(err)
            character(len=*), intent(in) :: err
#ifdef ERROR_TRACEBACK
            integer                      :: div0
#endif
#ifdef ANSI_TERM_COLORS
            write(*, "(A)") CHAR(27) // "[31m" // &
                            " **   ERROR: " // err // &
                            CHAR(27) // "[0m"
#else
            write(*, "(A)") " **   ERROR: " // err
#endif
#ifdef ERROR_TRACEBACK
            write(*, "(A)") " ** Failed to process data/write NetCDF4."
            write(*, "(A)") "    (Traceback requested, triggering div0 error...)"
            div0 = 1 / 0
#else
            stop " ** Failed to process data/write NetCDF4."
#endif
        end subroutine error
        
        subroutine warning(warn)
            character(len=*), intent(in) :: warn
#ifdef ANSI_TERM_COLORS
            write(*, "(A)") CHAR(27) // "[33m" // &
                            " ** WARNING: " // warn // &
                            CHAR(27) // "[0m"
#else
            write(*, "(A)") " ** WARNING: " // warn
#endif
        end subroutine warning
        
        subroutine nc_set_action_display(action_on_off)
            logical :: action_on_off
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (enable_action) then
                write(action_str, "(A, L, A)") "nc_set_action_display(action_on_off = ", action_on_off, ")"
                call actionm(trim(action_str))
            end if
#endif
            enable_action = action_on_off
        end subroutine nc_set_action_display
        
#ifdef ENABLE_ACTION_MSGS
        subroutine actionm(act)
            character(len=*), intent(in) :: act
            if (enable_action) &
#ifdef ANSI_TERM_COLORS
                write(*, "(A)") CHAR(27) // "[36m" // &
                                " **  ACTION: " // act // &
                                CHAR(27) // "[0m"
#else
                write(*, "(A)") " **  ACTION: " // act
#endif
        end subroutine actionm
#endif
        
        subroutine nc_set_info_display(info_on_off)
            logical :: info_on_off
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (enable_action) then
                write(action_str, "(A, L, A)") "nc_set_info_display(info_on_off = ", info_on_off, ")"
                call actionm(trim(action_str))
            end if
#endif
            enable_info = info_on_off
        end subroutine nc_set_info_display
        
        subroutine info(ifo)
            character(len=*), intent(in) :: ifo
            if (enable_info) &
#ifdef ANSI_TERM_COLORS
                write(*, "(A)") CHAR(27) // "[34m" // &
                                " **    INFO: " // ifo // &
                                CHAR(27) // "[0m"
#else
                write(*, "(A)") " **    INFO: " // ifo
#endif
        end subroutine info
        
#ifdef _DEBUG_MEM_
        subroutine debug(dbg)
            character(len=*), intent(in) :: dbg
            write(*, "(A, A)") "D: ", dbg
        end subroutine debug
#endif
    
end module netcdf_layer
