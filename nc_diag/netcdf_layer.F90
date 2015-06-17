module netcdf_layer
    use netcdf
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
    
    integer(i_short), parameter             :: NLAYER_DEFAULT_ENT = 1024
    
    integer :: ncid
    logical :: init_done = .FALSE.
    logical :: header_locked = .FALSE.
    
#ifndef IGNORE_VERSION
    logical :: NLAYER_STRING_BROKEN = .FALSE.
#endif
    
#include "netcdf_realloc_decl.f90"
#include "netcdf_lheader_decl.f90"
#include "netcdf_chaninfo_decl.F90"
#include "netcdf_metadata_decl.f90"
    
    contains
#include "netcdf_realloc_imp.f90"
#include "netcdf_ciresize.F90"
#include "netcdf_mresize.F90"
#include "netcdf_lheader_imp.F90"
#include "netcdf_chaninfo_imp.F90"
#include "netcdf_metadata_imp.F90"
        
        subroutine nc_diag_init(filename)
            character(len=*),intent(in)    :: filename
            character(len=:), allocatable  :: version_num
            !print *,'Initializing netcdf layer library, version ...'
            write (*,"(A, A, A)") 'Initializing netcdf layer library, version ', trim(nf90_inq_libvers()), '...'
            call string_before_delimiter(trim(nf90_inq_libvers()), " ", version_num)
            
#ifndef IGNORE_VERSION
            call nc_version_check(version_num)
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
                call check( nf90_create(filename, OR(NF90_NETCDF4, NF90_CLOBBER), ncid) )
                if (allocated(diag_chaninfo_store)) then
                    call error("BUG! diag_chaninfo_store is allocated, but init_done is set!")
                end if
                
                if (allocated(diag_metadata_store)) then
                    call error("BUG! diag_metadata_store is allocated, but init_done is set!")
                end if
                
                allocate(diag_chaninfo_store)
                allocate(diag_metadata_store)
                
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
        
        subroutine nc_diag_write
            call nc_diag_chaninfo_write_def
            call nc_diag_metadata_write_def
            
            ! Lock definition writing!
            call check(nf90_enddef(ncid))
            
            call nc_diag_chaninfo_write_data
            call nc_diag_metadata_write_data
            
            call check(nf90_close(ncid))
            
        end subroutine nc_diag_write
        
        subroutine check(status)
          integer, intent ( in) :: status
          
          if(status /= nf90_noerr) then 
            call error(trim(nf90_strerror(status)))
          end if
        end subroutine check
        
        subroutine error(err)
            character(len=*), intent(in) :: err
#ifdef ANSI_TERM_COLORS
            write(*, "(A)") CHAR(27) // "[31m"
#endif
            write(*, "(A, A)") " ** ERROR: ", err
#ifdef ANSI_TERM_COLORS
            write(*, "(A)") CHAR(27) // "[0m"
#endif
            stop " ** Failed to process data/write NetCDF4."
        end subroutine error
        
        subroutine warning(warn)
            character(len=*), intent(in) :: warn
#ifdef ANSI_TERM_COLORS
            write(*, "(A)") CHAR(27) // "[33m"
#endif
            write(*, "(A, A)") " ** WARNING: ", warn
#ifdef ANSI_TERM_COLORS
            write(*, "(A)") CHAR(27) // "[0m"
#endif
        end subroutine warning
        
#ifdef _DEBUG_MEM_
        subroutine debug(dbg)
            character(len=*), intent(in) :: dbg
            write(*, "(A, A)") "D: ", dbg
        end subroutine debug
#endif
    
end module netcdf_layer
