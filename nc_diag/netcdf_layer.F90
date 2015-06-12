module netcdf_layer
    use netcdf
    use fnv32mod
    use kinds
    implicit none
    
    logical,dimension(:),allocatable        :: def_locked
    real(r_single),dimension(:),allocatable :: bifix
    !real(r_single),dimension(:),allocatable    :: bifix
    
    ! diag_header struct constants
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
    
#include "netcdf_realloc_decl.f90"
#include "netcdf_lheader_decl.f90"
#include "netcdf_chaninfo_decl.F90"
    
    contains
#include "netcdf_realloc_imp.f90"
#include "netcdf_hresize.F90"
#include "netcdf_ciresize.F90"
#include "netcdf_lheader_imp.F90"
#include "netcdf_chaninfo_imp.F90"
        
        subroutine nc_diag_init(filename)
            character(len=*),intent(in)    :: filename
            print *,'Initializing netcdf layer library...'
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
                if (allocated(diag_header_store)) then
                    call error("BUG! diag_header_store is allocated, but init_done is set!")
                end if
                allocate(diag_header_store)
                allocate(diag_chaninfo_store)
                init_done = .TRUE.
            else
                call error("Attempted to initialize without closing previous nc_diag file!")
            end if
        end subroutine nc_diag_init
        
        !subroutine nc_diag_metadata
        !    
        !end subroutine nc_diag_metadata
        !
        !subroutine nc_diag_data(data_name, data_index, data)
        !    
        !end subroutine nc_diag_data
        
        subroutine nc_diag_write
            call nc_diag_header_write
            call nc_diag_chaninfo_write_def
            
            ! Lock definition writing!
            call check(nf90_enddef(ncid))
            
            call nc_diag_chaninfo_write_data
            
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
            write(*, "(A, A)") "ERROR: ", err
            stop "Failed to process data/write NetCDF4."
        end subroutine error
        
        subroutine warning(warn)
            character(len=*), intent(in) :: warn
            write(*, "(A, A)") "WARNING: ", warn
        end subroutine warning
        
#ifdef _DEBUG_MEM_
        subroutine debug(dbg)
            character(len=*), intent(in) :: dbg
            write(*, "(A, A)") "D: ", dbg
        end subroutine debug
#endif
    
end module netcdf_layer
