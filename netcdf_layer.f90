module netcdf_layer
    use netcdf
    use FNV32MOD
    use kinds
    implicit none
    
    logical,dimension(:),allocatable           :: def_locked
    real(r_single),dimension(:),allocatable    :: bifix
    !real(r_single),dimension(:),allocatable    :: bifix
    
    contains
        subroutine init_netcdf(filename, ncid)
            character(len=*),intent(in)    :: filename
            integer, intent(out)           :: ncid
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
            call check( nf90_create(filename, OR(NF90_NETCDF4, NF90_CLOBBER), ncid) )
        end subroutine init_netcdf
        
        subroutine add_netcdf_dimension(ncid, dim_name, dim_type, dim_id)
            integer, intent(in)            :: ncid
            character(len=*),intent(in)    :: dim_name
            integer,intent(in)             :: dim_type
            integer, intent(out)           :: dim_id
            call check( nf90_def_dim(ncid, dim_name, dim_type, dim_id) )
        end subroutine
        
        subroutine add_netcdf_variable(ncid, var_name, var_type, var_dim, var_id)
            integer, intent(in)            :: ncid
            character(len=*),intent(in)    :: var_name
            integer,intent(in)             :: var_type
            integer,dimension(:),intent(in):: var_dim
            integer, intent(out)           :: var_id
            
            call check( nf90_def_var(ncid, var_name, var_type, var_dim, var_id) )
        end subroutine add_netcdf_variable
        
        subroutine add_netcdf_attribute(ncid, var_id, attr_name, 
        
        subroutine check(status)
          integer, intent ( in) :: status
          
          if(status /= nf90_noerr) then 
            print *, trim(nf90_strerror(status))
            stop "Stopped"
          end if
        end subroutine check
    
end module netcdf_layer
