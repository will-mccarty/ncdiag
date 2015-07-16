program test_nc_unlimdims
    use netcdf
    use netcdf_unlimdims
    
    logical :: enable_info = .TRUE.
    
    integer :: cli_arg_count, ncid_input, num_unlims, tmp_dim_size, i
    integer, dimension(:), allocatable :: unlim_dims
    character(len=10000000) :: input_file, prgm_name
    character(len=NF90_MAX_NAME) :: tmp_dim_name
    
    call get_command_argument(0, prgm_name)
    cli_arg_count = command_argument_count()
    
    if (cli_arg_count /= 1) &
        call error("Usage: " // trim(prgm_name) // " [input NetCDF4 file]")
    
    call get_command_argument(1, input_file)
    
    call info("Opening NetCDF4 file: " // trim(input_file))
    
    call check(nf90_open(input_file, NF90_NOWRITE, ncid_input))
    
    call check(pf_nf90_inq_unlimdims(ncid_input, num_unlims))
    
    write (*, "(A, I0)") "Number of unlimited dimensions: ", num_unlims
    allocate(unlim_dims(num_unlims))
    
    call check(pf_nf90_inq_unlimdims(ncid_input, num_unlims, unlim_dims))
    
    do i = 1, num_unlims
        call check(nf90_inquire_dimension(ncid_input, int(unlim_dims(i)), &
                            tmp_dim_name, tmp_dim_size))
        write (*, "(A, I0, A, I0, A)") " => Unlimited dimension | ID: ", unlim_dims(i), " | Size: ", tmp_dim_size, &
            " | Name = " // trim(tmp_dim_name)
    end do
    
    deallocate(unlim_dims)
    
    contains
#include "ncdc_util.F90"

end program test_nc_unlimdims
