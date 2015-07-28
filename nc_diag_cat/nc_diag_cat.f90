program nc_diag_cat
    use netcdf
    use netcdf_unlimdims
    use ncdc_realloc
    use kinds
    
    ! NCDC = Net CDF Diag Concatenation
#include "ncdc_cli_decl.F90"
#include "ncdc_metadata_decl.F90"
#include "ncdc_data_decl.F90"
    logical                            :: enable_info = .TRUE.
    
    integer(i_long)                    :: ncid_output, ncid_input
    
    integer, parameter                 :: NC_DIAG_CAT_GZIP_COMPRESS = 6
    integer, parameter                 :: NC_DIAG_CAT_CHUNK_SIZE = 16384
    
    character(len=300)                 :: info_str
    call nc_diag_cat_process_args
    
    
    real :: start_time, stop_time
    
#ifndef NO_NETCDF
    write (*,"(A, A, A)") 'Initializing netcdf layer library, version ', trim(nf90_inq_libvers()), '...'
    !call string_before_delimiter(trim(nf90_inq_libvers()), " ", version_num)
#endif
    
    call info("Creating new NetCDF file: " // trim(output_file))
#ifndef NO_NETCDF
    call check( nf90_create(output_file, OR(NF90_NETCDF4, NF90_CLOBBER), ncid_output, &
        0) )
#endif
    
    ! nc_diag_cat steps:
    !   1) Do a quick pass to read metadata, then allocate space as
    !      necessary.
    !   2) Define variables with metadata. Do NOT store attributes.
    !   3) Read each file again, this time concatenating everything
    !      as we go.
    
    call cpu_time(start_time)
    call nc_diag_cat_metadata_pass
    call cpu_time(stop_time)
    
    write (info_str, "(A, F, A)") "Metadata read took ", stop_time - start_time, " seconds!"
    call info(trim(info_str))
    call nc_diag_cat_metadata_define
    
#ifdef DEBUG
    print *, "MAIN: trigger data pass!"
#endif
    
    call cpu_time(start_time)
    call nc_diag_cat_metadata_alloc
    call cpu_time(stop_time)
    
    write (info_str, "(A, F, A)") "Data preallocation took ", stop_time - start_time, " seconds!"
    call info(trim(info_str))
    call cpu_time(start_time)
    call nc_diag_cat_data_pass
    call cpu_time(stop_time)
    
    write (info_str, "(A, F, A)") "Data read took ", stop_time - start_time, " seconds!"
    call info(trim(info_str))
    
    call cpu_time(start_time)
    call nc_diag_cat_data_commit
    call cpu_time(stop_time)
    
    write (info_str, "(A, F, A)") "Data commit took ", stop_time - start_time, " seconds!"
    call info(trim(info_str))
        
#ifdef DEBUG
    print *, "ALL DONE!"
#endif
    
    call info("All data queued, letting NetCDF take over (and actually write)!")
    
    call cpu_time(start_time)
    call check(nf90_close(ncid_output))
    call cpu_time(stop_time)
    
    write (info_str, "(A, F, A)") "Final data write took ", stop_time - start_time, " seconds!"
    call info(trim(info_str))
    call info("All done!")
    
    contains
        
#include "ncdc_util.F90"
#include "ncdc_cli_imp.F90"
#include "ncdc_metadata_imp.F90"
#include "ncdc_data_imp.F90"
end program nc_diag_cat
