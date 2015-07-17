program nc_diag_compress
    use iflport
    use netcdf
    use netcdf_unlimdims
    use nc_cmp_realloc
    use kinds
    
#include "nc_cmp_cli_decl.F90"
#include "nc_cmp_metadata_decl.F90"
#include "nc_cmp_data_decl.F90"
    logical                            :: enable_info = .TRUE.
    
    integer(i_long)                    :: ncid_output, ncid_input
    integer(i_long)                    :: bsize = 16777216;
    
    integer, parameter                 :: nc_diag_compress_GZIP_COMPRESS = 9
    integer, parameter                 :: nc_diag_compress_CHUNK_SIZE = 16384
    
    call nc_diag_compress_process_args
    
#ifndef NO_NETCDF
    write (*,"(A, A, A)") 'Initializing netcdf layer library, version ', trim(nf90_inq_libvers()), '...'
    !call string_before_delimiter(trim(nf90_inq_libvers()), " ", version_num)
#endif
    
    call info("Creating new NetCDF file: " // trim(output_file))
#ifndef NO_NETCDF
    call check( nf90_create(output_file, OR(NF90_NETCDF4, NF90_CLOBBER), ncid_output, &
        0, bsize, cache_nelems = 16777216) ) ! Optimization settings
#endif
    
    ! nc_diag_compress steps:
    !   1) Do a quick pass to read metadata, then allocate space as
    !      necessary.
    !   2) Define variables with metadata. Do NOT store attributes.
    !   3) Read each file again, this time concatenating everything
    !      as we go.
    
    call nc_diag_compress_metadata_pass
    call nc_diag_compress_metadata_define
    
#ifdef DEBUG
    print *, "MAIN: trigger data pass!"
#endif
    
    call nc_diag_compress_data_pass
    
#ifdef DEBUG
    print *, "ALL DONE!"
#endif
    
    call info("All data queued, letting NetCDF take over (and actually write)!")
    
    call check(nf90_close(ncid_output))
    
    call info("All done!")
    
    contains
        
#include "nc_cmp_util.F90"
#include "nc_cmp_cli_imp.F90"
#include "nc_cmp_metadata_imp.F90"
#include "nc_cmp_data_imp.F90"
end program nc_diag_compress
