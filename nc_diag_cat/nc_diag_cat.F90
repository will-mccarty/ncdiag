program nc_diag_cat
    use kinds
    use netcdf
    use netcdf_unlimdims
    use ncdc_climsg
    use ncdc_state
    use ncdc_cli_process
    use ncdc_metadata
    
#ifdef USE_MPI
    use ncdc_data_mpi
#else
    use ncdc_data
#endif
    
    ! NCDC = Net CDF Diag Concatenation
    character(len=300)                 :: info_str
    
    real :: start_time, stop_time
    
#ifdef USE_MPI
    ! MPI is essentially a smarter fork()... but remember, we're still
    ! forking! That means that there WILL be multiple processes!
    
    ! Do MPI things:
    ! First, initialize it!
    call MPI_INIT(ierr)
    
    ! Get the current processor (or really, the "PC") number
    call MPI_COMM_RANK(MPI_COMM_WORLD, cur_proc, ierr)
    
    ! Get the total number of processors / PCs
    call MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, ierr)
    
    if (num_procs < 2) &
        call error("At least 2 processors are required to use MPI features.")
    
    if (num_procs < 3) &
        call warning("3 processors or more is needed to best use MPI features.")
    
    if (cur_proc == 0) &
        call info("Using MPI for faster concatenation.")
#endif
    
#ifndef NO_NETCDF
    call info('Initializing netcdf layer library, version ' // trim(nf90_inq_libvers()) // '...')
    !call string_before_delimiter(trim(nf90_inq_libvers()), " ", version_num)
#endif
    
    ! nc_diag_cat steps:
    !   1) Do a quick pass to read metadata, then allocate space as
    !      necessary.
    !   2) Define variables with metadata. Do NOT store attributes.
    !   3) Read each file again, this time concatenating everything
    !      as we go.
    
    call nc_diag_cat_process_args
    
#ifdef USE_MPI
    if (cur_proc == 0) then
#endif
        call info("Creating new NetCDF file: " // trim(output_file))
#ifndef NO_NETCDF
        call check( nf90_create(output_file, OR(NF90_NETCDF4, NF90_CLOBBER), ncid_output, &
            0) )
#endif
#ifdef USE_MPI
    end if
#endif
    
    call cpu_time(start_time)
    call nc_diag_cat_metadata_pass
    call cpu_time(stop_time)
    
    write (info_str, "(A, F, A)") "Metadata read took ", stop_time - start_time, " seconds!"
    call info(trim(info_str))
    
#ifdef USE_MPI
    if (cur_proc == 0) then
#endif
        call nc_diag_cat_metadata_define
        
#ifdef DEBUG
        print *, "MAIN: trigger data pass!"
#endif
    
        call cpu_time(start_time)
        call nc_diag_cat_metadata_alloc
        call cpu_time(stop_time)
        
        write (info_str, "(A, F, A)") "Data preallocation took ", stop_time - start_time, " seconds!"
        call info(trim(info_str))
#ifdef USE_MPI
    end if
#endif
    
    call cpu_time(start_time)
    call nc_diag_cat_data_pass
    call cpu_time(stop_time)
    
    write (info_str, "(A, F, A)") "Data read took ", stop_time - start_time, " seconds!"
    call info(trim(info_str))
    
#ifdef USE_MPI
    if (cur_proc == 0) then
#endif
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
#ifdef USE_MPI
    endif
    
    call MPI_FINALIZE(ierr)
#endif
    
    call info("All done!")
end program nc_diag_cat
