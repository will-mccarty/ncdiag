program test_ncdres_fclose_crash_1
    use kinds
    use nc_diag_res
    
    ! Nothing is open, so this should fail!
    call nc_diag_close_resource_file
end program test_ncdres_fclose_crash_1
