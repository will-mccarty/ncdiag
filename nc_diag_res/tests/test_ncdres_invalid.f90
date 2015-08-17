program test_ncdres_invalid
    use kinds
    use nc_diag_res
    
    call ncdres_set_info_display(.TRUE.)
    
    call nc_diag_load_resource_file("test_invalid.json")
    call assert_false(nc_diag_load_check_variable("myvar1"), "myvar1")
    call assert_false(nc_diag_load_check_variable("myvar2"), "myvar2")
    call assert_false(nc_diag_load_check_variable("myvar3"), "myvar3")
    call assert_false(nc_diag_load_check_variable("myvar4"), "myvar4")
    call nc_diag_close_resource_file
    
    contains
        subroutine assert_true(res, metadata)
            logical,          intent(in) :: res
            character(len=*), intent(in) :: metadata
            
            if (.NOT. res) &
                call ncdres_error("Assertion for TRUE failed for test " // metadata // "!")
            call ncdres_info("PASS: Test assert TRUE: "  // metadata)
        end subroutine assert_true
        
        subroutine assert_false(res, metadata)
            logical,          intent(in) :: res
            character(len=*), intent(in) :: metadata
            
            if (res) &
                call ncdres_error("Assertion for FALSE failed for test " // metadata // "!")
            call ncdres_info("PASS: Test assert FALSE: "  // metadata)
        end subroutine assert_false
end program test_ncdres_invalid
