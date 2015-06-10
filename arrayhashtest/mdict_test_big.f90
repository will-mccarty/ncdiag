! minidict tester

program mdict_test_big
    use kinds
    !use fnv32mod
    use minidict
    
    integer(i_long), parameter                :: KEY_AMT = 70000
    integer(i_long), parameter                :: REPEAT = 100
    type(mdict), pointer :: test_dict
    
    integer(i_kind) :: val
    integer(i_long) :: i, j
    character(len=100) :: keyname
    
    call mdict_init(test_dict)
    
    do j = 1, REPEAT
        do i = 1, KEY_AMT
            write (keyname, "(A3, I8)") "key", i
            call mdict_add(test_dict, keyname, i*2)
        end do
    end do
    
    do j = 1, REPEAT
        do i = 1, KEY_AMT
            write (keyname, "(A3, I4)") "key", i
            !if (.NOT. (mdict_find(test_dict, keyname))) then
            !    print *, "Error in validating dictionary keys!"
            !    stop "Dictionary validation failed."
            !end if
            
            val = mdict_get(test_dict, keyname)
            write (*, "(A, A8, A, I4)") "Value of key  ", keyname, ": ", val
        end do
    end do
    
    call mdict_print_debug_info(test_dict)
end program mdict_test_big
