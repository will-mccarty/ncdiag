! minidict tester

program mdict_test
    use kinds
    
    integer(i_long), parameter                :: KEY_AMT = 250
    integer(i_long), parameter                :: REPEAT = 100
    character(len=100), dimension(KEY_AMT) :: keys
    integer(i_kind), dimension(KEY_AMT) :: vals
    integer(i_long) :: count
    
    integer(i_kind) :: val
    integer(i_long) :: i, j
    character(len=100) :: keyname
    
    count = 0
    
    do i = 1, KEY_AMT
        write (keyname, "(A3, I4)") "key", i
        call add_val(keyname, i*2)
    end do
    
    do j = 1, REPEAT
        do i = 1, KEY_AMT
            write (keyname, "(A3, I4)") "key", i
            !if (.NOT. (mdict_find(test_dict, keyname))) then
            !    print *, "Error in validating dictionary keys!"
            !    stop "Dictionary validation failed."
            !end if
            
            val = fetch_val(keyname)
            write (*, "(A, A8, A, I4)") "Value of key  ", keyname, ": ", val
        end do
    end do
    
    contains
        subroutine add_val(key, value)
            character(len=*), intent(in) :: key
            integer(i_kind), intent(in) :: value
            
            count = count + 1
            keys(count) = key
            vals(count) = value
        end subroutine add_val
        
        function fetch_val(key) result(value)
            character(len=*), intent(in) :: key
            integer(i_kind) :: value
            integer(i_long) :: i
            
            value = 0
            
            do i = 1, count
                if (keys(i) == key) then
                    value = vals(i)
                    exit
                end if
            end do
        end function fetch_val
end program mdict_test
