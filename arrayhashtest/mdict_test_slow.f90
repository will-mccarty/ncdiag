! minidict tester

program mdict_test
    use kinds
    
    character(len=100), dimension(10) :: keys
    integer(i_kind), dimension(10) :: vals
    integer(i_long) :: count
    
    integer(i_kind) :: val1, val2, val3, val4, val5, val6, val7, val8, val9, val10
    
    count = 0
    
    call add_val("key1", 123)
    call add_val("key2", 234)
    call add_val("key3", 567)
    call add_val("key4", 891)
    call add_val("key5", 234)
    call add_val("key6", 567)
    call add_val("key7", 890)
    call add_val("key8", 111)
    call add_val("key9", 222)
    call add_val("key10", 333)
    
    val1 = fetch_val("key1")
    val2 = fetch_val("key2")
    val3 = fetch_val("key3")
    val4 = fetch_val("key4")
    val5 = fetch_val("key5")
    val6 = fetch_val("key6")
    val7 = fetch_val("key7")
    val8 = fetch_val("key8")
    val9 = fetch_val("key9")
    val10 = fetch_val("key10")
    
    write (*, "(A, I4, I4, I4)") "Values of keys key1, key2, key3:", val1, val2, val3
    write (*, "(A, I4, I4, I4)") "Values of keys key4, key5, key6:", val4, val5, val6
    write (*, "(A, I4, I4, I4)") "Values of keys key7, key8, key9:", val7, val8, val9
    write (*, "(A, I4, I4, I4)") "Values of keys key10:", val10
    
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
