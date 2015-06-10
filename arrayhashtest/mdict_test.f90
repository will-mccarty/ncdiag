! minidict tester

program mdict_test
    use kinds
    !use fnv32mod
    use minidict
    
    type(mdict), pointer :: test_dict
    
    integer(i_kind) :: val
    integer(i_long) :: i
    character(len=100) :: keyname
    
    call mdict_init(test_dict)
    
    do i = 1, 1000
        write (keyname, "(A3, I4)") "key", i
        call mdict_add(test_dict, keyname, i*2)
    end do
    
    do i = 1, 1000
        write (keyname, "(A3, I4)") "key", i
        if (.NOT. (mdict_find(test_dict, keyname)))
            print *, "Error in validating dictionary keys!"
            stop "Dictionary validation failed."
        end if
        
        val = mdict_get(test_dict, keyname)
        write (*, "(A, A, A, I4)") "Value of key ", keyname, ": ", val)
    end if
    
    val1 = mdict_get(test_dict, "key1")
    val2 = mdict_get(test_dict, "key2")
    val3 = mdict_get(test_dict, "key3")
    val4 = mdict_get(test_dict, "key4")
    val5 = mdict_get(test_dict, "key5")
    val6 = mdict_get(test_dict, "key6")
    val7 = mdict_get(test_dict, "key7")
    val8 = mdict_get(test_dict, "key8")
    val9 = mdict_get(test_dict, "key9")
    val10 = mdict_get(test_dict, "key10")
    
    write (*, "(A, I4, I4, I4)") "Values of keys key1, key2, key3:", val1, val2, val3
    write (*, "(A, I4, I4, I4)") "Values of keys key4, key5, key6:", val4, val5, val6
    write (*, "(A, I4, I4, I4)") "Values of keys key7, key8, key9:", val7, val8, val9
    write (*, "(A, I4, I4, I4)") "Values of keys key10:", val10
    
end program mdict_test
