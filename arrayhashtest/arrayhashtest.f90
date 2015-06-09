program arrayhashtest
    use kinds
    use fnv32mod
    
    implicit none
    
    integer(i_byte), dimension(3) :: test
    character(len=100) :: teststr
    integer(i_long) :: hash1
    integer(i_long) :: hash2
    
    test = (/ 123, 234, 345 /)
    teststr = "hello world"
    
    hash1 = Z'811C9DC5'
    hash2 = Z'811C9DC5'
    
    print *, "========================="
    print *, "Hashing integer input..."
    print *, "========================="
    write (*, "(A14, Z)") ("Input hash: ", hash1)
    call fnv32(test, 3, hash1)
    write (*, "(A14, Z)") ("Output hash: ", hash1)
    print *,hash1
    
    print *, "========================="
    print *, "Hashing string input..."
    print *, "========================="
    write (*, "(A14, Z)") ("Input hash: ", hash2)
    call fnv32_str(teststr, hash2)
    write (*, "(A14, Z)") ("Output hash: ", hash2)
    print *,hash2
end program arrayhashtest
