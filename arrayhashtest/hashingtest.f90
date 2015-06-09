program arrayhashtest
    use kinds
    use fnv32mod
    
    implicit none
    
    integer(i_byte), dimension(3) :: test
    character(len=100) :: teststr
    integer(i_long) :: hash1, hash2, hash3, hash4
    
    test = (/ 123, 234, 345 /)
    teststr = "hello world"
    
    hash1 = Z'811C9DC5'
    hash2 = Z'811C9DC5'
    
    hash3 = Z'811C9DC5'
    hash4 = Z'811C9DC5'
    
    print *, "========================="
    print *, "Hashing integer input..."
    print *, "========================="
    write (*, "(A20, I)") ("Input hash (int): ", hash1)
    write (*, "(A20, Z)") ("Input hash (hex): ", hash1)
    call fnv32(test, 3, hash1)
    write (*, "(A20, I)") ("Output hash (int): ", hash1)
    write (*, "(A20, Z)") ("Output hash (hex): ", hash1)
    !print *,hash1
    
    print *, "========================="
    print *, "Hashing string input..."
    print *, "========================="
    write (*, "(A20, I)") ("Input hash (int): ", hash2)
    write (*, "(A20, Z)") ("Input hash (hex): ", hash2)
    call fnv32_str(teststr, hash2)
    write (*, "(A20, I)") ("Output hash (int): ", hash2)
    write (*, "(A20, Z)") ("Output hash (hex): ", hash2)
    !print *,hash2
    
    print *, "================================="
    print *, "Hashing integer input (16 bit)..."
    print *, "================================="
    write (*, "(A20, I)") ("Input hash (int): ", hash3)
    write (*, "(A20, Z)") ("Input hash (hex): ", hash3)
    call fnv16(test, 3, hash3)
    write (*, "(A20, I)") ("Output hash (int): ", hash3)
    write (*, "(A20, Z)") ("Output hash (hex): ", hash3)
    !print *,hash3
    
    print *, "================================="
    print *, "Hashing string input (16 bit)..."
    print *, "================================="
    write (*, "(A20, I)") ("Input hash (int): ", hash4)
    write (*, "(A20, Z)") ("Input hash (hex): ", hash4)
    call fnv16_str(teststr, hash4)
    write (*, "(A20, I)") ("Output hash (int): ", hash4)
    write (*, "(A20, Z)") ("Output hash (hex): ", hash4)
    !print *,hash4
end program arrayhashtest
