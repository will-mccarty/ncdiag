! utils_test.f90
! Test utilities module

program utils_test
    use utils
    
    character(len=100) :: str
    
    str = "Hello, world! Today's a great day!"
    
    print *, "Original string:"
    print *, str
    print *, "Split string's first element, delimited with a space:"
    print *, string_split_index(str, " ", 1)
    print *, "Split string's second element, delimited with a space:"
    print *, string_split_index(str, " ", 2)
    
    print *, "Split string's first element, delimited with a comma:"
    print *, string_split_index(str, ",", 1)
    
    print *, "Split string's second element, delimited with a comma:"
    print *, string_split_index(str, ",", 2)
end program utils_test
