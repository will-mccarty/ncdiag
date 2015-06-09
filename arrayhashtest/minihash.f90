! minihash

module minihash
    type mhash_node
        character(len=*) :: id
        integer(i_kind)  :: value
    end type mhash_node
    
    type mhash_ptr
        type(mhash_node), pointer :: ptr
    end type mhash_ptr
    
    type mhash
        integer(i_long) :: count
        type(mhash_ptr), dimension(:), allocatable :: mhash_arr
        logical, dimension(:), allocatable :: mhash_arr_alloc
    end type mhash
    
    type(mhash) :: mhash_store
    logical :: mhash_alloc = .FALSE.
    
    contains
        subroutine mhash_init(
            if (.NOT. mhash_alloc) then
                allocate(mhash_store)
                mhash_alloc = .TRUE.
            end if
        end subroutine mhash_init
        
        subroutine mhash_add(key, value)
            
        end subroutine mhash_add
        
        subroutine mhash_find(key)
        
        end subroutine mhash_find
        
        subroutine mhash_get(key)
        
        end subroutine mhash_get
