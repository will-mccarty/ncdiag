! minidict

module minidict
    use kinds
    use fnv32mod
    
    integer(i_long), parameter                :: MDICT_MAX_KEYS = 65536
    integer(i_long), parameter                :: MDICT_VALS     = 1024
    
    type mdict_node
        character(len=100) :: id
        integer(i_kind)  :: value
    end type mdict_node
    
    type mdict_ptr
        ! This is a pointer to an array of type(mdict_node)
        type(mdict_node), dimension(:), pointer :: ptr
    end type mdict_ptr
    
    type mdict
        ! number of keys
        integer(i_long)                            :: count
        ! array of pointers to type(mdict_node)
        type(mdict_ptr), dimension(:), allocatable :: mdict_arr
        ! whether the particular element has been allocated or not
        ! -1 indicates no allocation, positive #s indicate usage count
        logical, dimension(:), allocatable         :: mdict_arr_alloc
    end type mdict
    
    type(mdict) :: mdict_store
    logical :: mdict_alloc = .FALSE.
    
    contains
        ! mdict_init(dict)
        !   Initialize a mini dictionary.
        !   
        !   NOTE: If you are reusing a variable, you MUST make sure the
        !   dictionary has been closed first! Otherwise, you WILL
        !   experience memory leaks!
        !   
        !   Arguments:
        !     dict: type(mdict) pointer
        subroutine mdict_init(dict)
            type(mdict), pointer, intent(inout)  :: dict
            
            nullify(dict)
            allocate(dict)
            
            allocate(dict%mdict_arr_alloc(MDICT_MAX_KEYS))
            dict%mdict_arr_alloc = -1
            
            allocate(dict%mdict_arr(MDICT_MAX_KEYS))
            
            dict%count = 0
        end subroutine mdict_init
        
        subroutine mdict_destroy(dict)
            type(mdict), pointer, intent(inout)  :: dict
            
            integer :: i
            
            do i = 0, MDICT_MAX_KEYS
                if (dict%mdict_arr_alloc(i)) then
                    deallocate(dict%mdict_arr(i)%ptr)
                end if
            end do
            
            deallocate(dict%mdict_arr)
            deallocate(dict%mdict_arr_alloc)
            deallocate(dict)
        end subroutine mdict_destroy
        
        subroutine mdict_realloc(ptr)
            type(mdict_node), dimension(:), pointer :: ptr
            
            type(mdict_node), dimension(:), save, allocatable :: tmp
            
            allocate(tmp(size(ptr) + MDICT_VALS))
            tmp(1:size(ptr)) = ptr
            deallocate(ptr)
            ptr = tmp
        end subroutine mdict_realloc
        
        subroutine mdict_append(dict, hash_index, node)
            type(mdict), pointer, intent(inout)  :: dict
            integer(i_long), intent(in)                :: hash_index
            type(mdict_node), intent(in)               :: node
            
            integer(i_long) :: i
            logical         :: exists
            
            if (dict%mdict_arr_alloc(hash_index) == -1) then
                allocate(dict%mdict_arr(hash_index)%ptr(MDICT_VALS))
                dict%mdict_arr_alloc(hash_index) = 0
            else
                if (dict%mdict_arr_alloc(hash_index) >= size(dict%mdict_arr(hash_index)%ptr)) then
                    call mdict_realloc(dict%mdict_arr(hash_index)%ptr)
                endif
            end if
            
            ! Check if the key exists. If it does, overwrite the node!
            exists = .FALSE.
            
            do i = 1, dict%mdict_arr_alloc(hash_index)
                if (dict%mdict_arr(hash_index)%ptr(i)%id == node%id) then
                    dict%mdict_arr(hash_index)%ptr(i) = node
                    exists = .TRUE.
                    exit
                end if
            end do
            
            ! If the key doesn't already exist, create a new one!
            if (.NOT. exists) then
                ! Increment the count
                dict%mdict_arr_alloc(hash_index) = dict%mdict_arr_alloc(hash_index) + 1
                dict%mdict_arr(hash_index)%ptr(dict%mdict_arr_alloc(hash_index)) = node
            end if
        end subroutine mdict_append
        
        subroutine mdict_add(dict, key, assoc_value)
            type(mdict), pointer, intent(inout)   :: dict
            character(len=*), intent(in)          :: key
            integer(i_kind), intent(in)           :: assoc_value
            
            type(mdict_node)                      :: node
            integer(i_long)                       :: hash
            
            node%id    = key
            node%value = assoc_value
            
            ! Hash with fnv_1a, truncated to 16 bit
            hash = Z'811C9DC5'
            call fnv16_str(key, hash)
            
            call mdict_append(dict, hash + 1, node)
        end subroutine mdict_add
        
        function mdict_find(dict, key) result(exists)
            type(mdict), pointer, intent(in)      :: dict
            character(len=*), intent(in)          :: key
            
            logical                               :: exists
            integer(i_long)                       :: hash, i
            
            ! Hash with fnv_1a, truncated to 16 bit
            hash = Z'811C9DC5'
            call fnv16_str(key, hash)
            
            ! Check if the key exists. If it does, overwrite the node!
            exists = .FALSE.
            do i = 1, dict%mdict_arr_alloc(hash + 1)
                if (dict%mdict_arr(hash + 1)%ptr(i)%id == key) then
                    exists = .TRUE.
                    exit
                end if
            end do
        end function mdict_find
        
        function mdict_get(dict, key) result(value)
            type(mdict), pointer, intent(inout)   :: dict
            character(len=*), intent(in)          :: key
            
            logical                               :: exists
            integer(i_long)                       :: hash, i
            integer(i_kind)                       :: value
            
            ! Hash with fnv_1a, truncated to 16 bit
            hash = Z'811C9DC5'
            call fnv16_str(key, hash)
            
            ! Check if the key exists. If it does, overwrite the node!
            exists = .FALSE.
            
            do i = 1, dict%mdict_arr_alloc(hash + 1)
                if (dict%mdict_arr(hash + 1)%ptr(i)%id == key) then
                    value = dict%mdict_arr(hash + 1)%ptr(i)%value
                    exists = .TRUE.
                    exit
                end if
            end do
            
            if (.NOT. exists) then
                value = 0
            end if
            
        end function mdict_get
        
        subroutine mdict_print_debug_info(dict)
            type(mdict), pointer, intent(inout)   :: dict
            integer(i_long)                       :: i, j
            do i = 1, MDICT_MAX_KEYS
                if (dict%mdict_arr_alloc(i) /= -1) then
                    write (*, "(A, I5, A, I5, A)") "At key ", i, " found ", dict%mdict_arr_alloc(i), " allocated k/v pairs:"
                    do j = 1, dict%mdict_arr_alloc(i)
                        write (*, "(A, A, A, I5)") "  -> Found key/value pair: key '", dict%mdict_arr(i)%ptr(j)%id, "', value:", dict%mdict_arr(i)%ptr(j)%value
                    end do
                end if
            end do
        end subroutine mdict_print_debug_info
end module minidict
