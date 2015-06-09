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
        
        subroutine mdict_append(dict, hash_index, node)
            type(mdict), pointer, intent(inout)  :: dict
            integer(i_long), intent(in)                :: hash_index
            type(mdict_node), intent(in)               :: node
            
            type(mdict_ptr), dimension(:), allocatable :: mdict_arr
            
            if (dict%mdict_arr_alloc(hash_index) == -1) then
                mdict_arr = dict%mdict_arr(hash_index)
                allocate(mdict_arr(MDICT_VALS))
                dict%mdict_arr_alloc(hash_index) = 0
            end if
            
            dict%mdict_arr_alloc(hash_index) = dict%mdict_arr_alloc(hash_index) + 1
            mdict_arr(dict%mdict_arr_alloc(hash_index))%ptr = node
        end subroutine mdict_append
        
        subroutine mdict_add(dict, key, assoc_value)
            type(mdict), pointer, intent(inout)   :: dict
            character(len=*), intent(in)          :: key
            integer(i_kind), intent(in)           :: assoc_value
            
        end subroutine mdict_add
        
        subroutine mdict_find(dict, key)
            type(mdict), pointer, intent(inout)   :: dict
            character(len=*), intent(in)          :: key
        
        end subroutine mdict_find
        
        subroutine mdict_get(dict, key)
            type(mdict), pointer, intent(inout)   :: dict
            character(len=*), intent(in)          :: key
            
        end subroutine mdict_get
end module minidict
