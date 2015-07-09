        !===============================================================
        ! nc_diag_data2d_resize - data resizing support
        !===============================================================
        ! DO NOT COMPILE THIS DIRECTLY! THIS IS MEANT TO BE INCLUDED
        ! INSIDE A LARGER F90 SOURCE!
        ! If you compile this directly, you WILL face the WRATH of your
        ! compiler!
        !---------------------------------------------------------------
        ! Depends on:
        !   netcdf_realloc_decl.f90, netcdf_realloc_imp.f90
        !
        ! Technically, order shouldn't matter... but just in case,
        ! include netcdf_realloc_imp.f90 FIRST!
        !---------------------------------------------------------------
        ! nc_diag_data2d_resize subroutines resize the arrays inside the
        ! diag_data type.
        !---------------------------------------------------------------
        ! This file provides the actual subroutines for data array
        ! resizing, referred to by the data resizing interface.
        !---------------------------------------------------------------
        
        ! nc_diag_data2d_resize - input integer(i_byte)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_data2d_resize_byte(var_index, data_index, addl_num_entries)
            integer(i_llong), intent(in)     :: var_index
            integer(i_llong), intent(in)     :: data_index
            integer(i_llong), intent(in)     :: addl_num_entries
            
            integer(i_llong)                 :: new_size
            if (allocated(diag_data2d_store%stores(var_index)%storage(data_index)%byte)) then
                diag_data2d_store%stores(var_index)%storage(data_index)%acount = addl_num_entries
                
                if (diag_data2d_store%stores(var_index)%storage(data_index)%acount >= &
                    diag_data2d_store%stores(var_index)%storage(data_index)%asize) then
#ifdef ENABLE_ACTION_MSGS
                    if (enable_action) then
                        call actionm("nc_diag_data2d_resize_byte: doing reallocation!")
                    end if
#endif
                    new_size = size(diag_data2d_store%stores(var_index)%storage(data_index)%byte) * 0.5
                    call nc_diag_realloc(diag_data2d_store%stores(var_index)%storage(data_index)%byte, new_size)
                    diag_data2d_store%stores(var_index)%storage(data_index)%asize = new_size
                end if
            else
                diag_data2d_store%stores(var_index)%storage(data_index)%acount = addl_num_entries
                allocate(diag_data2d_store%stores(var_index)%storage(data_index)%byte(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_data2d_store%stores(var_index)%storage(data_index)%asize = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_data2d_resize_byte
        
        ! nc_diag_data2d_resize - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_data2d_resize_short(var_index, data_index, addl_num_entries)
            integer(i_llong), intent(in)     :: var_index
            integer(i_llong), intent(in)     :: data_index
            integer(i_llong), intent(in)     :: addl_num_entries
            
            integer(i_llong)                 :: new_size
            
            if (allocated(diag_data2d_store%stores(var_index)%storage(data_index)%short)) then
                diag_data2d_store%stores(var_index)%storage(data_index)%acount = addl_num_entries
                
                if (diag_data2d_store%stores(var_index)%storage(data_index)%acount >= &
                    diag_data2d_store%stores(var_index)%storage(data_index)%asize) then
#ifdef ENABLE_ACTION_MSGS
                    if (enable_action) then
                        call actionm("nc_diag_data2d_resize_short: doing reallocation!")
                    end if
#endif
                    new_size = size(diag_data2d_store%stores(var_index)%storage(data_index)%short) * 0.5
                    call nc_diag_realloc(diag_data2d_store%stores(var_index)%storage(data_index)%short, new_size)
                    diag_data2d_store%stores(var_index)%storage(data_index)%asize = new_size
                end if
            else
                diag_data2d_store%stores(var_index)%storage(data_index)%acount = addl_num_entries
                allocate(diag_data2d_store%stores(var_index)%storage(data_index)%short(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_data2d_store%stores(var_index)%storage(data_index)%asize = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_data2d_resize_short
        
        ! nc_diag_data2d_resize - input integer(i_llong)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_data2d_resize_long(var_index, data_index, addl_num_entries)
            integer(i_llong), intent(in)     :: var_index
            integer(i_llong), intent(in)     :: data_index
            integer(i_llong), intent(in)     :: addl_num_entries
            
            integer(i_llong)                 :: new_size
            
            if (allocated(diag_data2d_store%stores(var_index)%storage(data_index)%long)) then
                diag_data2d_store%stores(var_index)%storage(data_index)%acount = addl_num_entries
#ifdef _DEBUG_MEM_
                print *, "diag_data2d_store%stores(var_index)%storage(data_index)%acount"
                print *, diag_data2d_store%stores(var_index)%storage(data_index)%acount
                print *, "diag_data2d_store%stores(var_index)%storage(data_index)%asize"
                print *, diag_data2d_store%stores(var_index)%storage(data_index)%asize
                print *, "diag_data2d_store%stores(var_index)%storage(data_index)%long actual size:"
                print *, size(diag_data2d_store%stores(var_index)%storage(data_index)%long)
#endif
                
                if (diag_data2d_store%stores(var_index)%storage(data_index)%acount >= &
                    diag_data2d_store%stores(var_index)%storage(data_index)%asize) then
#ifdef ENABLE_ACTION_MSGS
                    if (enable_action) then
                        call actionm("nc_diag_data2d_resize_long: doing reallocation!")
                    end if
#endif
                    new_size = size(diag_data2d_store%stores(var_index)%storage(data_index)%long) * 0.5
                    
                    call nc_diag_realloc(diag_data2d_store%stores(var_index)%storage(data_index)%long, new_size)
                    
                    diag_data2d_store%stores(var_index)%storage(data_index)%asize = size(diag_data2d_store%stores(var_index)%storage(data_index)%long)
                end if
            else
#ifdef _DEBUG_MEM_
                print *, "INITIAL - diag_data2d_store%stores(var_index)%storage(data_index)%acount set to:"
                print *, addl_num_entries
#endif
                diag_data2d_store%stores(var_index)%storage(data_index)%acount = addl_num_entries
                allocate(diag_data2d_store%stores(var_index)%storage(data_index)%long(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_data2d_store%stores(var_index)%storage(data_index)%asize = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_data2d_resize_long
        
        ! nc_diag_data2d_resize - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_data2d_resize_rsingle(var_index, data_index, addl_num_entries)
            integer(i_llong), intent(in)     :: var_index
            integer(i_llong), intent(in)     :: data_index
            integer(i_llong), intent(in)     :: addl_num_entries
            
            integer(i_llong)                 :: new_size
            
            if (allocated(diag_data2d_store%stores(var_index)%storage(data_index)%rsingle)) then
                diag_data2d_store%stores(var_index)%storage(data_index)%acount = addl_num_entries
                
                if (diag_data2d_store%stores(var_index)%storage(data_index)%acount >= &
                    diag_data2d_store%stores(var_index)%storage(data_index)%asize) then
#ifdef ENABLE_ACTION_MSGS
                    if (enable_action) then
                        call actionm("nc_diag_data2d_resize_rsingle: doing reallocation!")
                    end if
#endif
                    new_size = size(diag_data2d_store%stores(var_index)%storage(data_index)%rsingle) * 0.5
                    call nc_diag_realloc(diag_data2d_store%stores(var_index)%storage(data_index)%rsingle, new_size)
                    diag_data2d_store%stores(var_index)%storage(data_index)%asize = new_size
                end if
            else
                diag_data2d_store%stores(var_index)%storage(data_index)%acount = addl_num_entries
                allocate(diag_data2d_store%stores(var_index)%storage(data_index)%rsingle(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_data2d_store%stores(var_index)%storage(data_index)%asize = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_data2d_resize_rsingle
        
        ! nc_diag_data2d_resize - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_data2d_resize_rdouble(var_index, data_index, addl_num_entries)
            integer(i_llong), intent(in)     :: var_index
            integer(i_llong), intent(in)     :: data_index
            integer(i_llong), intent(in)     :: addl_num_entries
            
            integer(i_llong)                 :: new_size
            
            if (allocated(diag_data2d_store%stores(var_index)%storage(data_index)%rdouble)) then
                diag_data2d_store%stores(var_index)%storage(data_index)%acount = addl_num_entries
                
                if (diag_data2d_store%stores(var_index)%storage(data_index)%acount >= &
                    diag_data2d_store%stores(var_index)%storage(data_index)%asize) then
#ifdef ENABLE_ACTION_MSGS
                    if (enable_action) then
                        call actionm("nc_diag_data2d_resize_rdouble: doing reallocation!")
                    end if
#endif
                    new_size = size(diag_data2d_store%stores(var_index)%storage(data_index)%rdouble) * 0.5
                    call nc_diag_realloc(diag_data2d_store%stores(var_index)%storage(data_index)%rdouble, new_size)
                    diag_data2d_store%stores(var_index)%storage(data_index)%asize = new_size
                end if
            else
                diag_data2d_store%stores(var_index)%storage(data_index)%acount = addl_num_entries
                allocate(diag_data2d_store%stores(var_index)%storage(data_index)%rdouble(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_data2d_store%stores(var_index)%storage(data_index)%asize = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_data2d_resize_rdouble

        ! nc_diag_data2d_resize - input character(len=*)
        ! Corresponding NetCDF4 type: string? char?
        subroutine nc_diag_data2d_resize_string(var_index, data_index, addl_num_entries)
            integer(i_llong), intent(in)     :: var_index
            integer(i_llong), intent(in)     :: data_index
            integer(i_llong), intent(in)     :: addl_num_entries
            
            integer(i_llong)                 :: new_size
            
            if (.NOT. associated(diag_data2d_store%stores(var_index)%storage)) then
                allocate(diag_data2d_store%stores(var_index)%storage(NLAYER_DEFAULT_ENT))
                diag_data2d_store%stores(var_index)%acount = 1
                diag_data2d_store%stores(var_index)%asize = NLAYER_DEFAULT_ENT
            end if
            
            if (allocated(diag_data2d_store%stores(var_index)%storage(data_index)%string)) then
                diag_data2d_store%stores(var_index)%storage(data_index)%acount = addl_num_entries
                
                if (diag_data2d_store%stores(var_index)%storage(data_index)%acount >= &
                    diag_data2d_store%stores(var_index)%storage(data_index)%asize) then
#ifdef ENABLE_ACTION_MSGS
                    if (enable_action) then
                        call actionm("nc_diag_data2d_resize_string: doing reallocation!")
                    end if
#endif
                    new_size = size(diag_data2d_store%stores(var_index)%storage(data_index)%string) * 0.5
                    call nc_diag_realloc(diag_data2d_store%stores(var_index)%storage(data_index)%string, new_size)
                    diag_data2d_store%stores(var_index)%storage(data_index)%asize = new_size
                end if
            else
                diag_data2d_store%stores(var_index)%storage(data_index)%acount = addl_num_entries
                allocate(diag_data2d_store%stores(var_index)%storage(data_index)%string(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_data2d_store%stores(var_index)%storage(data_index)%asize = addl_num_entries + NLAYER_DEFAULT_ENT
                diag_data2d_store%stores(var_index)%storage(data_index)%is_data = .TRUE.
                diag_data2d_store%stores(var_index)%storage(data_index)%data_type = NLAYER_STRING
            end if
        end subroutine nc_diag_data2d_resize_string
        
        subroutine nc_diag_data2d_resize_storage_type(addl_num_entries)
            integer(i_llong), intent(in)    :: addl_num_entries
            
            type(diag_d_storage), dimension(:), allocatable   :: tmp_stor_arr
            
#ifdef ENABLE_ACTION_MSGS
            if (enable_action) then
                call actionm("nc_diag_data2d_resize_storage: doing reallocation!")
            end if
#endif
            
            ! We need to realloc ourselves here...
            allocate(tmp_stor_arr(size(diag_data2d_store%stores) + addl_num_entries))
            tmp_stor_arr(1:size(diag_data2d_store%stores)) = diag_data2d_store%stores
            deallocate(diag_data2d_store%stores)
            allocate(diag_data2d_store%stores(size(tmp_stor_arr)))
            diag_data2d_store%stores = tmp_stor_arr
            deallocate(tmp_stor_arr)
        end subroutine nc_diag_data2d_resize_storage_type
        
        subroutine nc_diag_data2d_resize_internal_storage(var_index, current_index)
            integer(i_llong), intent(in)    :: var_index
            integer(i_llong), intent(in)    :: current_index
            
            type(diag_d_storage), dimension(:), allocatable   :: tmp_stor_arr
            
#ifdef _DEBUG_MEM_
            print *, "int storage call"
#endif
            
            if (associated(diag_data2d_store%stores(var_index)%storage)) then
#ifdef _DEBUG_MEM_
                print *, "associated!"
                print *, "current_index = ", current_index
                print *, "diag_data2d_store%stores(var_index)%asize = ", diag_data2d_store%stores(var_index)%asize
                print *, "diag_data2d_store%stores(var_index)%acount = ", diag_data2d_store%stores(var_index)%acount
#endif
                if (current_index > diag_data2d_store%stores(var_index)%asize) then
#ifdef _DEBUG_MEM_
                    print *, "resize needed for data2d internal storage!"
#endif
#ifdef ENABLE_ACTION_MSGS
                    if (enable_action) then
                        call actionm("nc_diag_data2d_resize_internal_storage: doing reallocation!")
                    end if
#endif
                    ! We need to realloc ourselves here...
                    allocate(tmp_stor_arr(int8(current_index * 2)))
                    tmp_stor_arr(1:size(diag_data2d_store%stores(var_index)%storage)) = diag_data2d_store%stores(var_index)%storage
                    deallocate(diag_data2d_store%stores(var_index)%storage)
                    allocate(diag_data2d_store%stores(var_index)%storage(size(tmp_stor_arr)))
                    diag_data2d_store%stores(var_index)%storage = tmp_stor_arr
                    deallocate(tmp_stor_arr)
                    diag_data2d_store%stores(var_index)%acount = current_index
                end if
                
                if (current_index > diag_data2d_store%stores(var_index)%acount) then
                    diag_data2d_store%stores(var_index)%acount = current_index
                end if
            else
                allocate(diag_data2d_store%stores(var_index)%storage(current_index + NLAYER_DEFAULT_ENT))
                diag_data2d_store%stores(var_index)%acount = 1
            end if
            diag_data2d_store%stores(var_index)%asize = size(diag_data2d_store%stores(var_index)%storage)
            
        end subroutine nc_diag_data2d_resize_internal_storage
