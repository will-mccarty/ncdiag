        !===============================================================
        ! nc_diag_chaninfo_resize - channel info resizing support (impl)
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
        ! nc_diag_chaninfo_resize subroutines resize the arrays inside
        ! the diag_chaninfo type.
        !---------------------------------------------------------------
        ! This file provides the actual subroutines, referred to by the
        ! interface.
        
        ! nc_diag_chaninfo_resize - input integer(i_byte)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_chaninfo_resize_byte(addl_num_entries, update_acount_in)
            integer(i_llong), intent(in)    :: addl_num_entries
            logical, intent(in), optional   :: update_acount_in
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            
            logical                         :: update_acount
            
            ! Assume true by default
            if (.NOT. present(update_acount_in)) then
                update_acount = .TRUE.
            else
                update_acount = update_acount_in
            end if
            
            ! Here, we increment the count by the number of additional entries,
            ! and the size by that amount as well. 
            ! 
            ! If we didn't allocate yet, we simply set the count to the number of
            ! initial entries, and then allocate that number + our default
            ! initialization amount. Our initial size is that number + the initial
            ! amount.
            
            ! NLAYER_BYTE is located at the first index, 1.
            sc_index = 1
            
            if (allocated(diag_chaninfo_store%ci_byte)) then
                if (update_acount) diag_chaninfo_store%acount(sc_index) = diag_chaninfo_store%acount(sc_index) + addl_num_entries
                if (diag_chaninfo_store%acount(sc_index) >= diag_chaninfo_store%asize(sc_index)) then
#ifdef ENABLE_ACTION_MSGS
            if (enable_action) then
                call actionm("nc_diag_chaninfo_resize_byte: doing reallocation!")
            end if
#endif
                    call nc_diag_realloc(diag_chaninfo_store%ci_byte, int8(addl_num_entries + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_chaninfo_store%alloc_multi))))
                    diag_chaninfo_store%asize(sc_index) = size(diag_chaninfo_store%ci_byte)
                    
                    diag_chaninfo_store%alloc_multi = diag_chaninfo_store%alloc_multi + 1
                end if
            else
                if (update_acount) diag_chaninfo_store%acount(sc_index) = addl_num_entries
                allocate(diag_chaninfo_store%ci_byte(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_chaninfo_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_chaninfo_resize_byte
        
        ! nc_diag_chaninfo_resize - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_chaninfo_resize_short(addl_num_entries, update_acount_in)
            integer(i_llong), intent(in)    :: addl_num_entries
            logical, intent(in), optional   :: update_acount_in
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            
            logical                         :: update_acount
            
            ! Assume true by default
            if (.NOT. present(update_acount_in)) then
                update_acount = .TRUE.
            else
                update_acount = update_acount_in
            end if
            
            ! Here, we increment the count by the number of additional entries,
            ! and the size by that amount as well. 
            ! 
            ! If we didn't allocate yet, we simply set the count to the number of
            ! initial entries, and then allocate that number + our default
            ! initialization amount. Our initial size is that number + the initial
            ! amount.
            
            ! NLAYER_SHORT is located at the second index, 2.
            sc_index = 2
            
            if (allocated(diag_chaninfo_store%ci_short)) then
                if (update_acount) diag_chaninfo_store%acount(sc_index) = diag_chaninfo_store%acount(sc_index) + addl_num_entries
                if (diag_chaninfo_store%acount(sc_index) >= diag_chaninfo_store%asize(sc_index)) then
#ifdef ENABLE_ACTION_MSGS
                    if (enable_action) then
                        call actionm("nc_diag_chaninfo_resize_short: doing reallocation!")
                    end if
#endif
                    call nc_diag_realloc(diag_chaninfo_store%ci_short, int8(addl_num_entries + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_chaninfo_store%alloc_multi))))
                    diag_chaninfo_store%asize(sc_index) = size(diag_chaninfo_store%ci_short)
                    
                    diag_chaninfo_store%alloc_multi = diag_chaninfo_store%alloc_multi + 1
                end if
            else
                if (update_acount) diag_chaninfo_store%acount(sc_index) = addl_num_entries
                allocate(diag_chaninfo_store%ci_short(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_chaninfo_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_chaninfo_resize_short
        
        ! nc_diag_chaninfo_resize - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_chaninfo_resize_long(addl_num_entries, update_acount_in)
            integer(i_llong), intent(in)    :: addl_num_entries
            logical, intent(in), optional   :: update_acount_in
            
            ! Did we realloc at all?
            !logical :: chaninfo_realloc
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            
            logical                         :: update_acount
            
            ! Assume true by default
            if (.NOT. present(update_acount_in)) then
                update_acount = .TRUE.
            else
                update_acount = update_acount_in
            end if
            
            ! Default is false - no realloc done. 
            !chaninfo_realloc = .FALSE.
            
            ! Here, we increment the count by the number of additional entries,
            ! and the size by that amount as well. 
            ! 
            ! If we didn't allocate yet, we simply set the count to the number of
            ! initial entries, and then allocate that number + our default
            ! initialization amount. Our initial size is that number + the initial
            ! amount.
            
            ! NLAYER_LONG is located at the third index, 3.
            sc_index = 3
            
            if (allocated(diag_chaninfo_store%ci_long)) then
                if (update_acount) diag_chaninfo_store%acount(sc_index) = diag_chaninfo_store%acount(sc_index) + addl_num_entries
                
                if (diag_chaninfo_store%acount(sc_index) >= diag_chaninfo_store%asize(sc_index)) then
#ifdef _DEBUG_MEM_
                    print *, "realloc needed for chaninfo long!"
                    write (*, "(A, I0, A, I0, A)") "(size needed / size available: ", diag_chaninfo_store%acount(sc_index), " / ", diag_chaninfo_store%asize(sc_index), ")"
#endif
#ifdef ENABLE_ACTION_MSGS
                    if (enable_action) then
                        call actionm("nc_diag_chaninfo_resize_long: doing reallocation!")
                    end if
#endif
                    call nc_diag_realloc(diag_chaninfo_store%ci_long, int8(addl_num_entries + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_chaninfo_store%alloc_multi))))
                    diag_chaninfo_store%asize(sc_index) = size(diag_chaninfo_store%ci_long)
                    
                    diag_chaninfo_store%alloc_multi = diag_chaninfo_store%alloc_multi + 1
                end if
            else
                if (update_acount) diag_chaninfo_store%acount(sc_index) = addl_num_entries
                allocate(diag_chaninfo_store%ci_long(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_chaninfo_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_chaninfo_resize_long
        
        ! nc_diag_chaninfo_resize - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_chaninfo_resize_rsingle(addl_num_entries, update_acount_in)
            integer(i_llong), intent(in)    :: addl_num_entries
            logical, intent(in), optional   :: update_acount_in
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            
            logical                         :: update_acount
            
            ! Assume true by default
            if (.NOT. present(update_acount_in)) then
                update_acount = .TRUE.
            else
                update_acount = update_acount_in
            end if
            
            ! Here, we increment the count by the number of additional entries,
            ! and the size by that amount as well. 
            ! 
            ! If we didn't allocate yet, we simply set the count to the number of
            ! initial entries, and then allocate that number + our default
            ! initialization amount. Our initial size is that number + the initial
            ! amount.
            
            ! NLAYER_FLOAT is located at the fourth index, 4.
            sc_index = 4
            
            if (allocated(diag_chaninfo_store%ci_rsingle)) then
                if (update_acount) diag_chaninfo_store%acount(sc_index) = diag_chaninfo_store%acount(sc_index) + addl_num_entries
                if (diag_chaninfo_store%acount(sc_index) >= diag_chaninfo_store%asize(sc_index)) then
#ifdef ENABLE_ACTION_MSGS
                    if (enable_action) then
                        call actionm("nc_diag_chaninfo_resize_rsingle: doing reallocation!")
                    end if
#endif
                    call nc_diag_realloc(diag_chaninfo_store%ci_rsingle, int8(addl_num_entries + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_chaninfo_store%alloc_multi))))
                    diag_chaninfo_store%asize(sc_index) = size(diag_chaninfo_store%ci_rsingle)
                    
                    diag_chaninfo_store%alloc_multi = diag_chaninfo_store%alloc_multi + 1
                end if
            else
                if (update_acount) diag_chaninfo_store%acount(sc_index) = addl_num_entries
                allocate(diag_chaninfo_store%ci_rsingle(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_chaninfo_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_chaninfo_resize_rsingle
        
        ! nc_diag_chaninfo_resize - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_chaninfo_resize_rdouble(addl_num_entries, update_acount_in)
            integer(i_llong), intent(in)    :: addl_num_entries
            logical, intent(in), optional   :: update_acount_in
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            
            logical                         :: update_acount
            
            ! Assume true by default
            if (.NOT. present(update_acount_in)) then
                update_acount = .TRUE.
            else
                update_acount = update_acount_in
            end if
            
            ! Here, we increment the count by the number of additional entries,
            ! and the size by that amount as well. 
            ! 
            ! If we didn't allocate yet, we simply set the count to the number of
            ! initial entries, and then allocate that number + our default
            ! initialization amount. Our initial size is that number + the initial
            ! amount.
            
            ! NLAYER_DOUBLE is located at the fifth index, 5.
            sc_index = 5
            
            if (allocated(diag_chaninfo_store%ci_rdouble)) then
                if (update_acount) diag_chaninfo_store%acount(sc_index) = diag_chaninfo_store%acount(sc_index) + addl_num_entries
                if (diag_chaninfo_store%acount(sc_index) >= diag_chaninfo_store%asize(sc_index)) then
#ifdef ENABLE_ACTION_MSGS
                    if (enable_action) then
                        call actionm("nc_diag_chaninfo_resize_rdouble: doing reallocation!")
                    end if
#endif
                    call nc_diag_realloc(diag_chaninfo_store%ci_rdouble, int8(addl_num_entries + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_chaninfo_store%alloc_multi))))
                    diag_chaninfo_store%asize(sc_index) = size(diag_chaninfo_store%ci_rdouble)
                    
                    diag_chaninfo_store%alloc_multi = diag_chaninfo_store%alloc_multi + 1
                end if
            else
                if (update_acount) diag_chaninfo_store%acount(sc_index) = addl_num_entries
                allocate(diag_chaninfo_store%ci_rdouble(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_chaninfo_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_chaninfo_resize_rdouble

        ! nc_diag_chaninfo_resize - input character(len=*)
        ! Corresponding NetCDF4 type: string? char?
        subroutine nc_diag_chaninfo_resize_string(addl_num_entries, update_acount_in)
            integer(i_llong), intent(in)    :: addl_num_entries
            logical, intent(in), optional   :: update_acount_in
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            
            logical                         :: update_acount
            
            ! Assume true by default
            if (.NOT. present(update_acount_in)) then
                update_acount = .TRUE.
            else
                update_acount = update_acount_in
            end if
            
            ! Here, we increment the count by the number of additional entries,
            ! and the size by that amount as well. 
            ! 
            ! If we didn't allocate yet, we simply set the count to the number of
            ! initial entries, and then allocate that number + our default
            ! initialization amount. Our initial size is that number + the initial
            ! amount.
            
            ! NLAYER_STRING is located at the sixth index, 6.
            sc_index = 6
            
            if (allocated(diag_chaninfo_store%ci_string)) then
                if (update_acount) diag_chaninfo_store%acount(sc_index) = diag_chaninfo_store%acount(sc_index) + addl_num_entries
                if (diag_chaninfo_store%acount(sc_index) >= diag_chaninfo_store%asize(sc_index)) then
#ifdef ENABLE_ACTION_MSGS
                    if (enable_action) then
                        call actionm("nc_diag_chaninfo_resize_string: doing reallocation!")
                    end if
#endif
                    call nc_diag_realloc(diag_chaninfo_store%ci_string, int8(addl_num_entries + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_chaninfo_store%alloc_multi))))
                    diag_chaninfo_store%asize(sc_index) = size(diag_chaninfo_store%ci_string)
                    
                    diag_chaninfo_store%alloc_multi = diag_chaninfo_store%alloc_multi + 1
                end if
            else
                if (update_acount) diag_chaninfo_store%acount(sc_index) = addl_num_entries
                allocate(diag_chaninfo_store%ci_string(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_chaninfo_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
        end subroutine nc_diag_chaninfo_resize_string
        
