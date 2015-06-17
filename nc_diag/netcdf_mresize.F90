        !===============================================================
        ! nc_diag_metadata_resize - metadata resizing support
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
        ! nc_diag_metadata_resize subroutines resize the arrays inside the
        ! diag_metadata type.
        !---------------------------------------------------------------
        ! This file provides the actual subroutines for metadata array
        ! resizing, referred to by the metadata resizing interface.
        !---------------------------------------------------------------
        
        ! nc_diag_metadata_resize - input integer(i_byte)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_metadata_resize_byte(addl_num_entries, elevector)
            integer(i_long), intent(in)     :: addl_num_entries
            logical, intent(in)             :: elevector
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            integer(i_long)                 :: sc_index_vi
            
            ! Here, we increment the count by the number of additional entries,
            ! and the size by that amount as well. 
            ! 
            ! If we didn't allocate yet, we simply set the count to the number of
            ! initial entries, and then allocate that number + our default
            ! initialization amount. Our initial size is that number + the initial
            ! amount.
            
            ! NLAYER_BYTE is located at the first index, 1.
            ! sc_index_vi is just sc_index + 6, 6 being the number of single types
            sc_index = 1
            sc_index_vi = sc_index + 6
            
            if (allocated(diag_metadata_store%m_byte)) then
                diag_metadata_store%acount(sc_index) = diag_metadata_store%acount(sc_index) + addl_num_entries
                if (diag_metadata_store%acount(sc_index) >= diag_metadata_store%asize(sc_index)) then
                    call nc_diag_realloc(diag_metadata_store%m_byte, addl_num_entries + (NLAYER_DEFAULT_ENT * (2 ** diag_metadata_store%alloc_m_multi(sc_index))))
                    diag_metadata_store%asize(sc_index) = size(diag_metadata_store%m_byte)
                    
                    diag_metadata_store%alloc_m_multi(sc_index) = diag_metadata_store%alloc_m_multi(sc_index) + 1
                end if
            else
                diag_metadata_store%acount(sc_index) = addl_num_entries
                allocate(diag_metadata_store%m_byte(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_metadata_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
            
            if (elevector) then
                ! Element 6 is the last "single" element.
                ! Element 7 and beyond stores the vector size/count info.
                ! 
                ! The range is from element 7 to element 12.
                ! (7, 8, 9, 10, 11, 12)
                ! 
                ! Note that here we're just storing the index info - so we're only
                ! ever going to increase the size by 1.
                
                if (allocated(diag_metadata_store%m_byte_vi)) then
                    diag_metadata_store%acount(sc_index_vi) = diag_metadata_store%acount(sc_index_vi) + 1
                    if (diag_metadata_store%acount(sc_index_vi) >= diag_metadata_store%asize(sc_index_vi)) then
                        call nc_diag_realloc(diag_metadata_store%m_byte_vi, 1 + (NLAYER_DEFAULT_ENT * (2 ** diag_metadata_store%alloc_hi_multi(sc_index))))
                        diag_metadata_store%asize(sc_index_vi) = diag_metadata_store%acount(sc_index_vi)
                        
                        diag_metadata_store%alloc_hi_multi(sc_index) = diag_metadata_store%alloc_hi_multi(sc_index) + 1
                    end if
                else
                    diag_metadata_store%acount(sc_index_vi) = 1
                    allocate(diag_metadata_store%m_byte_vi(NLAYER_DEFAULT_ENT))
                    diag_metadata_store%asize(sc_index_vi) = NLAYER_DEFAULT_ENT
                end if
            end if
        end subroutine nc_diag_metadata_resize_byte
        
        ! nc_diag_metadata_resize - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_metadata_resize_short(addl_num_entries, elevector)
            integer(i_long), intent(in)     :: addl_num_entries
            logical, intent(in)             :: elevector
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            integer(i_long)                 :: sc_index_vi
            
            ! Here, we increment the count by the number of additional entries,
            ! and the size by that amount as well. 
            ! 
            ! If we didn't allocate yet, we simply set the count to the number of
            ! initial entries, and then allocate that number + our default
            ! initialization amount. Our initial size is that number + the initial
            ! amount.
            
            ! NLAYER_SHORT is located at the second index, 2.
            ! sc_index_vi is just sc_index + 6, 6 being the number of single types
            sc_index = 2
            sc_index_vi = sc_index + 6
            
            if (allocated(diag_metadata_store%m_short)) then
                diag_metadata_store%acount(sc_index) = diag_metadata_store%acount(sc_index) + addl_num_entries
                if (diag_metadata_store%acount(sc_index) >= diag_metadata_store%asize(sc_index)) then
                    call nc_diag_realloc(diag_metadata_store%m_short, addl_num_entries + (NLAYER_DEFAULT_ENT * (2 ** diag_metadata_store%alloc_m_multi(sc_index))))
                    diag_metadata_store%asize(sc_index) = size(diag_metadata_store%m_short)
                    
                    diag_metadata_store%alloc_m_multi(sc_index) = diag_metadata_store%alloc_m_multi(sc_index) + 1
                end if
            else
                diag_metadata_store%acount(sc_index) = addl_num_entries
                allocate(diag_metadata_store%m_short(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_metadata_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
            
            if (elevector) then
                ! Element 6 is the last "single" element.
                ! Element 7 and beyond stores the vector size/count info.
                ! 
                ! The range is from element 7 to element 12.
                ! (7, 8, 9, 10, 11, 12)
                ! 
                ! Note that here we're just storing the index info - so we're only
                ! ever going to increase the size by 1.
                
                if (allocated(diag_metadata_store%m_short_vi)) then
                    diag_metadata_store%acount(sc_index_vi) = diag_metadata_store%acount(sc_index_vi) + 1
                    if (diag_metadata_store%acount(sc_index_vi) >= diag_metadata_store%asize(sc_index_vi)) then
                        call nc_diag_realloc(diag_metadata_store%m_short_vi, 1 + (NLAYER_DEFAULT_ENT * (2 ** diag_metadata_store%alloc_hi_multi(sc_index))))
                        diag_metadata_store%asize(sc_index_vi) = diag_metadata_store%acount(sc_index_vi)
                        
                        diag_metadata_store%alloc_hi_multi(sc_index) = diag_metadata_store%alloc_hi_multi(sc_index) + 1
                    end if
                else
                    diag_metadata_store%acount(sc_index_vi) = 1
                    allocate(diag_metadata_store%m_short_vi(NLAYER_DEFAULT_ENT))
                    diag_metadata_store%asize(sc_index_vi) = NLAYER_DEFAULT_ENT
                end if
            end if
        end subroutine nc_diag_metadata_resize_short
        
        ! nc_diag_metadata_resize - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_metadata_resize_long(addl_num_entries, elevector)
            integer(i_long), intent(in)     :: addl_num_entries
            logical, intent(in)             :: elevector
            
            ! Did we realloc at all?
            !logical :: metadata_realloc
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            integer(i_long)                 :: sc_index_vi
            
#ifdef _DEBUG_MEM_
            character(len=200) :: debugstr
#endif
            
            ! Default is false - no realloc done. 
            !metadata_realloc = .FALSE.
            
            ! Here, we increment the count by the number of additional entries,
            ! and the size by that amount as well. 
            ! 
            ! If we didn't allocate yet, we simply set the count to the number of
            ! initial entries, and then allocate that number + our default
            ! initialization amount. Our initial size is that number + the initial
            ! amount.
            
            ! NLAYER_LONG is located at the third index, 3.
            ! sc_index_vi is just sc_index + 6, 6 being the number of single types
            sc_index = 3
            sc_index_vi = sc_index + 6
            
            if (allocated(diag_metadata_store%m_long)) then
                diag_metadata_store%acount(sc_index) = diag_metadata_store%acount(sc_index) + addl_num_entries
                
#ifdef _DEBUG_MEM_
                write (debugstr, "(A, I1, A, I7, A, I7)") "In sc_index ", sc_index, ", the acount/asize is: ", diag_metadata_store%acount(sc_index), "/", diag_metadata_store%asize(sc_index)
                call debug(debugstr)
#endif
                
                if (diag_metadata_store%acount(sc_index) >= diag_metadata_store%asize(sc_index)) then
#ifdef _DEBUG_MEM_
                    call debug("acount < asize, reallocating.")
                    print *, "Start long realloc..."
#endif
                    call nc_diag_realloc(diag_metadata_store%m_long, addl_num_entries + (NLAYER_DEFAULT_ENT * (2 ** diag_metadata_store%alloc_m_multi(sc_index))))
                    diag_metadata_store%asize(sc_index) = size(diag_metadata_store%m_long)
                    
                    diag_metadata_store%alloc_m_multi(sc_index) = diag_metadata_store%alloc_m_multi(sc_index) + 1
                    
#ifdef _DEBUG_MEM_
                    print *, "alloc_m_multi increased to:"
                    print *, diag_metadata_store%alloc_m_multi(sc_index)
#endif
                end if
            else
                diag_metadata_store%acount(sc_index) = addl_num_entries
                allocate(diag_metadata_store%m_long(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_metadata_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
            
            if (elevector) then
                ! Element 6 is the last "single" element.
                ! Element 7 and beyond stores the vector size/count info.
                ! 
                ! The range is from element 7 to element 12.
                ! (7, 8, 9, 10, 11, 12)
                ! 
                ! Note that here we're just storing the index info - so we're only
                ! ever going to increase the size by 1.
                
                if (allocated(diag_metadata_store%m_long_vi)) then
                    diag_metadata_store%acount(sc_index_vi) = diag_metadata_store%acount(sc_index_vi) + 1
                    if (diag_metadata_store%acount(sc_index_vi) >= diag_metadata_store%asize(sc_index_vi)) then
                        call nc_diag_realloc(diag_metadata_store%m_long_vi, 1 + (NLAYER_DEFAULT_ENT * (2 ** diag_metadata_store%alloc_hi_multi(sc_index))))
                        !diag_metadata_store%asize(sc_index_vi) = diag_metadata_store%acount(sc_index_vi) + NLAYER_DEFAULT_ENT
                        diag_metadata_store%asize(sc_index_vi) = size(diag_metadata_store%m_long_vi)
                        
                        diag_metadata_store%alloc_hi_multi(sc_index) = diag_metadata_store%alloc_hi_multi(sc_index) + 1
#ifdef _DEBUG_MEM_
                        print *, "alloc_hi_multi increased to:"
                        print *, diag_metadata_store%alloc_hi_multi(sc_index)
#endif
                    end if
                else
                    diag_metadata_store%acount(sc_index_vi) = 1
                    allocate(diag_metadata_store%m_long_vi(NLAYER_DEFAULT_ENT))
                    diag_metadata_store%asize(sc_index_vi) = NLAYER_DEFAULT_ENT
                end if
            end if
        end subroutine nc_diag_metadata_resize_long
        
        ! nc_diag_metadata_resize - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_metadata_resize_rsingle(addl_num_entries, elevector)
            integer(i_long), intent(in)     :: addl_num_entries
            logical, intent(in)             :: elevector
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            integer(i_long)                 :: sc_index_vi
            
            ! Here, we increment the count by the number of additional entries,
            ! and the size by that amount as well. 
            ! 
            ! If we didn't allocate yet, we simply set the count to the number of
            ! initial entries, and then allocate that number + our default
            ! initialization amount. Our initial size is that number + the initial
            ! amount.
            
            ! NLAYER_FLOAT is located at the fourth index, 4.
            ! sc_index_vi is just sc_index + 6, 6 being the number of single types
            sc_index = 4
            sc_index_vi = sc_index + 6
            
            if (allocated(diag_metadata_store%m_rsingle)) then
                diag_metadata_store%acount(sc_index) = diag_metadata_store%acount(sc_index) + addl_num_entries
                if (diag_metadata_store%acount(sc_index) >= diag_metadata_store%asize(sc_index)) then
                    call nc_diag_realloc(diag_metadata_store%m_rsingle, addl_num_entries + (NLAYER_DEFAULT_ENT * (2 ** diag_metadata_store%alloc_m_multi(sc_index))))
                    diag_metadata_store%asize(sc_index) = size(diag_metadata_store%m_rsingle)
                    
                    diag_metadata_store%alloc_m_multi(sc_index) = diag_metadata_store%alloc_m_multi(sc_index) + 1
                end if
            else
                diag_metadata_store%acount(sc_index) = addl_num_entries
                allocate(diag_metadata_store%m_rsingle(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_metadata_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
            
            if (elevector) then
                ! Element 6 is the last "single" element.
                ! Element 7 and beyond stores the vector size/count info.
                ! 
                ! The range is from element 7 to element 12.
                ! (7, 8, 9, 10, 11, 12)
                ! 
                ! Note that here we're just storing the index info - so we're only
                ! ever going to increase the size by 1.
                
                if (allocated(diag_metadata_store%m_rsingle_vi)) then
                    diag_metadata_store%acount(sc_index_vi) = diag_metadata_store%acount(sc_index_vi) + 1
                    if (diag_metadata_store%acount(sc_index_vi) >= diag_metadata_store%asize(sc_index_vi)) then
                        call nc_diag_realloc(diag_metadata_store%m_rsingle_vi, 1 + (NLAYER_DEFAULT_ENT * (2 ** diag_metadata_store%alloc_hi_multi(sc_index))))
                        diag_metadata_store%asize(sc_index_vi) = diag_metadata_store%acount(sc_index_vi)
                        
                        diag_metadata_store%alloc_hi_multi(sc_index) = diag_metadata_store%alloc_hi_multi(sc_index) + 1
                    end if
                else
                    diag_metadata_store%acount(sc_index_vi) = 1
                    allocate(diag_metadata_store%m_rsingle_vi(NLAYER_DEFAULT_ENT))
                    diag_metadata_store%asize(sc_index_vi) = NLAYER_DEFAULT_ENT
                end if
            end if
        end subroutine nc_diag_metadata_resize_rsingle
        
        ! nc_diag_metadata_resize - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_metadata_resize_rdouble(addl_num_entries, elevector)
            integer(i_long), intent(in)     :: addl_num_entries
            logical, intent(in)             :: elevector
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            integer(i_long)                 :: sc_index_vi
            
            ! Here, we increment the count by the number of additional entries,
            ! and the size by that amount as well. 
            ! 
            ! If we didn't allocate yet, we simply set the count to the number of
            ! initial entries, and then allocate that number + our default
            ! initialization amount. Our initial size is that number + the initial
            ! amount.
            
            ! NLAYER_DOUBLE is located at the fifth index, 5.
            ! sc_index_vi is just sc_index + 6, 6 being the number of single types
            sc_index = 5
            sc_index_vi = sc_index + 6
            
            if (allocated(diag_metadata_store%m_rdouble)) then
                diag_metadata_store%acount(sc_index) = diag_metadata_store%acount(sc_index) + addl_num_entries
                if (diag_metadata_store%acount(sc_index) >= diag_metadata_store%asize(sc_index)) then
                    call nc_diag_realloc(diag_metadata_store%m_rdouble, addl_num_entries + (NLAYER_DEFAULT_ENT * (2 ** diag_metadata_store%alloc_m_multi(sc_index))))
                    diag_metadata_store%asize(sc_index) = size(diag_metadata_store%m_rdouble)
                    
                    diag_metadata_store%alloc_m_multi(sc_index) = diag_metadata_store%alloc_m_multi(sc_index) + 1
                end if
            else
                diag_metadata_store%acount(sc_index) = addl_num_entries
                allocate(diag_metadata_store%m_rdouble(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_metadata_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
            
            if (elevector) then
                ! Element 6 is the last "single" element.
                ! Element 7 and beyond stores the vector size/count info.
                ! 
                ! The range is from element 7 to element 12.
                ! (7, 8, 9, 10, 11, 12)
                ! 
                ! Note that here we're just storing the index info - so we're only
                ! ever going to increase the size by 1.
                
                if (allocated(diag_metadata_store%m_rdouble_vi)) then
                    diag_metadata_store%acount(sc_index_vi) = diag_metadata_store%acount(sc_index_vi) + 1
                    if (diag_metadata_store%acount(sc_index_vi) >= diag_metadata_store%asize(sc_index_vi)) then
                        call nc_diag_realloc(diag_metadata_store%m_rdouble_vi, 1 + (NLAYER_DEFAULT_ENT * (2 ** diag_metadata_store%alloc_hi_multi(sc_index))))
                        diag_metadata_store%asize(sc_index_vi) = diag_metadata_store%acount(sc_index_vi)
                        
                        diag_metadata_store%alloc_hi_multi(sc_index) = diag_metadata_store%alloc_hi_multi(sc_index) + 1
                    end if
                else
                    diag_metadata_store%acount(sc_index_vi) = 1
                    allocate(diag_metadata_store%m_rdouble_vi(NLAYER_DEFAULT_ENT))
                    diag_metadata_store%asize(sc_index_vi) = NLAYER_DEFAULT_ENT
                end if
            end if
        end subroutine nc_diag_metadata_resize_rdouble

        ! nc_diag_metadata_resize - input character(len=*)
        ! Corresponding NetCDF4 type: string? char?
        subroutine nc_diag_metadata_resize_string(addl_num_entries)
            integer(i_long), intent(in)     :: addl_num_entries
            
            ! This is the Size Count index (sc_index) - we'll just set
            ! this and then just change the variable we're altering
            ! every time.
            integer(i_long)                 :: sc_index
            integer(i_long)                 :: sc_index_vi
            
            ! Here, we increment the count by the number of additional entries,
            ! and the size by that amount as well. 
            ! 
            ! If we didn't allocate yet, we simply set the count to the number of
            ! initial entries, and then allocate that number + our default
            ! initialization amount. Our initial size is that number + the initial
            ! amount.
            
            ! NLAYER_BYTE is located at the sixth index, 6.
            ! sc_index_vi is just sc_index + 6, 6 being the number of single types
            sc_index = 6
            sc_index_vi = sc_index + 6
            
            if (allocated(diag_metadata_store%m_string)) then
                diag_metadata_store%acount(sc_index) = diag_metadata_store%acount(sc_index) + addl_num_entries
                if (diag_metadata_store%acount(sc_index) >= diag_metadata_store%asize(sc_index)) then
                    call nc_diag_realloc(diag_metadata_store%m_string, addl_num_entries  + (NLAYER_DEFAULT_ENT * (2 ** diag_metadata_store%alloc_m_multi(sc_index))))
                    diag_metadata_store%asize(sc_index) = size(diag_metadata_store%m_string)
                    
                    diag_metadata_store%alloc_m_multi(sc_index) = diag_metadata_store%alloc_m_multi(sc_index) + 1
                end if
            else
                diag_metadata_store%acount(sc_index) = addl_num_entries
                allocate(diag_metadata_store%m_string(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_metadata_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
            
            ! String array not available with NF90 attributes
            !if (elevector) then
            !    ! Element 6 is the last "single" element.
            !    ! Element 7 and beyond stores the vector size/count info.
            !    ! 
            !    ! The range is from element 7 to element 12.
            !    ! (7, 8, 9, 10, 11, 12)
            !    ! 
            !    ! Note that here we're just storing the index info - so we're only
            !    ! ever going to increase the size by 1.
            !    
            !    if (allocated(diag_metadata_store%m_string_vi)) then
            !        diag_metadata_store%acount(sc_index_vi) = diag_metadata_store%acount(sc_index_vi) + 1
            !        if (diag_metadata_store%acount(sc_index_vi) >= diag_metadata_store%asize(sc_index_vi)) then
            !            call nc_diag_realloc(diag_metadata_store%m_string_vi, 1)
            !            diag_metadata_store%asize(sc_index_vi) = diag_metadata_store%acount(sc_index_vi)
            !            
            !            diag_metadata_store%alloc_hi_multi(sc_index) = diag_metadata_store%alloc_hi_multi(sc_index) + 1
            !        end if
            !    else
            !        diag_metadata_store%acount(sc_index_vi) = 1
            !        allocate(diag_metadata_store%m_string_vi(NLAYER_DEFAULT_ENT))
            !        diag_metadata_store%asize(sc_index_vi) = NLAYER_DEFAULT_ENT
            !    end if
            !end if
        end subroutine nc_diag_metadata_resize_string
        
