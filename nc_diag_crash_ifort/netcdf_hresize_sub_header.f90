        ! ---------------------------
        ! nc_diag_header definitions
        ! ---------------------------
        ! DO NOT COMPILE THIS DIRECTLY! THIS IS MEANT TO BE INCLUDED
        ! INSIDE A LARGER F90 SOURCE!
        ! If you compile this directly, you WILL face the WRATH of your
        ! compiler!
        
        ! Depends on: netcdf_realloc_header.f90
        ! Technically, order shouldn't matter... but just in case,
        ! include netcdf_realloc_header.f90 FIRST!
        
        ! nc_diag_header subroutines correspond to the global atributes,
        ! set by NF90_PUT_ATT()
        
        ! This file provides the actual subroutines, referred to by the
        ! interface.
        
        ! nc_diag_header - input integer(i_byte)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_header_resize_byte(addl_num_entries, elevector)
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
            
            if (allocated(diag_header_store%h_byte)) then
                diag_header_store%acount(sc_index) = diag_header_store%acount(sc_index) + addl_num_entries
                if (diag_header_store%acount(sc_index) < diag_header_store%asize(sc_index)) then
                    call nc_diag_realloc(diag_header_store%h_byte, addl_num_entries)
                    diag_header_store%asize(sc_index) = diag_header_store%acount(sc_index)
                end if
            else
                diag_header_store%acount(sc_index) = addl_num_entries
                allocate(diag_header_store%h_byte(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_header_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
            
            if (elevector) then
                ! Element 6 is the last "single" element.
                ! Element 7 and beyond stores the vector size/count info.
                ! 
                ! The range is from element 7 to element 11.
                ! (7, 8, 9, 10, 11)
                ! 
                ! Note that here we're just storing the index info - so we're only
                ! ever going to increase the size by 1.
                
                if (allocated(diag_header_store%h_byte_vi)) then
                    diag_header_store%acount(sc_index_vi) = diag_header_store%acount(sc_index_vi) + 1
                    if (diag_header_store%acount(sc_index_vi) < diag_header_store%asize(sc_index_vi)) then
                        call nc_diag_realloc(diag_header_store%h_byte_vi, 1)
                        diag_header_store%asize(sc_index_vi) = diag_header_store%acount(sc_index_vi)
                    end if
                else
                    diag_header_store%acount(sc_index_vi) = 1
                    allocate(diag_header_store%h_byte_vi(NLAYER_DEFAULT_ENT))
                    diag_header_store%asize(sc_index_vi) = NLAYER_DEFAULT_ENT
                end if
            end if
            
            
            
        end subroutine nc_diag_header_resize_byte
        
        ! nc_diag_header - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_header_resize_short(addl_num_entries, elevector)
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
            
            ! NLAYER_SHORT is located at the first index, 2.
            ! sc_index_vi is just sc_index + 6, 6 being the number of single types
            sc_index = 2
            sc_index_vi = sc_index + 6
            
            if (allocated(diag_header_store%h_short)) then
                diag_header_store%acount(sc_index) = diag_header_store%acount(sc_index) + addl_num_entries
                if (diag_header_store%acount(sc_index) < diag_header_store%asize(sc_index)) then
                    call nc_diag_realloc(diag_header_store%h_short, addl_num_entries)
                    diag_header_store%asize(sc_index) = diag_header_store%acount(sc_index)
                end if
            else
                diag_header_store%acount(sc_index) = addl_num_entries
                allocate(diag_header_store%h_short(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_header_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
            
            if (elevector) then
                ! Element 6 is the last "single" element.
                ! Element 7 and beyond stores the vector size/count info.
                ! 
                ! The range is from element 7 to element 11.
                ! (7, 8, 9, 10, 11)
                ! 
                ! Note that here we're just storing the index info - so we're only
                ! ever going to increase the size by 1.
                
                if (allocated(diag_header_store%h_short_vi)) then
                    diag_header_store%acount(sc_index_vi) = diag_header_store%acount(sc_index_vi) + 1
                    if (diag_header_store%acount(sc_index_vi) < diag_header_store%asize(sc_index_vi)) then
                        call nc_diag_realloc(diag_header_store%h_short_vi, 1)
                        diag_header_store%asize(sc_index_vi) = diag_header_store%acount(sc_index_vi)
                    end if
                else
                    diag_header_store%acount(sc_index_vi) = 1
                    allocate(diag_header_store%h_short_vi(NLAYER_DEFAULT_ENT))
                    diag_header_store%asize(sc_index_vi) = NLAYER_DEFAULT_ENT
                end if
            end if
        end subroutine nc_diag_header_resize_short
        
        ! nc_diag_header - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_header_resize_long(addl_num_entries, elevector)
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
            sc_index = 3
            sc_index_vi = sc_index + 6
            
            if (allocated(diag_header_store%h_long)) then
                diag_header_store%acount(sc_index) = diag_header_store%acount(sc_index) + addl_num_entries
                call debug("(A, I, A, I)") "acount/asize: ", diag_header_store%acount(sc_index), "/", diag_header_store%asize(sc_index)
                if (diag_header_store%acount(sc_index) < diag_header_store%asize(sc_index)) then
                    call nc_diag_realloc(diag_header_store%h_long, addl_num_entries)
                    diag_header_store%asize(sc_index) = diag_header_store%acount(sc_index)
                end if
            else
                diag_header_store%acount(sc_index) = addl_num_entries
                allocate(diag_header_store%h_long(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_header_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
            
            if (elevector) then
                ! Element 6 is the last "single" element.
                ! Element 7 and beyond stores the vector size/count info.
                ! 
                ! The range is from element 7 to element 11.
                ! (7, 8, 9, 10, 11)
                ! 
                ! Note that here we're just storing the index info - so we're only
                ! ever going to increase the size by 1.
                
                if (allocated(diag_header_store%h_long_vi)) then
                    diag_header_store%acount(sc_index_vi) = diag_header_store%acount(sc_index_vi) + 1
                    if (diag_header_store%acount(sc_index_vi) < diag_header_store%asize(sc_index_vi)) then
                        call nc_diag_realloc(diag_header_store%h_long_vi, 1)
                        diag_header_store%asize(sc_index_vi) = diag_header_store%acount(sc_index_vi)
                    end if
                else
                    diag_header_store%acount(sc_index_vi) = 1
                    allocate(diag_header_store%h_long_vi(NLAYER_DEFAULT_ENT))
                    diag_header_store%asize(sc_index_vi) = NLAYER_DEFAULT_ENT
                end if
            end if
        end subroutine nc_diag_header_resize_long
        
        ! nc_diag_header - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_header_resize_rsingle(addl_num_entries, elevector)
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
            sc_index = 4
            sc_index_vi = sc_index + 6
            
            if (allocated(diag_header_store%h_rsingle)) then
                diag_header_store%acount(sc_index) = diag_header_store%acount(sc_index) + addl_num_entries
                if (diag_header_store%acount(sc_index) < diag_header_store%asize(sc_index)) then
                    call nc_diag_realloc(diag_header_store%h_rsingle, addl_num_entries)
                    diag_header_store%asize(sc_index) = diag_header_store%acount(sc_index)
                end if
            else
                diag_header_store%acount(sc_index) = addl_num_entries
                allocate(diag_header_store%h_rsingle(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_header_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
            
            if (elevector) then
                ! Element 6 is the last "single" element.
                ! Element 7 and beyond stores the vector size/count info.
                ! 
                ! The range is from element 7 to element 11.
                ! (7, 8, 9, 10, 11)
                ! 
                ! Note that here we're just storing the index info - so we're only
                ! ever going to increase the size by 1.
                
                if (allocated(diag_header_store%h_rsingle_vi)) then
                    diag_header_store%acount(sc_index_vi) = diag_header_store%acount(sc_index_vi) + 1
                    if (diag_header_store%acount(sc_index_vi) < diag_header_store%asize(sc_index_vi)) then
                        call nc_diag_realloc(diag_header_store%h_rsingle_vi, 1)
                        diag_header_store%asize(sc_index_vi) = diag_header_store%acount(sc_index_vi)
                    end if
                else
                    diag_header_store%acount(sc_index_vi) = 1
                    allocate(diag_header_store%h_rsingle_vi(NLAYER_DEFAULT_ENT))
                    diag_header_store%asize(sc_index_vi) = NLAYER_DEFAULT_ENT
                end if
            end if
        end subroutine nc_diag_header_resize_rsingle
        
        ! nc_diag_header - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_header_resize_rdouble(addl_num_entries, elevector)
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
            sc_index = 5
            sc_index_vi = sc_index + 6
            
            if (allocated(diag_header_store%h_rdouble)) then
                diag_header_store%acount(sc_index) = diag_header_store%acount(sc_index) + addl_num_entries
                if (diag_header_store%acount(sc_index) < diag_header_store%asize(sc_index)) then
                    call nc_diag_realloc(diag_header_store%h_rdouble, addl_num_entries)
                    diag_header_store%asize(sc_index) = diag_header_store%acount(sc_index)
                end if
            else
                diag_header_store%acount(sc_index) = addl_num_entries
                allocate(diag_header_store%h_rdouble(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_header_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
            
            if (elevector) then
                ! Element 6 is the last "single" element.
                ! Element 7 and beyond stores the vector size/count info.
                ! 
                ! The range is from element 7 to element 11.
                ! (7, 8, 9, 10, 11)
                ! 
                ! Note that here we're just storing the index info - so we're only
                ! ever going to increase the size by 1.
                
                if (allocated(diag_header_store%h_rdouble_vi)) then
                    diag_header_store%acount(sc_index_vi) = diag_header_store%acount(sc_index_vi) + 1
                    if (diag_header_store%acount(sc_index_vi) < diag_header_store%asize(sc_index_vi)) then
                        call nc_diag_realloc(diag_header_store%h_rdouble_vi, 1)
                        diag_header_store%asize(sc_index_vi) = diag_header_store%acount(sc_index_vi)
                    end if
                else
                    diag_header_store%acount(sc_index_vi) = 1
                    allocate(diag_header_store%h_rdouble_vi(NLAYER_DEFAULT_ENT))
                    diag_header_store%asize(sc_index_vi) = NLAYER_DEFAULT_ENT
                end if
            end if
        end subroutine nc_diag_header_resize_rdouble

        ! nc_diag_header - input character(len=*)
        ! Corresponding NetCDF4 type: string? char?
        subroutine nc_diag_header_resize_string(addl_num_entries, elevector)
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
            sc_index = 6
            
            if (allocated(diag_header_store%h_string)) then
                diag_header_store%acount(sc_index) = diag_header_store%acount(sc_index) + addl_num_entries
                if (diag_header_store%acount(sc_index) < diag_header_store%asize(sc_index)) then
                    call nc_diag_realloc(diag_header_store%h_string, addl_num_entries)
                    diag_header_store%asize(sc_index) = diag_header_store%acount(sc_index)
                end if
            else
                diag_header_store%acount(sc_index) = addl_num_entries
                allocate(diag_header_store%h_string(addl_num_entries + NLAYER_DEFAULT_ENT))
                diag_header_store%asize(sc_index) = addl_num_entries + NLAYER_DEFAULT_ENT
            end if
            
            ! No vector stuff for strings here, so we're done.
        end subroutine nc_diag_header_resize_string
        
