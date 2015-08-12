module ncdr_climsg
    ! NetCDF Diag Reader - CLI Message portion
    ! (Declarations)
    logical :: enable_info = .FALSE.
    logical :: enable_action = .FALSE.
    
    contains
        ! NetCDF Diag Reader - CLI Message portion
        ! (Subroutine/Function implementation)
        
        subroutine error(err)
            character(len=*), intent(in) :: err
#ifdef ERROR_TRACEBACK
            integer                      :: div0
#endif
#ifdef ANSI_TERM_COLORS
            write(*, "(A)") CHAR(27) // "[31m" // &
                            " **   ERROR: " // err // &
                            CHAR(27) // "[0m"
#else
            write(*, "(A)") " **   ERROR: " // err
#endif
#ifdef ERROR_TRACEBACK
            write(*, "(A)") " ** Failed to read NetCDF4."
            write(*, "(A)") "    (Traceback requested, triggering div0 error...)"
            div0 = 1 / 0
#else
            write (*, "(A)") " ** Failed to read NetCDF4."
            stop 1
#endif
        end subroutine error
        
        subroutine warning(warn)
            character(len=*), intent(in) :: warn
#ifdef ANSI_TERM_COLORS
            write(*, "(A)") CHAR(27) // "[33m" // &
                            " ** WARNING: " // warn // &
                            CHAR(27) // "[0m"
#else
            write(*, "(A)") " ** WARNING: " // warn
#endif
        end subroutine warning
        
        subroutine set_action_display(action_on_off)
            logical :: action_on_off
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (enable_action) then
                write(action_str, "(A, L, A)") "nc_set_action_display(action_on_off = ", action_on_off, ")"
                call actionm(trim(action_str))
            end if
#endif
            enable_action = action_on_off
        end subroutine set_action_display
        
#ifdef ENABLE_ACTION_MSGS
        subroutine actionm(act)
            character(len=*), intent(in) :: act
            if (enable_action) &
#ifdef ANSI_TERM_COLORS
                write(*, "(A)") CHAR(27) // "[36m" // &
                                " **  ACTION: " // act // &
                                CHAR(27) // "[0m"
#else
                write(*, "(A)") " **  ACTION: " // act
#endif
        end subroutine actionm
#endif
        
        subroutine set_info_display(info_on_off)
            logical :: info_on_off
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (enable_action) then
                write(action_str, "(A, L, A)") "nc_set_info_display(info_on_off = ", info_on_off, ")"
                call actionm(trim(action_str))
            end if
#endif
            enable_info = info_on_off
        end subroutine set_info_display
        
        subroutine info(ifo)
            character(len=*), intent(in) :: ifo
            if (enable_info) &
#ifdef ANSI_TERM_COLORS
                write(*, "(A)") CHAR(27) // "[34m" // &
                                " **    INFO: " // ifo // &
                                CHAR(27) // "[0m"
#else
                write(*, "(A)") " **    INFO: " // ifo
#endif
        end subroutine info
        
#ifdef _DEBUG_MEM_
        subroutine debug(dbg)
            character(len=*), intent(in) :: dbg
            write(*, "(A, A)") "D: ", dbg
        end subroutine debug
#endif

end module ncdr_climsg
