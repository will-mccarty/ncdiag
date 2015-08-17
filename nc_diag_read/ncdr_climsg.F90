module ncdr_climsg
    implicit none
    
    ! NetCDF Diag Reader - CLI Message portion
    ! (Declarations)
    logical :: ncdr_enable_info = .FALSE.
    logical :: ncdr_enable_action = .FALSE.
    
    contains
        ! NetCDF Diag Reader - CLI Message portion
        ! (Subroutine/Function implementation)
        
        subroutine ncdr_error(err)
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
            write(*, "(A)") "    Couldn't trigger traceback, ending gracefully."
            write(*, "(A)") "    (Ensure floating point exceptions are enabled,"
            write(*, "(A)") "    and that you have debugging (-g) and tracebacks"
            write(*, "(A)") "    compiler flags enabled!)"
            stop 1
#else
            write (*, "(A)") " ** Failed to read NetCDF4."
            stop 1
#endif
        end subroutine ncdr_error
        
        subroutine ncdr_warning(warn)
            character(len=*), intent(in) :: warn
#ifdef ANSI_TERM_COLORS
            write(*, "(A)") CHAR(27) // "[33m" // &
                            " ** WARNING: " // warn // &
                            CHAR(27) // "[0m"
#else
            write(*, "(A)") " ** WARNING: " // warn
#endif
        end subroutine ncdr_warning
        
        subroutine ncdr_set_action_display(action_on_off)
            logical :: action_on_off
#ifdef ncdr_enable_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (ncdr_enable_action) then
                write(action_str, "(A, L, A)") "nc_set_action_display(action_on_off = ", action_on_off, ")"
                call actionm(trim(action_str))
            end if
#endif
            ncdr_enable_action = action_on_off
        end subroutine ncdr_set_action_display
        
#ifdef ncdr_enable_ACTION_MSGS
        subroutine ncdr_actionm(act)
            character(len=*), intent(in) :: act
            if (ncdr_enable_action) &
#ifdef ANSI_TERM_COLORS
                write(*, "(A)") CHAR(27) // "[36m" // &
                                " **  ACTION: " // act // &
                                CHAR(27) // "[0m"
#else
                write(*, "(A)") " **  ACTION: " // act
#endif
        end subroutine ncdr_actionm
#endif
        
        subroutine ncdr_set_info_display(info_on_off)
            logical :: info_on_off
#ifdef ncdr_enable_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (ncdr_enable_action) then
                write(action_str, "(A, L, A)") "nc_set_info_display(info_on_off = ", info_on_off, ")"
                call actionm(trim(action_str))
            end if
#endif
            ncdr_enable_info = info_on_off
        end subroutine ncdr_set_info_display
        
        subroutine ncdr_info(ifo)
            character(len=*), intent(in) :: ifo
            if (ncdr_enable_info) &
#ifdef ANSI_TERM_COLORS
                write(*, "(A)") CHAR(27) // "[34m" // &
                                " **    INFO: " // ifo // &
                                CHAR(27) // "[0m"
#else
                write(*, "(A)") " **    INFO: " // ifo
#endif
        end subroutine ncdr_info
        
#ifdef _DEBUG_MEM_
        subroutine ncdr_debug(dbg)
            character(len=*), intent(in) :: dbg
            write(*, "(A, A)") "D: ", dbg
        end subroutine ncdr_debug
#endif

end module ncdr_climsg
