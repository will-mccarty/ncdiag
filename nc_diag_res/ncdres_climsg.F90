module ncdres_climsg
    ! NetCDF Diag Reader - CLI Message portion
    ! (Declarations)
    logical :: ncdres_enable_info = .FALSE.
    logical :: ncdres_enable_action = .FALSE.
    
    contains
        ! NetCDF Diag Reader - CLI Message portion
        ! (Subroutine/Function implementation)
        
        subroutine ncdres_error(err)
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
        end subroutine ncdres_error
        
        subroutine ncdres_warning(warn)
            character(len=*), intent(in) :: warn
#ifdef ANSI_TERM_COLORS
            write(*, "(A)") CHAR(27) // "[33m" // &
                            " ** WARNING: " // warn // &
                            CHAR(27) // "[0m"
#else
            write(*, "(A)") " ** WARNING: " // warn
#endif
        end subroutine ncdres_warning
        
        subroutine ncdres_set_action_display(action_on_off)
            logical :: action_on_off
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (ncdres_enable_action) then
                write(action_str, "(A, L, A)") "nc_set_action_display(action_on_off = ", action_on_off, ")"
                call actionm(trim(action_str))
            end if
#endif
            ncdres_enable_action = action_on_off
        end subroutine ncdres_set_action_display
        
#ifdef ENABLE_ACTION_MSGS
        subroutine ncdres_actionm(act)
            character(len=*), intent(in) :: act
            if (enable_action) &
#ifdef ANSI_TERM_COLORS
                write(*, "(A)") CHAR(27) // "[36m" // &
                                " **  ACTION: " // act // &
                                CHAR(27) // "[0m"
#else
                write(*, "(A)") " **  ACTION: " // act
#endif
        end subroutine ncdres_actionm
#endif
        
        subroutine ncdres_set_info_display(info_on_off)
            logical :: info_on_off
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (enable_action) then
                write(action_str, "(A, L, A)") "nc_set_info_display(info_on_off = ", info_on_off, ")"
                call actionm(trim(action_str))
            end if
#endif
            ncdres_enable_info = info_on_off
        end subroutine ncdres_set_info_display
        
        subroutine ncdres_info(ifo)
            character(len=*), intent(in) :: ifo
            if (ncdres_enable_info) &
#ifdef ANSI_TERM_COLORS
                write(*, "(A)") CHAR(27) // "[34m" // &
                                " **    INFO: " // ifo // &
                                CHAR(27) // "[0m"
#else
                write(*, "(A)") " **    INFO: " // ifo
#endif
        end subroutine ncdres_info
        
#ifdef _DEBUG_MEM_
        subroutine ncdres_debug(dbg)
            character(len=*), intent(in) :: dbg
            write(*, "(A, A)") "D: ", dbg
        end subroutine ncdres_debug
#endif

end module ncdres_climsg
