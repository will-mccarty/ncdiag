module nclayer_climsg
    use kinds, only: i_long
    use netcdf, only: nf90_noerr, nf90_strerror
    
    implicit none
    
    logical :: nclayer_enable_info = .FALSE.
    logical :: nclayer_enable_action = .FALSE.
    
    contains
        subroutine nclayer_error(err)
            character(len=*), intent(in) :: err
#ifdef ERROR_TRACEBACK
            integer(i_long)              :: div0
#endif
#ifdef ANSI_TERM_COLORS
            write(*, "(A)") CHAR(27) // "[31m" // &
                            " **   ERROR: " // err // &
                            CHAR(27) // "[0m"
#else
            write(*, "(A)") " **   ERROR: " // err
#endif
#ifdef ERROR_TRACEBACK
            write(*, "(A)") " ** Failed to process data/write NetCDF4."
            write(*, "(A)") "    (Traceback requested, triggering div0 error...)"
            div0 = 1 / 0
#else
            stop " ** Failed to process data/write NetCDF4."
#endif
        end subroutine nclayer_error
        
        subroutine nclayer_warning(warn)
            character(len=*), intent(in) :: warn
#ifdef ANSI_TERM_COLORS
            write(*, "(A)") CHAR(27) // "[33m" // &
                            " ** WARNING: " // warn // &
                            CHAR(27) // "[0m"
#else
            write(*, "(A)") " ** WARNING: " // warn
#endif
        end subroutine nclayer_warning
        
        subroutine nc_set_action_display(action_on_off)
            logical :: action_on_off
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, L, A)") "nc_set_action_display(action_on_off = ", action_on_off, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            nclayer_enable_action = action_on_off
        end subroutine nc_set_action_display
        
#ifdef ENABLE_ACTION_MSGS
        subroutine nclayer_actionm(act)
            character(len=*), intent(in) :: act
            if (nclayer_enable_action) &
#ifdef ANSI_TERM_COLORS
                write(*, "(A)") CHAR(27) // "[36m" // &
                                " **  ACTION: " // act // &
                                CHAR(27) // "[0m"
#else
                write(*, "(A)") " **  ACTION: " // act
#endif
        end subroutine nclayer_actionm
#endif
        
        subroutine nc_set_info_display(info_on_off)
            logical :: info_on_off
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, L, A)") "nc_set_info_display(info_on_off = ", info_on_off, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            nclayer_enable_info = info_on_off
        end subroutine nc_set_info_display
        
        subroutine nclayer_info(ifo)
            character(len=*), intent(in) :: ifo
            if (nclayer_enable_info) &
#ifdef ANSI_TERM_COLORS
                write(*, "(A)") CHAR(27) // "[34m" // &
                                " **    INFO: " // ifo // &
                                CHAR(27) // "[0m"
#else
                write(*, "(A)") " **    INFO: " // ifo
#endif
        end subroutine nclayer_info
        
#ifdef _DEBUG_MEM_
        subroutine nclayer_debug(dbg)
            character(len=*), intent(in) :: dbg
            write(*, "(A, A)") "D: ", dbg
        end subroutine nclayer_debug
#endif
        
        subroutine nclayer_check(status)
          integer(i_long), intent(in) :: status
          
          if(status /= nf90_noerr) then 
            call nclayer_error(trim(nf90_strerror(status)))
          end if
        end subroutine nclayer_check
end module nclayer_climsg
