        subroutine check(status)
          integer, intent ( in) :: status
          
          if(status /= nf90_noerr) then 
            call error(trim(nf90_strerror(status)))
          end if
        end subroutine check
        
        subroutine error(err)
            character(len=*), intent(in) :: err
#ifdef ERROR_TRACEBACK
            integer                      :: div0
#endif
#ifdef USE_MPI
                write(*, "(A, I0, A)") &
#else
                write(*, "(A)") &
#endif
#ifdef ANSI_TERM_COLORS
                            CHAR(27) // "[31m" // &
#endif
#ifdef USE_MPI
                            "[PROC ", cur_proc, "]" // &
#endif
                            " **   ERROR: " // err // &
#ifdef ANSI_TERM_COLORS
                            CHAR(27) // "[0m"
#endif
#ifdef ERROR_TRACEBACK
            write(*, "(A)") " ** Failed to process data/write NetCDF4."
            write(*, "(A)") "    (Traceback requested, triggering div0 error...)"
            div0 = 1 / 0
#else
            stop " ** Failed to process data/write NetCDF4."
#endif
        end subroutine error
        
        subroutine warning(warn)
            character(len=*), intent(in) :: warn
#ifdef USE_MPI
            write(*, "(A, I0, A)") &
#else
            write(*, "(A)") &
#endif
#ifdef ANSI_TERM_COLORS
                            CHAR(27) // "[33m" // &
#endif
#ifdef USE_MPI
                            "[PROC ", cur_proc, "]" // &
#endif
                            " ** WARNING: " // warn // &
#ifdef ANSI_TERM_COLORS
                            CHAR(27) // "[0m"
#endif
        end subroutine warning
        
        subroutine info(ifo)
            character(len=*), intent(in) :: ifo
            if (enable_info) &
#ifdef USE_MPI
                write(*, "(A, I0, A)") &
#else
                write(*, "(A)") &
#endif
#ifdef ANSI_TERM_COLORS
                                CHAR(27) // "[34m" // &
#endif
#ifdef USE_MPI
                                "[PROC ", cur_proc, "]" // &
#endif
                                " **    INFO: " // ifo // &
#ifdef ANSI_TERM_COLORS
                                CHAR(27) // "[0m"
#endif
        end subroutine info
        
#ifdef _DEBUG_MEM_
        subroutine debug(dbg)
            character(len=*), intent(in) :: dbg
            write(*, "(A, A)") "D: ", dbg
        end subroutine debug
#endif
    
