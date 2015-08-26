! nc_diag_write - NetCDF Layer Diag Writing Module
! Copyright 2015 Albert Huang - SSAI/NASA for NASA GSFC GMAO (610.1).
! 
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
! 
!   http://www.apache.org/licenses/LICENSE-2.0
! 
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
! implied. See the License for the specific language governing
! permissions and limitations under the License.
! 
! command line message printing module - ncdw_climsg
!
module ncdw_climsg
    ! Module that provides command line message printing support.
    ! 
    ! This module has all of the subroutines needed to print various
    ! types of command line messages.
    ! 
    ! Message types include:
    !   -> Errors - errors that occur within the application. Errors
    !      will always result in the program exiting (via stop). If
    !      ANSI colors are enabled, this will show up in all red.
    !      
    !   -> Warnings - warnings that occur within the application. This
    !      will show a warning, but allow the program to continue. If
    !      ANSI colors are enabled, this will show up in yellow (or
    !      orange, depending on your terminal colors).
    !      
    !   -> Info - information about the application's progress. These
    !      tend to be verbose, hence the option to toggle them on and
    !      off. By default, they are turned off.
    !      
    !   -> Action - debug information that displays key subroutines and
    !      their arguments at the start of the subroutine. These are 
    !      very verbose, hence the option to toggle them on and off.
    !      
    !      In addition, since these are placed in front of subroutines,
    !      they require a compile time flag to turn on, since they take
    !      processing time.
    !      
    !      By default, due to the high verbosity, they are off.
    !      
    !   -> Debug - debug information about the application in general.
    !      These are extremely verbose, and can only be turned on with
    !      a compile time flag.
    ! 
    
    ! Load our numerical types from kinds - we just need our standard
    ! integer type, i_long
    use kinds, only: i_long
    
    use netcdf, only: nf90_noerr, nf90_strerror
    
    implicit none
    
    ! Whether to enable info message printing or not.
    ! By default, this is set to FALSE.
    logical :: nclayer_enable_info = .FALSE.
    
    ! Whether to enable action message printing or not.
    ! By default, this is set to FALSE.
    ! 
    ! Note that even if this is set to TRUE, action message support
    ! must be enabled at compile time for messages to be printed.
    logical :: nclayer_enable_action = .FALSE.
    
    contains
        ! Display a given error message.
        ! 
        ! ---
        ! 
        ! Args:
        !     err (character(len=*)): the number of entries
        !         to make enough space for.
        !     
        ! Raises:
        !     The following errors will trigger indirectly from other
        !     subroutines called here:
        !     
        !     ---
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
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
end module ncdw_climsg
