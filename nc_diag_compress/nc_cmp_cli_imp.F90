        subroutine usage(err)
            character(len=*), intent(in), optional :: err
            
            if (present(err)) then
#ifdef ANSI_TERM_COLORS
                write(*, "(A)") CHAR(27) // "[31m" // &
                            " ** ERROR: " // err // &
                            CHAR(27) // "[0m"
#else
                write(*, "(A)") " ** ERROR: " // err
#endif
            end if
            
            call get_command_argument(0, prgm_name)
            print *, "nc_diag_compress v1.0"
            print *, "NetCDF Diag File Compression Tool"
            print *, "Usage: " // trim(prgm_name) // " -o OUTPUT_FILE INPUT_FILE"
            print *, "    Apply optimized compression on INPUT_FILE, and save into"
            print *, "    OUTPUT_FILE. Note that the amount of compression achieved"
            print *, "    depends on the input file's compression (or lack thereof)."
            print *, "    If the input file is highly compressed, this tool may not"
            print *, "    be able to improve the compression any further. If the"
            print *, "    input file is lightly compressed (or not at all), this tool"
            print *, "    may be able to help."
            stop
        end subroutine usage
        
        subroutine nc_diag_compress_process_args
            cli_arg_count = command_argument_count()
            
            if (cli_arg_count /= 3) then
                call usage
            end if
            
            ! Check for -o.
            ! We enforce this so that people really know what they're putting
            ! into this program!
            call get_command_argument(1, dummy_arg)
            
            if (trim(dummy_arg) /= "-o") then
                call usage("Invalid option - '-o' must be specified in the 1st argument.")
            end if
            
            ! Grab output file argument
            call get_command_argument(2, output_file)
            
            if (len_trim(output_file) <= 0) then
                call usage("Invalid output file name.")
            end if
            
            ! Grab first input file argument
            call get_command_argument(3, input_file)
            
            if (len_trim(input_file) <= 0) then
                call usage("Invalid first input file name.")
            end if
            
            ! Sanity checks done!
        end subroutine nc_diag_compress_process_args
