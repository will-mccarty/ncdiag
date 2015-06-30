        function nf90_put_var_text(ncid, varid, values, start, count, stride, map)
            integer,                         intent( in) :: ncid, varid
            character (len = *),             intent( in) :: values
            integer, dimension(:), optional, intent( in) :: start, count, stride, map
            integer                                      :: nf90_put_var_text
            
            nf90_put_var_text = 0
        end function nf90_put_var_text
        
        function nf90_put_var_1D_EightByteInt(ncid, varid, values, start, count, stride, map)
            integer,                         intent( in) :: ncid, varid
            integer (kind = i_llong), dimension(:), intent( in) :: values
            integer, dimension(:), optional, intent( in) :: start, count, stride, map
            integer                                      :: nf90_put_var_1D_EightByteInt
            
            nf90_put_var_1D_EightByteInt = 0
        end function nf90_put_var_1D_EightByteInt
        
        function nf90_put_var_2D_EightByteInt(ncid, varid, values, start, count, stride, map)
            integer,                         intent( in) :: ncid, varid
            integer (kind = i_llong), dimension(:, :), &
                intent( in) :: values
            integer, dimension(:), optional, intent( in) :: start, count, stride, map
            integer                                      :: nf90_put_var_2D_EightByteInt
            
            nf90_put_var_2D_EightByteInt = 0
        end function nf90_put_var_2D_EightByteInt
        
        
        function nf90_put_var_3D_EightByteInt(ncid, varid, values, start, count, stride, map)
            integer,                         intent( in) :: ncid, varid
            integer (kind = i_llong), dimension(:, :, :), &
                intent( in) :: values
            integer, dimension(:), optional, intent( in) :: start, count, stride, map
            integer                                      :: nf90_put_var_3D_EightByteInt
            
            nf90_put_var_3D_EightByteInt = 0
        end function nf90_put_var_3D_EightByteInt
        
        function nf90_put_var_4D_EightByteInt(ncid, varid, values, start, count, stride, map)
            integer,                         intent( in) :: ncid, varid
            integer (kind = i_llong), dimension(:, :, :, :), &
                intent( in) :: values
            integer, dimension(:), optional, intent( in) :: start, count, stride, map
            integer                                      :: nf90_put_var_4D_EightByteInt
            
            nf90_put_var_4D_EightByteInt = 0
        end function nf90_put_var_4D_EightByteInt
        
        function nf90_put_var_5D_EightByteInt(ncid, varid, values, start, count, stride, map)
            integer,                         intent( in) :: ncid, varid
            integer (kind = i_llong), dimension(:, :, :, :, :), &
                intent( in) :: values
            integer, dimension(:), optional, intent( in) :: start, count, stride, map
            integer                                      :: nf90_put_var_5D_EightByteInt
            
            nf90_put_var_5D_EightByteInt = 0
        end function nf90_put_var_5D_EightByteInt
        
        function nf90_put_var_6D_EightByteInt(ncid, varid, values, start, count, stride, map)
            integer,                         intent( in) :: ncid, varid
            integer (kind = i_llong), dimension(:, :, :, :, :, :), &
                intent( in) :: values
            integer, dimension(:), optional, intent( in) :: start, count, stride, map
            integer                                      :: nf90_put_var_6D_EightByteInt
            
            nf90_put_var_6D_EightByteInt = 0
        end function nf90_put_var_6D_EightByteInt
        
        
        function nf90_put_var_7D_EightByteInt(ncid, varid, values, start, count, stride, map)
            integer,                         intent( in) :: ncid, varid
            integer (kind = i_llong), dimension(:, :, :, :, :, :, :), &
                intent( in) :: values
            integer, dimension(:), optional, intent( in) :: start, count, stride, map
            integer                                      :: nf90_put_var_7D_EightByteInt
            
            nf90_put_var_7D_EightByteInt = 0
        end function nf90_put_var_7D_EightByteInt
        
        function nf90_put_var_OneByteInt(ncid, varid, values, start)
            integer,                         intent( in) :: ncid, varid
            integer (kind = i_byte), intent( in) :: values
            integer, dimension(:), optional, intent( in) :: start
            integer                                      :: nf90_put_var_OneByteInt

            nf90_put_var_OneByteInt = 0
        end function nf90_put_var_OneByteInt


        function nf90_put_var_TwoByteInt(ncid, varid, values, start)
            integer,                         intent( in) :: ncid, varid
            integer (kind = i_short), intent( in) :: values
            integer, dimension(:), optional, intent( in) :: start
            integer                                      :: nf90_put_var_TwoByteInt

            nf90_put_var_TwoByteInt = 0
        end function nf90_put_var_TwoByteInt


        function nf90_put_var_FourByteInt(ncid, varid, values, start)
            integer,                         intent( in) :: ncid, varid
            integer (kind = i_long), intent( in) :: values
            integer, dimension(:), optional, intent( in) :: start
            integer                                      :: nf90_put_var_FourByteInt

            nf90_put_var_FourByteInt = 0
        end function nf90_put_var_FourByteInt


        function nf90_put_var_FourByteReal(ncid, varid, values, start)
            integer,                         intent( in) :: ncid, varid
            real (kind = r_single), intent( in) :: values
            integer, dimension(:), optional, intent( in) :: start
            integer                                      :: nf90_put_var_FourByteReal

            nf90_put_var_FourByteReal = 0
        end function nf90_put_var_FourByteReal


        function nf90_put_var_EightByteReal(ncid, varid, values, start)
            integer,                         intent( in) :: ncid, varid
            real (kind = r_double), intent( in) :: values
            integer, dimension(:), optional, intent( in) :: start
            integer                                      :: nf90_put_var_EightByteReal

            nf90_put_var_EightByteReal = 0
        end function nf90_put_var_EightByteReal


        function nf90_get_var_OneByteInt(ncid, varid, values, start)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_byte), intent(out) :: values
         integer, dimension(:), optional, intent( in) :: start
         integer                                      :: nf90_get_var_OneByteInt

         nf90_get_var_OneByteInt = 0
        end function nf90_get_var_OneByteInt


        function nf90_get_var_TwoByteInt(ncid, varid, values, start)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_short), intent(out) :: values
         integer, dimension(:), optional, intent( in) :: start
         integer                                      :: nf90_get_var_TwoByteInt

         nf90_get_var_TwoByteInt = 0
        end function nf90_get_var_TwoByteInt


        function nf90_get_var_FourByteInt(ncid, varid, values, start)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_long), intent(out) :: values
         integer, dimension(:), optional, intent( in) :: start
         integer                                      :: nf90_get_var_FourByteInt

         nf90_get_var_FourByteInt = 0
        end function nf90_get_var_FourByteInt


        function nf90_get_var_FourByteReal(ncid, varid, values, start)
         integer,                         intent( in) :: ncid, varid
         real (kind = r_single), intent(out) :: values
         integer, dimension(:), optional, intent( in) :: start
         integer                                      :: nf90_get_var_FourByteReal
         
         nf90_get_var_FourByteReal = 0
        end function nf90_get_var_FourByteReal


        function nf90_get_var_EightByteReal(ncid, varid, values, start)
         integer,                         intent( in) :: ncid, varid
         real (kind = r_double), intent(out) :: values
         integer, dimension(:), optional, intent( in) :: start
         integer                                      :: nf90_get_var_EightByteReal

         nf90_get_var_EightByteReal = 0
        end function nf90_get_var_EightByteReal

        function nf90_put_var_EightByteInt(ncid, varid, values, start)
          integer,                         intent( in) :: ncid, varid
          integer (kind = i_llong), intent( in) :: values
          integer, dimension(:), optional, intent( in) :: start
          integer                                      :: nf90_put_var_EightByteInt

          nf90_put_var_EightByteInt = 0
        end function nf90_put_var_EightByteInt

        function nf90_put_var_1D_OneByteInt(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_byte), dimension(:), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_1D_OneByteInt
         
         nf90_put_var_1D_OneByteInt = 0
        end function nf90_put_var_1D_OneByteInt


        function nf90_put_var_2D_OneByteInt(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_byte), dimension(:, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_2D_OneByteInt

         nf90_put_var_2D_OneByteInt = 0
        end function nf90_put_var_2D_OneByteInt


        function nf90_put_var_3D_OneByteInt(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_byte), dimension(:, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_3D_OneByteInt

         nf90_put_var_3D_OneByteInt = 0
        end function nf90_put_var_3D_OneByteInt


        function nf90_put_var_4D_OneByteInt(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_byte), dimension(:, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_4D_OneByteInt

         nf90_put_var_4D_OneByteInt = 0
        end function nf90_put_var_4D_OneByteInt


        function nf90_put_var_5D_OneByteInt(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_byte), dimension(:, :, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_5D_OneByteInt

         nf90_put_var_5D_OneByteInt = 0
        end function nf90_put_var_5D_OneByteInt


        function nf90_put_var_6D_OneByteInt(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_byte), dimension(:, :, :, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_6D_OneByteInt
         
         nf90_put_var_6D_OneByteInt = 0
        end function nf90_put_var_6D_OneByteInt


        function nf90_put_var_7D_OneByteInt(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_byte), dimension(:, :, :, :, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_7D_OneByteInt

         nf90_put_var_7D_OneByteInt = 0
        end function nf90_put_var_7D_OneByteInt


        function nf90_put_var_1D_TwoByteInt(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_short), dimension(:), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_1D_TwoByteInt

         nf90_put_var_1D_TwoByteInt = 0
        end function nf90_put_var_1D_TwoByteInt


        function nf90_put_var_2D_TwoByteInt(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_short), dimension(:, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_2D_TwoByteInt
         
         nf90_put_var_2D_TwoByteInt = 0
        end function nf90_put_var_2D_TwoByteInt


        function nf90_put_var_3D_TwoByteInt(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_short), dimension(:, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_3D_TwoByteInt

         nf90_put_var_3D_TwoByteInt = 0
        end function nf90_put_var_3D_TwoByteInt


        function nf90_put_var_4D_TwoByteInt(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_short), dimension(:, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_4D_TwoByteInt

         nf90_put_var_4D_TwoByteInt = 0
        end function nf90_put_var_4D_TwoByteInt


        function nf90_put_var_5D_TwoByteInt(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_short), dimension(:, :, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_5D_TwoByteInt

         nf90_put_var_5D_TwoByteInt = 0
        end function nf90_put_var_5D_TwoByteInt


        function nf90_put_var_6D_TwoByteInt(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_short), dimension(:, :, :, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_6D_TwoByteInt

         nf90_put_var_6D_TwoByteInt = 0
        end function nf90_put_var_6D_TwoByteInt


        function nf90_put_var_7D_TwoByteInt(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_short), dimension(:, :, :, :, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_7D_TwoByteInt

         nf90_put_var_7D_TwoByteInt = 0
        end function nf90_put_var_7D_TwoByteInt


        function nf90_put_var_1D_FourByteInt(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_long), dimension(:), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_1D_FourByteInt

         nf90_put_var_1D_FourByteInt = 0
        end function nf90_put_var_1D_FourByteInt


        function nf90_put_var_2D_FourByteInt(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_long), dimension(:, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_2D_FourByteInt

         nf90_put_var_2D_FourByteInt = 0

        end function nf90_put_var_2D_FourByteInt


        function nf90_put_var_3D_FourByteInt(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_long), dimension(:, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_3D_FourByteInt

         nf90_put_var_3D_FourByteInt = 0
        end function nf90_put_var_3D_FourByteInt


        function nf90_put_var_4D_FourByteInt(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_long), dimension(:, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_4D_FourByteInt
         
         nf90_put_var_4D_FourByteInt = 0
        end function nf90_put_var_4D_FourByteInt


        function nf90_put_var_5D_FourByteInt(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_long), dimension(:, :, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_5D_FourByteInt

         nf90_put_var_5D_FourByteInt = 0
        end function nf90_put_var_5D_FourByteInt


        function nf90_put_var_6D_FourByteInt(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_long), dimension(:, :, :, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_6D_FourByteInt
         
         nf90_put_var_6D_FourByteInt = 0
        end function nf90_put_var_6D_FourByteInt


        function nf90_put_var_7D_FourByteInt(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         integer (kind = i_long), dimension(:, :, :, :, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_7D_FourByteInt

         nf90_put_var_7D_FourByteInt = 0
        end function nf90_put_var_7D_FourByteInt


        function nf90_put_var_1D_FourByteReal(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         real (kind = r_single), dimension(:), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_1D_FourByteReal

         nf90_put_var_1D_FourByteReal = 0
        end function nf90_put_var_1D_FourByteReal


        function nf90_put_var_2D_FourByteReal(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         real (kind = r_single), dimension(:, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_2D_FourByteReal

         nf90_put_var_2D_FourByteReal = 0
        end function nf90_put_var_2D_FourByteReal


        function nf90_put_var_3D_FourByteReal(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         real (kind = r_single), dimension(:, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_3D_FourByteReal

         nf90_put_var_3D_FourByteReal = 0
        end function nf90_put_var_3D_FourByteReal


        function nf90_put_var_4D_FourByteReal(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         real (kind = r_single), dimension(:, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_4D_FourByteReal

         nf90_put_var_4D_FourByteReal = 0
        end function nf90_put_var_4D_FourByteReal


        function nf90_put_var_5D_FourByteReal(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         real (kind = r_single), dimension(:, :, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_5D_FourByteReal

         nf90_put_var_5D_FourByteReal = 0
        end function nf90_put_var_5D_FourByteReal


        function nf90_put_var_6D_FourByteReal(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         real (kind = r_single), dimension(:, :, :, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_6D_FourByteReal

         nf90_put_var_6D_FourByteReal = 0
        end function nf90_put_var_6D_FourByteReal


        function nf90_put_var_7D_FourByteReal(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         real (kind = r_single), dimension(:, :, :, :, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_7D_FourByteReal

         nf90_put_var_7D_FourByteReal = 0
        end function nf90_put_var_7D_FourByteReal


        function nf90_put_var_1D_EightByteReal(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         real (kind = r_double), dimension(:), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_1D_EightByteReal

         nf90_put_var_1D_EightByteReal = 0
        end function nf90_put_var_1D_EightByteReal


        function nf90_put_var_2D_EightByteReal(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         real (kind = r_double), dimension(:, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_2D_EightByteReal

         nf90_put_var_2D_EightByteReal = 0
        end function nf90_put_var_2D_EightByteReal


        function nf90_put_var_3D_EightByteReal(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         real (kind = r_double), dimension(:, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_3D_EightByteReal

         nf90_put_var_3D_EightByteReal = 0
        end function nf90_put_var_3D_EightByteReal


        function nf90_put_var_4D_EightByteReal(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         real (kind = r_double), dimension(:, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_4D_EightByteReal

         nf90_put_var_4D_EightByteReal = 0
        end function nf90_put_var_4D_EightByteReal


        function nf90_put_var_5D_EightByteReal(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         real (kind = r_double), dimension(:, :, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_5D_EightByteReal

         nf90_put_var_5D_EightByteReal = 0
        end function nf90_put_var_5D_EightByteReal


        function nf90_put_var_6D_EightByteReal(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         real (kind = r_double), dimension(:, :, :, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_6D_EightByteReal

         nf90_put_var_6D_EightByteReal = 0
        end function nf90_put_var_6D_EightByteReal


        function nf90_put_var_7D_EightByteReal(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         real (kind = r_double), dimension(:, :, :, :, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_7D_EightByteReal

         nf90_put_var_7D_EightByteReal = 0
        end function nf90_put_var_7D_EightByteReal

       function nf90_put_var_1D_text(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         character (len = *), dimension(:), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_1D_text
         
         nf90_put_var_1D_text = 0
       end function nf90_put_var_1D_text


       function nf90_put_var_2D_text(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         character (len = *), dimension(:, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_2D_text
         
         nf90_put_var_2D_text = 0
       end function nf90_put_var_2D_text


       function nf90_put_var_3D_text(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         character (len = *), dimension(:, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_3D_text
         
         nf90_put_var_3D_text = 0
       end function nf90_put_var_3D_text


       function nf90_put_var_4D_text(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         character (len = *), dimension(:, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_4D_text
         
         nf90_put_var_4D_text = 0
       end function nf90_put_var_4D_text


       function nf90_put_var_5D_text(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         character (len = *), dimension(:, :, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_5D_text
         
         nf90_put_var_5D_text = 0
       end function nf90_put_var_5D_text


       function nf90_put_var_6D_text(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         character (len = *), dimension(:, :, :, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_6D_text
         
         nf90_put_var_6D_text = 0
       end function nf90_put_var_6D_text


       function nf90_put_var_7D_text(ncid, varid, values, start, count, stride, map)
         integer,                         intent( in) :: ncid, varid
         character (len = *), dimension(:, :, :, :, :, :, :), &
                                          intent( in) :: values
         integer, dimension(:), optional, intent( in) :: start, count, stride, map
         integer                                      :: nf90_put_var_7D_text
         
         nf90_put_var_7D_text = 0
       end function nf90_put_var_7D_text
